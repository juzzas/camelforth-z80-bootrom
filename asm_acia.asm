;==============================================================================
; acia driver. Adapted from feilipu's ACIA driver for the RC2014.
;==============================================================================
;
; The rework to support MS Basic HLOAD and the Z80 instruction tuning are
; copyright (C) 2020 Phillip Stevens
;
; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;
; ACIA 6850 interrupt driven serial I/O to run modified NASCOM Basic 4.7.
; Full input and output buffering with incoming data hardware handshaking.
; Handshake shows full before the buffer is totally filled to allow run-on
; from the sender. Transmit and receive are interrupt driven.
;
; feilipu, August 2020
;
;==============================================================================
;
; INCLUDES SECTION
;

;==============================================================================
;
; DEFINES SECTION
;

DEFC    SER_RX_BUFSIZE  =   $100    ; FIXED Rx buffer size, 256 Bytes, no range checking
DEFC    SER_RX_FULLSIZE =   SER_RX_BUFSIZE - $10
; Fullness of the Rx Buffer, when not_RTS is signalled
DEFC    SER_RX_EMPTYSIZE =  $08     ; Fullness of the Rx Buffer, when RTS is signalled

DEFC    SER_TX_BUFSIZE  =   $40     ; Size of the Tx Buffer, 2^n Bytes, n = 6 here

DEFC Z80_ACIA_BUFFER_BASE = 0x8000

;==============================================================================
;
; Some definitions used with the RC2014 on-board peripherals:
;

; ACIA 68B50 Register Mnemonics

DEFC    SER_CTRL_ADDR   =   $80    ; Address of Control Register (write only)
DEFC    SER_STATUS_ADDR =   $80    ; Address of Status Register (read only)
DEFC    SER_DATA_ADDR   =   $81    ; Address of Data Register

DEFC    SER_RESET       =   $03    ; Master Reset (issue before any other Control word)
DEFC    SER_CLK_DIV_64  =   $02    ; Divide the Clock by 64 (default value)
DEFC    SER_CLK_DIV_16  =   $01    ; Divide the Clock by 16
DEFC    SER_CLK_DIV_01  =   $00    ; Divide the Clock by 1

DEFC    SER_8O1         =   $1C    ; 8 Bits  Odd Parity 1 Stop Bit
DEFC    SER_8E1         =   $18    ; 8 Bits Even Parity 1 Stop Bit
DEFC    SER_8N1         =   $14    ; 8 Bits   No Parity 1 Stop Bit
DEFC    SER_8N2         =   $10    ; 8 Bits   No Parity 2 Stop Bits
DEFC    SER_7O1         =   $0C    ; 7 Bits  Odd Parity 1 Stop Bit
DEFC    SER_7E1         =   $08    ; 7 Bits Even Parity 1 Stop Bit
DEFC    SER_7O2         =   $04    ; 7 Bits  Odd Parity 2 Stop Bits
DEFC    SER_7E2         =   $00    ; 7 Bits Even Parity 2 Stop Bits

DEFC    SER_TDI_BRK     =   $60    ; _RTS low,  Transmitting Interrupt Disabled, BRK on Tx
DEFC    SER_TDI_RTS1    =   $40    ; _RTS high, Transmitting Interrupt Disabled
DEFC    SER_TEI_RTS0    =   $20    ; _RTS low,  Transmitting Interrupt Enabled
DEFC    SER_TDI_RTS0    =   $00    ; _RTS low,  Transmitting Interrupt Disabled

DEFC    SER_TEI_MASK    =   $60    ; Mask for the Tx Interrupt & RTS bits

DEFC    SER_REI         =   $80    ; Receive Interrupt Enabled

DEFC    SER_IRQ         =   $80    ; IRQ (Either Transmitted or Received Byte)
DEFC    SER_PE          =   $40    ; Parity Error (Received Byte)
DEFC    SER_OVRN        =   $20    ; Overrun (Received Byte
DEFC    SER_FE          =   $10    ; Framing Error (Received Byte)
DEFC    SER_CTS         =   $08    ; Clear To Send
DEFC    SER_DCD         =   $04    ; Data Carrier Detect
DEFC    SER_TDRE        =   $02    ; Transmit Data Register Empty
DEFC    SER_RDRF        =   $01    ; Receive Data Register Full

; General TTY

DEFC    CTRLC           =    03H     ; Control "C"
DEFC    CTRLG           =    07H     ; Control "G"
DEFC    BKSP            =    08H     ; Back space
DEFC    LF              =    0AH     ; Line feed
DEFC    CS              =    0CH     ; Clear screen
DEFC    CR              =    0DH     ; Carriage return
DEFC    CTRLO           =    0FH     ; Control "O"
DEFC    CTRLQ           =    11H     ; Control "Q"
DEFC    CTRLR           =    12H     ; Control "R"
DEFC    CTRLS           =    13H     ; Control "S"
DEFC    CTRLU           =    15H     ; Control "U"
DEFC    ESC             =    1BH     ; Escape
DEFC    DEL             =    7FH     ; Delete

;==============================================================================
;
; VARIABLES
;

DEFC    serRxInPtr      =     Z80_ACIA_BUFFER_BASE
DEFC    serRxOutPtr     =     serRxInPtr+2
DEFC    serTxInPtr      =     serRxOutPtr+2
DEFC    serTxOutPtr     =     serTxInPtr+2
DEFC    serRxBufUsed    =     serTxOutPtr+2
DEFC    serTxBufUsed    =     serRxBufUsed+1
DEFC    serControl      =     serTxBufUsed+1

; I/O Tx Buffer must start on 0xnnn0 because we increment low byte to roll-over
DEFC    BUFSTART_TX_IO  =     Z80_ACIA_BUFFER_BASE-(Z80_ACIA_BUFFER_BASE%$100) + $80

; I/O Rx Buffer must start on 0xnn00 because we increment low byte to roll-over
DEFC    BUFSTART_RX_IO  =     Z80_ACIA_BUFFER_BASE-(Z80_ACIA_BUFFER_BASE%$100) + $100

DEFC    serTxBuf        =     BUFSTART_TX_IO
DEFC    serRxBuf        =     BUFSTART_RX_IO

;==============================================================================
; C Section
SECTION code_user


;==============================================================================
;
; CODE SECTION
;

;------------------------------------------------------------------------------
PUBLIC handle_acia_int
handle_acia_int:
        push af
        push hl

        in a,(SER_STATUS_ADDR)      ; get the status of the ACIA
        rrca                        ; check whether a byte has been received, via SER_RDRF
        jr NC,im1_tx_send           ; if not, go check for bytes to transmit

im1_rx_get:
        in a,(SER_DATA_ADDR)        ; Get the received byte from the ACIA
        ld l,a                      ; Move Rx byte to l

        ld a,(serRxBufUsed)         ; Get the number of bytes in the Rx buffer
        cp SER_RX_BUFSIZE-1         ; check whether there is space in the buffer
        jr NC,im1_tx_check          ; buffer full, check if we can send something

        ld a,l                      ; get Rx byte from l
        ld hl,serRxBufUsed
        inc (hl)                    ; atomically increment Rx buffer count
        ld hl,(serRxInPtr)          ; get the pointer to where we poke
        ld (hl),a                   ; write the Rx byte to the serRxInPtr address

        inc l                       ; move the Rx pointer low byte along, 0xFF rollover
        ld (serRxInPtr),hl          ; write where the next byte should be poked

        ld a,(serRxBufUsed)         ; get the current Rx count
        cp SER_RX_FULLSIZE          ; compare the count with the preferred full size
        jr NZ,im1_tx_check          ; leave the RTS low, and check for Rx/Tx possibility

        ld a,(serControl)           ; get the ACIA control echo byte
        and ~SER_TEI_MASK           ; mask out the Tx interrupt bits
        or SER_TDI_RTS1             ; Set RTS high, and disable Tx Interrupt
        ld (serControl),a           ; write the ACIA control echo byte back
        out (SER_CTRL_ADDR),a       ; Set the ACIA CTRL register

im1_tx_check:
        in a,(SER_STATUS_ADDR)      ; get the status of the ACIA
        rrca                        ; check whether a byte has been received, via SER_RDRF
        jr C,im1_rx_get             ; another byte received, go get it

im1_tx_send:
        rrca                        ; check whether a byte can be transmitted, via SER_TDRE
        jr NC,im1_txa_end           ; if not, we're done for now

        ld a,(serTxBufUsed)         ; get the number of bytes in the Tx buffer
        or a                        ; check whether it is zero
        jr Z,im1_tei_clear          ; if the count is zero, then disable the Tx Interrupt

        ld hl,(serTxOutPtr)         ; get the pointer to place where we pop the Tx byte
        ld a,(hl)                   ; get the Tx byte
        out (SER_DATA_ADDR),a       ; output the Tx byte to the ACIA

        inc l                       ; move the Tx pointer, just low byte, along
        ld a,SER_TX_BUFSIZE-1       ; load the buffer size, (n^2)-1
        and l                       ; range check
        or serTxBuf&0xFF            ; locate base
        ld l,a                      ; return the low byte to l
        ld (serTxOutPtr),hl         ; write where the next byte should be popped

        ld hl,serTxBufUsed
        dec (hl)                    ; atomically decrement current Tx count

        jr NZ,im1_txa_end           ; if we've more Tx bytes to send, we're done for now

im1_tei_clear:
        ld a,(serControl)           ; get the ACIA control echo byte
        and ~SER_TEI_RTS0           ; mask out (disable) the Tx Interrupt
        ld (serControl),a           ; write the ACIA control byte back
        out (SER_CTRL_ADDR),a       ; Set the ACIA CTRL register

im1_txa_end:
        pop hl
        pop af

        ret

;------------------------------------------------------------------------------
; SECTION z80_acia_rxa_chk          ; ORG $00F0
PUBLIC acia_pollc
acia_pollc:
       ld a,(serRxBufUsed)
       ret

;------------------------------------------------------------------------------
;SECTION z80_acia_rxa                ; ORG $00F0
PUBLIC acia_getc
acia_getc:
        ld a,(serRxBufUsed)         ; get the number of bytes in the Rx buffer
        or a                        ; see if there are zero bytes available
        jr Z,acia_getc              ; wait, if there are no bytes available

        cp SER_RX_EMPTYSIZE         ; compare the count with the preferred empty size
        jr NZ,rxa_clean_up          ; if the buffer is too full, don't change the RTS

        di                          ; critical section begin
        ld a,(serControl)           ; get the ACIA control echo byte
        and ~SER_TEI_MASK           ; mask out the Tx interrupt bits
        or SER_TDI_RTS0             ; set RTS low.
        ld (serControl),a           ; write the ACIA control echo byte back
        ei                          ; critical section end
        out (SER_CTRL_ADDR),a       ; set the ACIA CTRL register

rxa_clean_up:
        push hl                     ; store HL so we don't clobber it

        ld hl,serRxBufUsed
        di
        dec (hl)                    ; atomically decrement Rx count
        ld hl,(serRxOutPtr)         ; get the pointer to place where we pop the Rx byte
        ei
        ld a,(hl)                   ; get the Rx byte

        inc l                       ; move the Rx pointer low byte along
        ld (serRxOutPtr),hl         ; write where the next byte should be popped

        pop hl                      ; recover HL
        ret                         ; char ready in A

;------------------------------------------------------------------------------
;SECTION z80_acia_txa                ; ORG $0120
PUBLIC acia_putc
acia_putc:
        push hl                     ; store HL so we don't clobber it
        ld l,a                      ; store Tx character

        ld a,(serTxBufUsed)         ; Get the number of bytes in the Tx buffer
        or a                        ; check whether the buffer is empty
        jr NZ,txa_buffer_out        ; buffer not empty, so abandon immediate Tx

        in a,(SER_STATUS_ADDR)      ; get the status of the ACIA
        and SER_TDRE                ; check whether a byte can be transmitted
        jr Z,txa_buffer_out         ; if not, so abandon immediate Tx

        ld a,l                      ; Retrieve Tx character for immediate Tx
        out (SER_DATA_ADDR),a       ; immediately output the Tx byte to the ACIA

        pop hl                      ; recover HL
        ret                         ; and just complete

txa_buffer_out:
        ld a,(serTxBufUsed)         ; Get the number of bytes in the Tx buffer
        cp SER_TX_BUFSIZE-1         ; check whether there is space in the buffer
        jr NC,txa_buffer_out        ; buffer full, so wait till it has space

        ld a,l                      ; Retrieve Tx character

        ld hl,serTxBufUsed
        di
        inc (hl)                    ; atomic increment of Tx count
        ld hl,(serTxInPtr)          ; get the pointer to where we poke
        ei
        ld (hl),a                   ; write the Tx byte to the serTxInPtr

        inc l                       ; move the Tx pointer, just low byte along
        ld a,SER_TX_BUFSIZE-1       ; load the buffer size, (n^2)-1
        and l                       ; range check
        or serTxBuf&0xFF            ; locate base
        ld l,a                      ; return the low byte to l
        ld (serTxInPtr),hl          ; write where the next byte should be poked

        pop hl                      ; recover HL

        ld a,(serControl)           ; get the ACIA control echo byte
        and SER_TEI_RTS0            ; test whether ACIA interrupt is set
        ret NZ                      ; if so then just return

        di                          ; critical section begin
        ld a,(serControl)           ; get the ACIA control echo byte again
        and ~SER_TEI_MASK           ; mask out the Tx interrupt bits
        or SER_TEI_RTS0             ; set RTS low. if the TEI was not set, it will work again
        ld (serControl),a           ; write the ACIA control echo byte back
        out (SER_CTRL_ADDR),a       ; set the ACIA CTRL register
        ei                          ; critical section end
        ret


;------------------------------------------------------------------------------

; assume interrupts are disabled
PUBLIC acia_init
acia_init:
        LD HL,serRxBuf              ; Initialise Rx Buffer
        LD (serRxInPtr),HL
        LD (serRxOutPtr),HL

        LD HL,serTxBuf              ; Initialise Tx Buffer
        LD (serTxInPtr),HL
        LD (serTxOutPtr),HL

        XOR A                       ; 0 the RXA & TXA Buffer Counts
        LD (serRxBufUsed),A
        LD (serTxBufUsed),A

        LD A,SER_RESET              ; Master Reset the ACIA
        OUT (SER_CTRL_ADDR),A

        LD A,SER_REI|SER_TDI_RTS0|SER_8N1|SER_CLK_DIV_64
                                    ; load the default ACIA configuration
                                    ; 8n1 at 115200 baud
                                    ; receive interrupt enabled
                                    ; transmit interrupt disabled

        LD (serControl),A           ; write the ACIA control byte echo
        OUT (SER_CTRL_ADDR),A       ; output to the ACIA control byte

        RET