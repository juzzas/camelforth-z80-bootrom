; MIT License
;
; Copyright (c) 2023 Justin Skists
;
; Permission is hereby granted, free of charge, to any person obtaining a copy
; of this software and associated documentation files (the "Software"), to deal
; in the Software without restriction, including without limitation the rights
; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
; copies of the Software, and to permit persons to whom the Software is
; furnished to do so, subject to the following conditions:
;
; The above copyright notice and this permission notice shall be included in all
; copies or substantial portions of the Software.
;
; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
; SOFTWARE.


; RST locations
; For convenience, because we can't easily change the ROM code interrupt
; routines already present in the RC2014, the ACIA serial Tx and Rx routines
; are reachable by calling RST instructions from your program.

; Tx: RST 08H expects a byte to transmit in the a register.
; Rx: RST 10H returns a received byte in the a register, and will block (loop)
;     until it has a byte to return.
; Rx Check: RST 18H will immediately return the number of bytes in the Rx
;           buffer (0 if buffer empty) in the a register.

; ACIA Interrupt: RST 38H is used by the ACIA 68B50 Serial Device

; TODO: Move All RST xxH targets to be rewritten in a JP table originating at 0x8000 in RAM.
; This will allow the use of debugging tools and reorganising the efficient RST call instructions as needed.


EXTERN handle_acia_int
EXTERN acia_putc
EXTERN acia_getc
EXTERN acia_pollc

EXTERN forth_push
EXTERN forth_pop
EXTERN forth_callxt



SECTION code_user

jp_hl:
	jp	(hl)

PUBLIC _z80_rst_08h
_z80_rst_08h:
    jp acia_putc


PUBLIC _z80_rst_10h
_z80_rst_10h:
    jp acia_getc


PUBLIC _z80_rst_18h
_z80_rst_18h:
    jp acia_pollc

PUBLIC _z80_rst_20h
_z80_rst_20h:
    jp forth_push

PUBLIC _z80_rst_28h
_z80_rst_28h:
    jp forth_pop

PUBLIC _z80_rst_30h
_z80_rst_30h:
    jp forth_callxt


PUBLIC _z80_rst_38h
_z80_rst_38h:
    di
    push hl
    push af

    call handle_acia_int

    ld hl, (intvec_ptr)
    ld a, h
    or l
    call nz, jp_hl

    pop af
    pop hl
    ei
    reti

PUBLIC _z80_nmi
_z80_nmi:
    di
    push hl
    push af

    ld hl, (nmivec_ptr)
    ld a, h
    or l
    call nz, jp_hl

    pop af
    pop hl
    ei
    retn

SECTION data_user

PUBLIC intvec_ptr
intvec_ptr:
        DEFW 0

PUBLIC nmivec_ptr
nmivec_ptr:
        DEFW 0

SECTION code_user
