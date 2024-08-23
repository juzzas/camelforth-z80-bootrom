; Originating from:
;    https://github.com/RC2014Z80/RC2014/blob/master/ROMs/CPM-IDE/mini/cpm22bios.asm
;
; Converted to z88dk z80asm for RC2014 by
; Phillip Stevens @feilipu https://feilipu.me
; March 2018
;

SECTION     code_16k

defc __IO_CF_IDE_DATA = 0x10
defc __IO_CF_IDE_ERROR = 0x11
defc __IO_CF_IDE_FEATURE = 0x11
defc __IO_CF_IDE_SEC_CNT = 0x12
defc __IO_CF_IDE_SECTOR = 0x13
defc __IO_CF_IDE_CYL_LSB = 0x14
defc __IO_CF_IDE_CYL_MSB = 0x15
defc __IO_CF_IDE_HEAD = 0x16
defc __IO_CF_IDE_COMMAND = 0x17
defc __IO_CF_IDE_STATUS = 0x17
defc __IO_CF_IDE_CONTROL = 0x1e
defc __IO_CF_IDE_ALT_STATUS = 0x1e
defc __IO_CF_IDE_LBA0 = 0x13
defc __IO_CF_IDE_LBA1 = 0x14
defc __IO_CF_IDE_LBA2 = 0x15
defc __IO_CF_IDE_LBA3 = 0x16

defc __IDE_CMD_READ = 0x20
defc __IDE_CMD_WRITE = 0x30
defc __IDE_CMD_STANDBY = 0xE0
defc __IDE_CMD_IDLE = 0xE1
defc __IDE_CMD_SLEEP = 0xE6
defc __IDE_CMD_CACHE_FLUSH = 0xE7
defc __IDE_CMD_ID = 0xEC
defc __IDE_CMD_FEATURE = 0xEF

;CF Features
defc __CF_8BIT = 0x01
defc __CF_NOCACHE =	0x82

EXTERN asm_z80_delay_ms

PUBLIC cflash_read_sector
cflash_read_sector:
    rst 0x28    ; read buffer_ptr from TOS
    ld hl, bc

    rst 0x28    ; read LBA-high from TOS
    push bc

    rst 0x28    ; read LBA-low from TOS to BC
    ld de,bc

    pop bc      ; bcde = LBA

    and a
    call ide_read_sector
    jr nc, read_write_error
    jp read_write_ok

PUBLIC cflash_write_sector
cflash_write_sector:
    rst 0x28    ; read buffer_ptr from TOS
    ld hl, bc

    rst 0x28    ; read LBA-high from TOS
    push bc

    rst 0x28    ; read LBA-low from TOS
    ld de,bc

    pop bc      ; bcde = LBA

    and a
    call ide_write_sector
    jr nc, read_write_error

read_write_ok:
    ld bc, 0xffff
    rst 0x20
    ret

read_write_error:
    ld bc, 0x0000
    rst 0x20
    ret


;------------------------------------------------------------------------------
; start of common area driver - Compact Flash IDE functions
;------------------------------------------------------------------------------

PUBLIC cflash_init
cflash_init:
    ; THE RC2014 SD PICO TAKES A FEW SECONDS TO INITIALIZE.  ATEMPTING TO
    ; ACCESS IT DURING THIS TIME WILL FAIL.  THE DATA LINES ALL HAVE
    ; PULL-DOWN RESISTORS, SO WHILE IT IS INITIALIZING, READING ANY
    ; REGISTER WILL CONSISTENTLY RETURN $00.  THE FOLLOWING BIT OF CODE
    ; WILL SCAN THE IDE REGISTER BLOCK.  WHILE ALL REGISTERS REMAIN ZERO,
    ; WE WAIT (UNTIL TIMEOUT).  IN MY TESTING, IT SEEMS VERY UNLIKELY
    ; THAT ANY OTHER DEVICE WILL RETURN $00 FOR ALL REGISTERS.
    ; (code, and comment, borrowed from RomWBW)

    ld  hl,500          ; 5 SECONDS
pico_wait001:
    ld  c,__IO_CF_IDE_DATA
    ld  b,8         ; NUMBER OF REGISTERS TO CHECK
pico_wait002:
    in  a,(c)           ; GET REGISTER VALUE
    or  a           ; SET FLAGS
    jr  nz,pico_waitend       ; IF NOT ZERO, MOVE ON
    inc c           ; NEXT REGISTER
    djnz    pico_wait002     ; CHECK ALL 8 REGS
    push bc
    push de
    push hl
    ld bc,100   ; delay 100ms
    call asm_z80_delay_ms
    pop hl
    pop de
    pop bc
    dec hl
    ld  a,h
    or  l
    jr  nz,pico_wait001      ; LOOP
    jp init_error     ; GIVE UP?


pico_waitend:
    call ide_wait_ready         ;make sure drive is ready to proceed

    ; Set 8-bit mode
    ld a,__CF_8BIT
    out (__IO_CF_IDE_FEATURE),a ; set 8bit mode
    ld a,__IDE_CMD_FEATURE
    out (__IO_CF_IDE_COMMAND),a ; command to enable 8 bit mode

    call ide_wait_ready

    ; Set No cache
    ld a,__CF_NOCACHE
    out (__IO_CF_IDE_FEATURE),a
    ld a,__IDE_CMD_FEATURE
    out (__IO_CF_IDE_COMMAND),a

    call ide_wait_ready

init_ok:
    ld bc, 0xffff
    rst 0x20
    ret

init_error:
    ld bc, 0x0000
    rst 0x20
    ret

; set up the drive LBA registers
; Uses AF, BC, DE
; LBA is contained in BCDE registers

ide_setup_lba:
    ld a,e
    out (__IO_CF_IDE_LBA0),a    ;set LBA0 0:7
    ld a,d
    out (__IO_CF_IDE_LBA1),a    ;set LBA1 8:15
    ld a,c
    out (__IO_CF_IDE_LBA2),a    ;set LBA2 16:23
    ld a,b
    and 00001111b               ;lowest 4 bits LBA address used only
    or  11100000b               ;to enable LBA address master mode
    out (__IO_CF_IDE_LBA3),a    ;set LBA3 24:27 + bits 5:7=111

    ld a,1
    out (__IO_CF_IDE_SEC_CNT),a ;set sector count to 1

    ret

; How to poll (waiting for the drive to be ready to transfer data):
; Read the Regular Status port until bit 7 (BSY, value = 0x80) clears,
; and bit 3 (DRQ, value = 0x08) sets.
; Or until bit 0 (ERR, value = 0x01) or bit 5 (WFT, value = 0x20) sets.
; If neither error bit is set, the device is ready right then.
; Uses AF, DE
; return carry on success

ide_wait_ready:
    in a,(__IO_CF_IDE_STATUS)
    and 00100001b               ;test for ERR or WFT
    ret NZ                      ;return clear carry flag on failure

    in a,(__IO_CF_IDE_STATUS)   ;get status byte again
    and 11000000b               ;mask off BuSY and RDY bits
    xor 01000000b               ;wait for RDY to be set and BuSY to be clear
    jp NZ,ide_wait_ready

    scf                         ;set carry flag on success
    ret

; Wait for the drive to be ready to transfer data.
; Returns the drive's status in A
; Uses AF, DE
; return carry on success

ide_wait_drq:
    in a,(__IO_CF_IDE_STATUS)
    and 00100001b               ;test for ERR or WFT
    ret NZ                      ;return clear carry flag on failure

    in a,(__IO_CF_IDE_STATUS)   ;get status byte again
    and 10001000b               ;mask off BuSY and DRQ bits
    xor 00001000b               ;wait for DRQ to be set and BuSY to be clear
    jp NZ,ide_wait_drq

    scf                         ;set carry flag on success
    ret


;------------------------------------------------------------------------------
; Routines that talk with the IDE drive, these should not be called by
; the main program.

; read a sector
; LBA specified by the 4 bytes in BCDE
; the address of the buffer to fill is in HL
; HL is left incremented by 512 bytes
; uses AF, BC, DE, HL
; return carry on success

ide_read_sector:
    call ide_wait_ready         ;make sure drive is ready
    call ide_setup_lba          ;tell it which sector we want in BCDE

    ld a,__IDE_CMD_READ
    out (__IO_CF_IDE_COMMAND),a ;ask the drive to read it

    call ide_wait_ready         ;make sure drive is ready to proceed
    call ide_wait_drq           ;wait until it's got the data

    ;Read a block of 512 bytes (one sector) from the drive
    ;8 bit data register and store it in memory at (HL++)

;    ld bc,__IO_CF_IDE_DATA&0xFF ;keep iterative count in b, I/O port in c
;    inir
;    inir
    ld c,4
rd4secs:
    ld b,128
rdByte:
    in a,(__IO_CF_IDE_DATA)
    ld (hl),a
    inc hl
    dec b
    jr nz, rdByte
    dec c
    jr nz, rd4secs

    scf                         ;carry = 1 on return = operation ok
    ret

;------------------------------------------------------------------------------
; Routines that talk with the IDE drive, these should not be called by
; the main program.

; write a sector
; specified by the 4 bytes in BCDE
; the address of the origin buffer is in HL
; HL is left incremented by 512 bytes
; uses AF, BC, DE, HL
; return carry on success

ide_write_sector:
    call ide_wait_ready         ;make sure drive is ready
    call ide_setup_lba          ;tell it which sector we want in BCDE

    ld a,__IDE_CMD_WRITE
    out (__IO_CF_IDE_COMMAND),a ;instruct drive to write a sector

    call ide_wait_ready         ;make sure drive is ready to proceed
    call ide_wait_drq           ;wait until it wants the data

    ;Write a block of 512 bytes (one sector) from (HL++) to
    ;the drive 8 bit data register
    ; ld bc,__IO_CF_IDE_DATA&0xFF ;keep iterative count in b, I/O port in c
    ;otir
    ;otir
    ld c,4
wr4secs:
    ld b,128
wrByte:
	ld a,(hl)
    out (__IO_CF_IDE_DATA),a
    inc hL
    dec b
    jr nz, wrByte

    dec c
    jr nz,wr4secs

    jp ide_wait_ready           ;wait until the write is complete
