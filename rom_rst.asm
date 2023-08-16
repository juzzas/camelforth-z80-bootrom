; RST locations
; For convenience, because we can't easily change the ROM code interrupt routines already present in the RC2014, the ACIA serial Tx and Rx routines are reachable by calling RST instructions from your program.

; Tx: RST 08H expects a byte to transmit in the a register.
; Rx: RST 10H returns a received byte in the a register, and will block (loop) until it has a byte to return.
; Rx Check: RST 18H will immediately return the number of bytes in the Rx buffer (0 if buffer empty) in the a register.

; FCB: RST 20H will execute a Fujinet Command Block (FCB) in the de register. See the Fujinet documentation for details.
; FujiNet Poll: RST 28H will poll the PROCEED line of the FujiNet for a response. The a register will be set to 1 if PROCEED is asserted, or 0 if PROCEED is not asserted.

; ACIA Interrupt: RST 38H is used by the ACIA 68B50 Serial Device

; TODO: Move All RST xxH targets to be rewritten in a JP table originating at 0x8000 in RAM.
; This will allow the use of debugging tools and reorganising the efficient RST call instructions as needed.


EXTERN handle_acia_int
EXTERN acia_putc
EXTERN acia_getc
EXTERN acia_pollc


SECTION code_user


PUBLIC _z80_rst_08h
_z80_rst_08h:
    jp acia_putc


PUBLIC _z80_rst_10h
_z80_rst_10h:
    jp acia_getc


PUBLIC _z80_rst_18h
_z80_rst_18h:
    jp acia_pollc


PUBLIC _z80_rst_38h
_z80_rst_38h:
    di
    call handle_acia_int
    ei
    reti