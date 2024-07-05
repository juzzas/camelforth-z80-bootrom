
; ===============================================================
; Mar 2014
; ===============================================================
;
; void z80_delay_ms(uint ms)
;
; Busy wait exactly the number of milliseconds, which includes the
; time needed for an unconditional call and the ret.
;
; ===============================================================

SECTION code_16k

PUBLIC asm_z80_delay_ms

EXTERN asm_z80_delay_tstate

defc RC2014_CLOCK = 7372800

asm_z80_delay_ms:

   ; enter : bc = milliseconds (0 = 65536)
   ;
   ; uses  : af, bc, de, hl

   ld e,c
   ld d,b

ms_loop:

   dec de
   
   ld a,d
   or e
   jr z, last_ms

   ld hl,+(RC2014_CLOCK / 1000) - 43
   call asm_z80_delay_tstate

   jr ms_loop

last_ms:

   ; we will be exact
   
   ld hl,+(RC2014_CLOCK / 1000) - 54
   jp asm_z80_delay_tstate
