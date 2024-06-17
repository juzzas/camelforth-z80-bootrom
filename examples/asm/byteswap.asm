; very show demo showing how to use the stack from assembly.
; This demo takes the parameter on the top of the stack and then
; swaps the nibbles around, and finally places it back on the stack
;
; Load .ihx file with HEXLOAD
;
;   HEX
;   1234 F000 CALL .
;
; The result will be 3412  OK


ORG 0xf000

start:
    rst 0x28  ; fetch TOS to BC
    ld a, b   ; swap B with C
    ld b, c
    ld c, a
    rst 0x20  ; push BC onto forth stack
    ret
