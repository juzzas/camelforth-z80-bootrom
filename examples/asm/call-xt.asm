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
    rst 0x28  ; fetch TOS to BC -- the XT to call

    ld hl, bc
    rst 0x30  ; call the XT

    ld bc, -1
    rst 0x20  ; write true to forth stack
    ret
