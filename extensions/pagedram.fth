\ CamelForth BootROM to access the RC2014 paged 32k RAM

ONLY FORTH ALSO UTILS DEFINITIONS

1 4 +THRU

PREVIOUS DEFINITIONS









\ Fetch byte from paged RAM   ( addr -- c )
CODE: PRC@
    $F3 C,            \ DI
    $D3 C, $38 C,     \ OUT $38,A
    $0A C,            \ LD A,(BC)
    $D3 C, $38 C,     \ OUT $38,A
    $FB C,            \ EI
    $06 C, $00 C,     \ LD B, 0
    $4F C,            \ LD C, A
    NEXT,
;CODE





\ Store byte from paged RAM   ( c addr -- )
CODE: PRC!
    $C5 C,            \ PUSH BC
    $E1 C,            \ POP HL      ; addr 
    $C1 C,            \ POP BC      ; c 
    $F3 C,            \ DI
    $D3 C, $38 C,     \ OUT $38,A   ; page ram
    $71 C,            \ LD (HL),C
    $D3 C, $38 C,     \ OUT $38,A   ; page rom
    $FB C,            \ EI
    $C1 C,            \ POP BC      ; next TOS
    NEXT,
;CODE



\ Copy block from paged RAM  ( src dest u -- )
CODE: PRMOVE
    $C5 C,            \ PUSH BC
    $D9 C,            \ EXX
    $C1 C,            \ POP BC   ; u
    $D1 C,            \ POP DE   ; dest
    $E1 C,            \ POP HL   ; src
    $F3 C,            \ DI
    $D3 C, $38 C,     \ OUT $38,A   ; page ram
    HERE                   ( label )
    $ED C, $A0 C,     \ LDI
    $78 C,            \ LD A,B
    $B1 C,            \ OR C
    $C2 C, ,          \ JP NZ, label
    $D3 C, $38 C,     \ OUT $38,A   ; page rom
    $FB C,            \ EI
    $D9 C,            \ EXX
    $C1 C,            \ POP BC      ; next TOS
    NEXT,
;CODE
