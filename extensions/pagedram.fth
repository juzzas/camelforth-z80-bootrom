\ CamelForth BootROM to access the RC2014 paged 32k RAM

ONLY FORTH ALSO SYSTEM DEFINITIONS

1 4 +THRU

PREVIOUS DEFINITIONS









\ Fetch byte from paged RAM   ( addr -- c )
ALSO ASSEMBLER
CODE PRC@
   DI              \ DI
   $38 OUT         \ OUT $38,A
   B LDAX          \ LD A,(BC)
   $38 OUT         \ OUT $38,A
   EI              \ EI
   0 B MVI         \ LD B, 0
   A C MOV         \ LD C, A

\   <$  F3  D3 38  0A  D3 38  FB  06 00  4F  $>
   NEXT,
;CODE


\ Store byte from paged RAM   ( c addr -- )
CODE PRC!
   B PUSH      \ PUSH BC
   H POP       \ POP HL      ; addr 
   B POP       \ POP BC      ; c 
   DI          \ DI
   $38 OUT     \ OUT $38,A   ; page ram
   C M MOV     \ LD (HL),C
   $38 OUT     \ OUT $38,A   ; page rom
   EI          \ EI
   B POP       \ POP BC      ; next TOS

\   <$  C5  E1  C1  F3  D3 38  71  D3 38  FB  C1  $>
   NEXT,
;CODE


\ Copy block from paged RAM  ( src dest u -- )
CODE PRMOVE
   B PUSH           \ PUSH BC
   EXX              \ EXX
   B POP            \ POP BC   ; u
   D POP            \ POP DE   ; dest
   H POP            \ POP HL   ; src
   DI               \ DI
   $38 OUT          \ OUT $38,A   ; page ram
   BEGIN
     LDI                          \ LDI
     B A MOV        \ LD A,B
     C ORA          \ OR C
   0= UNTIL         \ JP NZ, label
   $38 OUT          \ OUT $38,A   ; page rom
   EI               \ EI
   EXX              \ EXX
   B POP            \ POP BC      ; next TOS
   NEXT,
;CODE
