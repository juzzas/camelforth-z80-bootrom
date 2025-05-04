\ CamelForth BootROM to access the RC2014 paged 32k RAM

ONLY FORTH ALSO SYSTEM DEFINITIONS

1 4 +THRU

PREVIOUS DEFINITIONS









\ Fetch byte from paged RAM   ( addr -- c )
CODE PRC@
   \ DI
   \ OUT $38,A
   \ LD A,(BC)
   \ OUT $38,A
   \ EI
   \ LD B, 0
   \ LD C, A

   <$  F3  D3 38  0A  D3 38  FB  06 00  4F  $>
   NEXT,
;CODE



\ Store byte from paged RAM   ( c addr -- )
CODE PRC!
   \ PUSH BC
   \ POP HL      ; addr 
   \ POP BC      ; c 
   \ DI
   \ OUT $38,A   ; page ram
   \ LD (HL),C
   \ OUT $38,A   ; page rom
   \ EI
   \ POP BC      ; next TOS

   <$  C5  E1  C1  F3  D3 38  71  D3 38  FB  C1  $>
   NEXT,
;CODE


\ Copy block from paged RAM  ( src dest u -- )
CODE PRMOVE
   <$  C5     $>       \ PUSH BC
   <$  D9     $>       \ EXX
   <$  C1     $>       \ POP BC   ; u
   <$  D1     $>       \ POP DE   ; dest
   <$  E1     $>       \ POP HL   ; src
   <$  F3     $>       \ DI
   <$  D3 38  $>       \ OUT $38,A   ; page ram
   HERE                   ( label )
   <$  ED A0  $>       \ LDI
   <$  78     $>       \ LD A,B
   <$  B1     $>       \ OR C
   <$  C2     $> ,     \ JP NZ, label
   <$  D3 38  $>       \ OUT $38,A   ; page rom
   <$  FB     $>       \ EI
   <$  D9     $>       \ EXX
   <$  C1     $>       \ POP BC      ; next TOS
   NEXT,
;CODE
