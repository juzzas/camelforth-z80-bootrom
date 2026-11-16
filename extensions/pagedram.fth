\ CamelForth BootROM to access the RC2014 paged 32k RAM

ONLY FORTH ALSO SYSTEM DEFINITIONS

1 6 +THRU

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
   NEXT,
;CODE

\ Store byte to paged RAM   ( c addr -- )
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
   NEXT,
;CODE

\ Copy block from paged RAM (move from bottom)
CODE (PRMOVE)  ( src dest u -- )
   B PUSH           \ PUSH BC
   EXX              \ EXX
   B POP            \ POP BC   ; u
   D POP            \ POP DE   ; dest
   H POP            \ POP HL   ; src

   B A MOV          \ LD A,B
   C ORA            \ OR C
   0<> IF
      DI            \ DI
      $38 OUT       \ OUT $38,A   ; page ram
      LDIR          \ LDIR
      $38 OUT       \ OUT $38,A   ; page rom
      EI            \ EI
   THEN

   EXX              \ EXX
   B POP            \ POP BC      ; next TOS
   NEXT,
;CODE

\ Copy block from paged RAM (move from top)
CODE (PRMOVE>)  ( src dest u -- )
   B PUSH           \ PUSH BC
   EXX              \ EXX
   B POP            \ POP BC   ; u
   D POP            \ POP DE   ; dest
   H POP            \ POP HL   ; src

   B A MOV          \ LD A,B
   C ORA            \ OR C
   0<> IF
      DI            \ DI
      $38 OUT       \ OUT $38,A   ; page ram
      B DAD         \ ADD HL, BC   \ last byte in destination
      H DCX         \ DEC HL
      XCHG          \ EX DE, HL
      B DAD         \ ADD HL, BC   \ last byte in source
      H DCX         \ DEC HL
      LDDR
      $38 OUT       \ OUT $38,A   ; page rom
      EI            \ EI
   THEN

   EXX              \ EXX
   B POP            \ POP BC      ; next TOS
   NEXT,
;CODE

PREVIOUS

\ Fetch byte from paged RAM, increment address. Useful for memory walking.
: PRC@+  ( addr -- addr' c )
   DUP PRC@ >R 1+ R> ;

\ Store byte into paged RAM, increment address. Useful for memory walking.
: PRC!+  ( c addr -- addr' )
   TUCK PRC! 1+ ;

\ Smart paged ram move
: PRMOVE    ( addr1 addr2 u -- )
   >R 2DUP SWAP DUP R@ +  \   -- ... dst src src+n
   WITHIN IF  R> (PRMOVE>)   \     src <= dst < src+n
          ELSE  R> (PRMOVE)  THEN ; \         otherwise

