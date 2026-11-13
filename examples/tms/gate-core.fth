\ Initialise the SEED variable to a non-zero value and
\ use RND or RANDOM to generate random numbers

: xorshift ( n -- n )
   dup 7 lshift xor
   dup 9 rshift xor
   dup 8 lshift xor ;
      
variable seed
      
: rnd ( -- n )
   seed @
   xorshift
   dup seed ! ;
            
\ WARNING: Not evenly distributed but should be good
: random ( n -- n )
   rnd swap mod abs ;
   
40 CONSTANT SCR_W
30 CONSTANT SCR_H

1 CONSTANT SHIP_Y
VARIABLE SHIP_X

: SIGNON
   10 10 AT-XY
   ." The enemy gate is down!"
   1 3 DO 20 12 AT-XY i . 1000 MS -1 +LOOP
   ;

: ESC ( -- emit escape character )
   27 emit ;

: CLS ( -- clear screen )
   ESC ." [2J" ;


: INK ( n -- change ink to n 0-255 )
   ESC ." [38;5;"
   (.) TYPE
   ." m" ;


: PAPER ( n -- change paper to n 0-255 )
   ESC ." [48;5;"
   (.) TYPE
   ." m" ;

: INVIS
   ESC ." [?25l" ;

: VIS
   ESC ." [?25h" ;


: SCROLL
   27 EMIT ." [S"
   ;

CREATE ALIEN1 56 C, 124 C, 254 C, 214 C, 146 C, 146 C, 186 C, 0 C,

CREATE ALIEN2 16 C, 124 C, 124 C, 254 C, 124 C, 124 C, 16 C, 0 C,

CREATE SHIP1 0 C, 146 C, 254 C, 254 C, 254 C, 124 C, 124 C, 124 
C,

CREATE SHIP2 124 C, 108 C, 68 C, 108 C, 40 C, 56 C, 16  C, 0 C,

: ALIEN1_LD
   ESC ." [?128U"
   8 0 DO ALIEN1 I + C@ EMIT LOOP
   ;

: ALIEN2_LD
   ESC ." [?129U"
   8 0 DO ALIEN2 I + C@ EMIT LOOP
   ;

: SHIP1_LD
   ESC ." [?130U"
   8 0 DO SHIP1 I + C@ EMIT LOOP
   ;

: SHIP2_LD
   ESC ." [?131U"
   8 0 DO SHIP2 I + C@ EMIT LOOP
   ;

: ALIEN1_XY
   1 INK AT-XY 128 EMIT
   ;

: ALIEN2_XY
   8 INK AT-XY 129 EMIT
   ;
  
: LL 
   SCR_W 0 do 40 RANDOM
   ?dup 
   0= if i SCR_H 1- ALIEN1_XY 
   else 
      3 < if i SCR_H 1- ALIEN2_XY
      THEN 
   THEN LOOP
   ;

: INIT 
   42 SEED ! 
   20 SHIP_X !
   ALIEN1_LD
   ALIEN2_LD
   SHIP1_LD
   SHIP2_LD
   ;   

: SHIP_OUT
   3 INK
   SHIP_X @ SHIP_Y AT-XY 130 EMIT
   SHIP_X @ SHIP_Y 1+ AT-XY 131 EMIT
   ;
   
: SHIP_CLR
   SHIP_X @ SHIP_Y AT-XY 32 EMIT
   SHIP_X @ SHIP_Y 1+ AT-XY 32 EMIT
   ;

\ movement
: SHIP_L  SHIP_X @ 1 - SHIP_X ! ;
: SHIP_R  SHIP_X @ 1 + SHIP_X ! ;

VARIABLE SHIP-KEY

\ o (111) = left
\ p (112) = right
: SHIP_MOVE
   KEY? IF
      KEY SHIP-KEY !
      SHIP-KEY @ 111 = IF SHIP_L THEN
      SHIP-KEY @ 112 = IF SHIP_R THEN
   THEN ;
             
: RUN 
   INIT
   CLS
   INVIS
   SHIP_OUT
   SIGNON
   CLS
   SHIP_OUT
   100 0 DO 
      SHIP_CLR 
      SCROLL
      SHIP_MOVE 
      SHIP_OUT 
      LL 
   LOOP
   VIS
   ;


