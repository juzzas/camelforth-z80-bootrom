\ Forth 2012 Core extensions for CamelForth BootROM

.( Loading core-ext definitions... ) CR

FORTH DEFINITIONS
1 2 +THRU










: 2NIP  2SWAP 2DROP  ;
: 2ROT  5 ROLL  5 ROLL ;
VARIABLE TO-STATE  FALSE TO-STATE !
: TO TRUE TO-STATE ! ;
: VALUE CREATE , 
   DOES> TO-STATE @ IF !  FALSE TO-STATE ! ELSE @ THEN  ;

: (C")    ( -- c-addr ) \   run-time code for C"
  R> DUP  COUNT + ALIGNED >R  ;
: C"  ['] (C") COMPILE,   [CHAR] "  PARSE    HERE >COUNTED 
    HERE C@ 1+ ALIGNED ALLOT ; IMMEDIATE

