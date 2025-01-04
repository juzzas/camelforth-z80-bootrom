\ Forth 2012 Core extensions for CamelForth BootROM

.( Loading core-ext definitions... ) CR

FORTH DEFINITIONS
1 2 +THRU










: 2NIP  2SWAP 2DROP  ;
: 2ROT  5 ROLL  5 ROLL ;
: VALUE CREATE , 
   DOES> @  ;
: TO ;
: (C")    ( -- c-addr ) \   run-time code for C"
   R> COUNT OVER + ALIGNED >R  ;
: C"   [CHAR] "   PARSE ;
