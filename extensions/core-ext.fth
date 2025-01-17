\ Forth 2012 Core extensions for CamelForth BootROM

.( Loading core-ext definitions... ) CR

ONLY FORTH DEFINITIONS
1 3 +THRU










\ Forth 2012 Core extensions for CamelForth BootROM       1 / n
: DEFER    ( "name" -- )      \  create a deferred word
   CREATE ['] NOOP ,
   DOES>
   @ EXECUTE ;

: DEFER!   ( xt2 xt1 -- )     \        store xt2 in xt1
   >BODY ! ;

: DEFER@   ( xt1 -- xt2 )     \        fetch xt2 from xt1
   >BODY @ ;

: IS       ( xt "name" -- )     \ define a deferred word
   STATE @  IF  POSTPONE [']  POSTPONE DEFER!
   ELSE  ' DEFER!  THEN ; IMMEDIATE

\ Forth 2012 Core extensions for CamelForth BootROM       2 / n

\ get the action of a deferred word
: ACTION-OF  ( "name -- xt" )    
   STATE @  IF POSTPONE ['] POSTPONE DEFER@
   ELSE  ' DEFER@  THEN ; IMMEDIATE

: 2NIP  2SWAP 2DROP  ;
: 2ROT  5 ROLL  5 ROLL ;

VARIABLE TO-STATE  FALSE TO-STATE !
: TO TRUE TO-STATE ! ;
: VALUE CREATE , 
   DOES> TO-STATE @ IF !  FALSE TO-STATE ! ELSE @ THEN  ;


\ Forth 2012 Core extensions for CamelForth BootROM       3 / n

: (C")    ( -- c-addr ) \   run-time code for C"
  R> DUP  COUNT + ALIGNED >R  ;
: C"   ['] (C") COMPILE,   [CHAR] "  PARSE   HERE >COUNTED 
    HERE C@ 1+ ALIGNED ALLOT ; IMMEDIATE

FALSE VALUE s\"esc
0 VALUE s\"count

: \C,  ( c -- )   \ compiles escaped characters
  s\"esc IF 
    ELSE
    C, s\"count 1+ TO s\"count
  THEN  ;


\ : S\"  ['] (S") COMPILE,
\     SOURCE >IN @ DUP >R /STRING    ( addr n ; offset )
\     0     FALSE s\"esc !    ( addr n cnt ; offset )
\     BEGIN
\       OVER  WHILE
\           ROT 1 -    ( cnt addr n' ; offset )
\           ROT DUP C@ \C, 1+  ( n' cnt addr' ; offset )
\           ROT 1+  ROT ( addr' n' cnt' ; offset )


\ [CHAR] "  PARSE    HERE >COUNTED
\    HERE C@ 1+ ALIGNED ALLOT ; IMMEDIATE

