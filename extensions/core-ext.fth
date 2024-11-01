\ Forth 2012 Core extensions for CamelForth BootROM

.( Loading core-ext definitions... ) CR

FORTH DEFINITIONS
1 2 +THRU










   \ Forth 2012 Core-ext -- CASE/ENDCASE
: CASE ( -- 0 ) 0 ; IMMEDIATE
: OF ( -- )
    POSTPONE OVER POSTPONE =
    POSTPONE IF POSTPONE DROP ; IMMEDIATE
: ENDOF ( -- ) POSTPONE ELSE ; IMMEDIATE
: ENDCASE ( -- )
    POSTPONE DROP
    BEGIN ?DUP WHILE POSTPONE THEN REPEAT ; IMMEDIATE







: TRUE -1 ;
: FALSE 0 ;
: 2NIP  2SWAP 2DROP  ;

: DEFER    ( "name" -- )        create a deferred word
    CREATE ['] NOOP ,
   DOES>
    @ EXECUTE ;

: DEFER!   ( xt2 xt1 -- )             store xt2 in xt1
   >BODY ! ;

: DEFER@   ( xt2 xt1 -- )             store xt2 in xt1
   >BODY ! ;

: IS       ( xt "name" -- )     define a deferred word
   STATE @ IF
      POSTPONE ['] POSTPONE DEFER!
   ELSE
      ' DEFER!
   THEN ; IMMEDIATE

: ACTION-OF  ( "name -- xt" )     get the action of a deferred word
   STATE @ IF
      POSTPONE ['] POSTPONE DEFER@
   ELSE
      ' DEFER@
   THEN ; IMMEDIATE
