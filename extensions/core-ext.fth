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
