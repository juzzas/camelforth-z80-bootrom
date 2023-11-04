( ttester is based on the original tester suite by Hayes: )

( Usage:
( The basic usage takes the form  T{ <code> -> <expected stack> }T . )
( This executes  <code>  and compares the resulting stack contents with )
( the  <expected stack>  values, and reports any discrepancy between the )
( two sets of values. )
( For example: )
( T{ 1 2 3 SWAP -> 1 3 2 }T  OK )
( T{ 1 2 3 SWAP -> 1 2 2 }T INCORRECT RESULT: T{ 1 2 3 SWAP -> 1 2 2 }T OK )
( T{ 1 2 3 SWAP -> 1 2 }T WRONG NUMBER OF RESULTS: T{ 1 2 3 SWAP -> 1 2 }T OK )

VARIABLE ACTUAL-DEPTH
CREATE ACTUAL-RESULTS 32 CELLS ALLOT
VARIABLE START-DEPTH
VARIABLE XCURSOR      ( for ...}T )
VARIABLE ERROR-XT
VARIABLE #ERRORS 0 #ERRORS !

: TRUE -1 ;
: FALSE 0 ;

VARIABLE VERBOSE
   FALSE VERBOSE !

: ERROR 1 #ERRORS +! ERROR-XT @ EXECUTE ;   ( for vectoring of error reporting )
: EMPTY-STACK      ( ... -- )  ( empty stack; handles underflowed stack too. )
    DEPTH START-DEPTH @ < IF
        DEPTH START-DEPTH @ SWAP DO 0 LOOP
    THEN
    DEPTH START-DEPTH @ > IF
        DEPTH START-DEPTH @ DO DROP LOOP
    THEN   ;

: ERROR1  ( C-ADDR U -- )      ( display an error message )
                               ( followed by the line that had the error. )
   TYPE SOURCE TYPE CR         ( display line corresponding to error )
   EMPTY-STACK                 ( throw away everything else )
;

' ERROR1 ERROR-XT !

: T{   ( -- )
   DEPTH START-DEPTH ! 0 XCURSOR ! ;

: ->       ( ... -- )    ( record depth and contents of stack )
   DEPTH DUP ACTUAL-DEPTH !   ( record depth )
   START-DEPTH @ > IF         ( if there is something on the stack )
       DEPTH START-DEPTH @ - 0 DO ACTUAL-RESULTS I CELLS + ! LOOP   ( save them )
   THEN   ;

: }T    ( ... -- )      ( COMPARE STACK "EXPECTED" CONTENTS WITH SAVED )
                        ( "ACTUAL"  CONTENTS. )
   DEPTH ACTUAL-DEPTH @ = IF             ( if depths match )
      DEPTH START-DEPTH @ > IF           ( if there is something on the stack )
         DEPTH START-DEPTH @ - 0 DO      ( for each stack item )
            ACTUAL-RESULTS I CELLS + @   ( compare actual with expected )
            <> IF S" INCORRECT RESULT: " ERROR LEAVE THEN
         LOOP
      THEN
   ELSE                                  ( depth mismatch )
      S" WRONG NUMBER OF RESULTS: " ERROR
   THEN   ;

: ...}T ( -- )
    XCURSOR @ START-DEPTH @ + ACTUAL-DEPTH @ <> IF
        S" NUMBER OF CELL RESULTS BEFORE '->' DOES NOT MATCH ...}T SPECIFICATION: " ERROR
    ELSE DEPTH START-DEPTH @ = 0= IF
        S" NUMBER OF CELL RESULTS BEFORE AND AFTER '->' DOES NOT MATCH: " ERROR
    THEN THEN   ;

: XTESTER ( X -- )
    DEPTH 0= ACTUAL-DEPTH @ XCURSOR @ START-DEPTH @ + 1+ < OR IF
        S" NUMBER OF CELL RESULTS AFTER '->' BELOW ...}T SPECIFICATION: " ERROR EXIT
    ELSE ACTUAL-RESULTS XCURSOR @ CELLS + @ <> IF
        S" INCORRECT CELL RESULT: " ERROR
    THEN THEN
    1 XCURSOR +! ;

: X}T XTESTER ...}T ;
: XX}T XTESTER XTESTER ...}T ;
: XXX}T XTESTER XTESTER XTESTER ...}T ;
: XXXX}T XTESTER XTESTER XTESTER XTESTER ...}T ;

: TESTING        ( -- ) ( TALKING COMMENT. )
   SOURCE VERBOSE @
   IF DUP >R  CR TYPE SPACE  R> >IN !
   ELSE >IN ! DROP
   THEN ;

T{ 1 2 3 SWAP -> 1 3 2 XX}T
