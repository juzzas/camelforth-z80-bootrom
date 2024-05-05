( ttester is based on the original tester suite by Hayes: 1/7  )

( Usage:
( The basic usage takes the form:                              )
(     T{ <code> -> <expected stack> }T .                       )
( This executes  <code>  and compares the resulting stack      )
( contents with the  <expected stack>  values, and reports any )
( discrepancy between the two sets of values.                  )
( For example:                                                 )
(     T{ 1 2 3 SWAP -> 1 3 2 }T  OK                            )
(     T{ 1 2 3 SWAP -> 1 2 2 }T                                )
(               INCORRECT RESULT: T{ 1 2 3 SWAP -> 1 2 2 }T OK )
(     T{ 1 2 3 SWAP -> 1 2 }T                                  )
(          WRONG NUMBER OF RESULTS: T{ 1 2 3 SWAP -> 1 2 }T OK )


( ttester is based on the original tester suite by Hayes: 2/7  )
VARIABLE ACTUAL-DEPTH
CREATE ACTUAL-RESULTS 32 CELLS ALLOT
VARIABLE START-DEPTH
VARIABLE XCURSOR      ( for ...}T )
VARIABLE ERROR-XT     ( error reporting vector )
VARIABLE #ERRORS 0 #ERRORS !

: TRUE -1 ;
: FALSE 0 ;

VARIABLE VERBOSE
   TRUE VERBOSE !



( ttester is based on the original tester suite by Hayes: 3/7  )
: ERROR 1 #ERRORS +! ERROR-XT @ EXECUTE ;   ( for vector )
: EMPTY-STACK      ( ... -- )         ( empty stack; handles   )
    DEPTH START-DEPTH @ < IF          ( underflowed stack too. )
        DEPTH START-DEPTH @ SWAP DO 0 LOOP
    THEN
    DEPTH START-DEPTH @ > IF
        DEPTH START-DEPTH @ DO DROP LOOP
    THEN   ;

: ERROR1  ( C-ADDR U -- )   ( display an error message )
   TYPE SOURCE TYPE CR      ( display line of error )
   EMPTY-STACK      ;       ( throw away everything else )

' ERROR1 ERROR-XT !

( ttester is based on the original tester suite by Hayes: 4/7  )
: T{   ( -- )
   DEPTH START-DEPTH ! 0 XCURSOR ! ;

: ->       ( ... -- )    ( record depth and contents of stack )
   DEPTH DUP ACTUAL-DEPTH !   ( record depth )
   START-DEPTH @ > IF     ( if there is something on the stack )
       DEPTH START-DEPTH @ - 0 DO ACTUAL-RESULTS
          I CELLS + ! LOOP   ( save them )
   THEN   ;






( ttester is based on the original tester suite by Hayes: 5/7  )
: }T    ( ... -- )    ( COMPARE STACK "EXPECTED" CONTENTS WITH )
                      ( SAVED  "ACTUAL"  CONTENTS. )
   DEPTH ACTUAL-DEPTH @ = IF       ( if depths match )
      DEPTH START-DEPTH @ > IF     ( if something on the stack )
         DEPTH START-DEPTH @ - 0 DO     ( for each stack item )
                               ( compare actual with expected )
            ACTUAL-RESULTS I CELLS + @
            <> IF S" INCORRECT RESULT: " ERROR LEAVE THEN
         LOOP
      THEN
   ELSE                                  ( depth mismatch )
      S" WRONG NUMBER OF RESULTS: " ERROR
   THEN   ;


( ttester is based on the original tester suite by Hayes: 6/7  )
: ...}T ( -- )
   XCURSOR @ START-DEPTH @ + ACTUAL-DEPTH @ <> IF
      S" # CELLS BEFORE '->' DOES NOT MATCH ...}T SPEC: "
         ERROR
   ELSE DEPTH START-DEPTH @ = 0= IF
      S" # CELLS BEFORE AND AFTER '->' DOES NOT MATCH: "
         ERROR
   THEN THEN   ;
: XTESTER ( X -- )
   DEPTH 0= ACTUAL-DEPTH @ XCURSOR @ START-DEPTH @ + 1+ < OR IF
      S" # CELLS RESULTS AFTER '->' BELOW ...}T SPEC: "
           ERROR EXIT
   ELSE ACTUAL-RESULTS XCURSOR @ CELLS + @ <> IF
      S" INCORRECT CELL RESULT: " ERROR
   THEN THEN   1 XCURSOR +! ;
( ttester is based on the original tester suite by Hayes: 7/7  )
: X}T XTESTER ...}T ;
: XX}T XTESTER XTESTER ...}T ;
: XXX}T XTESTER XTESTER XTESTER ...}T ;
: XXXX}T XTESTER XTESTER XTESTER XTESTER ...}T ;

: TESTING        ( -- ) ( TALKING COMMENT. )
   SOURCE VERBOSE @
   IF DUP >R  CR TYPE SPACE  R> >IN !
   ELSE >IN ! DROP
   THEN ;

TESTING T{ 1 2 3 SWAP -> 1 3 2 XXX}T
