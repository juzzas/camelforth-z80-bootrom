HEX
CODE: [I  F3 C, ( DI )  NEXT, ;CODE \ enter critical section
CODE: I]  FB C, ( EI )  NEXT, ;CODE \ exit critical section
: ATOMIC@ ( a-addr -- n )  [I @ I] ;
: ATOMIC! ( n a-addr -- )  [I ! I] ;

: TASK     ( ccc"name" --  allocate task )
           ( Execution:  -- task-id )
   CREATE TASK% ALLOT ;

: SELF-TASK  ( -- task-id )  U0 ;

: (PAUSE)  SELF-TASK NEXT-TASK SWITCH-TASK ;



: START-TASK ( xt task-id -- ) 
    TUCK INIT-TASK  ( task-id )
    -1 OVER TASK>ACTIVE ! ( task-id )
    LINK @ OVER TASK>LINK ! ( task-id )
    LINK ! ;

: STOP-TASK  ( task-id -- )
   TASK>ACTIVE 0 SWAP ! ;

: MULTI ( -- )  ['] (PAUSE) 'PAUSE ! ;
: SINGLE ( -- ) ['] NOOP 'PAUSE ! ;





VARIABLE task-count ;
\ : task1  0 task-count ATOMIC! BEGIN
\    task-count ATOMIC@ +1
\    task-count ATOMIC! PAUSE  AGAIN ;
: task1 ." hello!" ;
TASK t1
: RUN  ['] task1 t1 START-TASK  ;
DECIMAL
