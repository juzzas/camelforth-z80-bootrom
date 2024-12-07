HEX
CODE: [I  F3 C, ( DI )  NEXT, ;CODE \ enter critical section
CODE: I]  FB C, ( EI )  NEXT, ;CODE \ exit critical section
: ATOMIC@ ( a-addr -- n )  [I @ I] ;
: ATOMIC! ( n a-addr -- )  [I ! I] ;

: TASK     ( ccc"name" --  allocate task )
           ( Execution:  -- task-id )
   CREATE TASK% ALLOT ;

: STATUS  U0 ;
: TASK>STATUS ( task-id -- addr ) STATUS U0 - + ;
: TASK>LINK ( task-id -- addr ) LINK U0 - + ;
: TASK>ENTRY ( task-id -- addr ) ENTRY U0 - + ;


: START-TASK ( xt task-id -- ) 
    DUP INIT-TASK  TASK>ENTRY !  ;

: STOP-TASK  ( task-id -- )
   TASK>STATUS ['] <SLEEP> SWAP ! ;

: MULTI ( -- )  ['] (PAUSE) 'PAUSE ! ;
: SINGLE ( -- ) ['] NOOP 'PAUSE ! ;
: PAUSE  'PAUSE @ EXECUTE ;








VARIABLE task-count ;
\ : task1  0 task-count ! BEGIN
\    task-count @ 1+
\    task-count ! PAUSE  AGAIN ;
: task1 ." hello!" ;
TASK t1
: RUN  ['] task1 t1 START-TASK  ;
DECIMAL
