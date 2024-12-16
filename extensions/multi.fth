ALSO UTILS   HEX
CODE: [I  F3 C, ( DI )  NEXT, ;CODE \ enter critical section
CODE: I]  FB C, ( EI )  NEXT, ;CODE \ exit critical section
DECIMAL
: ATOMIC@ ( a-addr -- n )  [I @ I] ;
: ATOMIC! ( n a-addr -- )  [I ! I] ;
: ATOMIC+! ( n a-addr -- )  [I +! I] ;
: ATOMIC!@  ( n a-addr -- n' )  DUP [I @ >R  !  R> I] ;
: TASK  ( ccc"name" --  allocate task )
        ( Execution:  -- task-id )    CREATE TASK% ALLOT ;

: STATUS  U0 ;  ( alias for STATUS USER variable )
: TASK>STATUS ( task-id -- addr ) STATUS U0 - + ;
: TASK>LINK ( task-id -- addr ) LINK U0 - + ;
: TASK>ENTRY ( task-id -- addr ) ENTRY U0 - + ;

: START-TASK ( xt task-id -- ) 
    DUP INIT-TASK  TASK>ENTRY !  ;
: RESTART-TASK ( xt task-id -- ) 
    TUCK TASK>ENTRY !  ['] <INIT> SWAP !  ;

: STOP-TASK  ( task-id -- )
   TASK>STATUS ['] <SLEEP> SWAP ! ;
: AWAKEN-TASK  TASK>STATUS  ['] <WAKE>  SWAP ! ;
: WAKE ( -- ) ['] <WAKE> STATUS ! ;
: SLEEP ( -- ) ['] <SLEEP> STATUS ! ;

: MULTI ( -- )  ['] (PAUSE) 'PAUSE ! ;
: SINGLE ( -- ) ['] NOOP 'PAUSE ! ;
: PAUSE  'PAUSE @ EXECUTE ;
PREVIOUS



VARIABLE task-count ;
: task1 
   0 task-count ! BEGIN  1 task-count ATOMIC+!  PAUSE  AGAIN ;
: task2 ." hello!" ;
TASK t1    ' task1 t1 START-TASK
TASK t2    ' task2 t2 START-TASK
: .t1  task-count ATOMIC@ . ;

MULTI

DECIMAL
