\ CamelForth BootROM multi-tasking wordset

CR .( Loading multi-tasking extension... )

ONLY FORTH DEFINITIONS  ALSO SYSTEM

1 4 +THRU

PREVIOUS







   \ CamelForth BootROM multi-tasking wordset     1 / 4

CODE [I  $F3 C, ( DI )  NEXT, ;CODE \ enter critical section
CODE I]  $FB C, ( EI )  NEXT, ;CODE \ exit critical section


: ATOMIC@ ( a-addr -- n )  [I @ I] ;
: ATOMIC! ( n a-addr -- )  [I ! I] ;
: ATOMIC+! ( n a-addr -- )  [I +! I] ;
: ATOMIC@!  ( n a-addr -- n' )  DUP [I @ >R  !  R> I] ;






   \ CamelForth BootROM multi-tasking wordset     2 / 4
: TASK  ( ccc"name" --  allocate task )
        ( Execution:  -- task-id )    CREATE TASK% ALLOT ;

: STATUS  U0 ;  ( alias for STATUS USER variable )
: TASK>STATUS ( task-id -- addr ) STATUS U0 - + ;
: TASK>LINK ( task-id -- addr ) LINK U0 - + ;
: TASK>ENTRY ( task-id -- addr ) ENTRY U0 - + ;








   \ CamelForth BootROM multi-tasking wordset     3 / 4
: START-TASK ( xt task-id -- ) 
    DUP INIT-TASK  TASK>ENTRY !  ;
: RESTART-TASK ( xt task-id -- ) 
    TUCK TASK>ENTRY !  ['] <INIT> SWAP !  ;

: STOP-TASK  ( task-id -- )
   TASK>STATUS ['] <SLEEP> SWAP ! ;
: WAKE-TASK  ( task-id -- )
   TASK>STATUS  ['] <WAKE>  SWAP ! ;
: SLEEP ( -- ) ['] <SLEEP> STATUS ! PAUSE ;





   \ CamelForth BootROM multi-tasking wordset     4 / 4

: MULTI ( -- )  ['] (PAUSE) 'PAUSE ! ;
: SINGLE ( -- ) ['] NOOP 'PAUSE ! ;



