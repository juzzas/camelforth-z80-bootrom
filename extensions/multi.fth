\ CamelForth BootROM multi-tasking wordset

CR .( Loading multi-tasking extension... )

ONLY FORTH DEFINITIONS  ALSO SYSTEM

1 2 +THRU

PREVIOUS







   \ CamelForth BootROM multi-tasking wordset     1 / 4

CODE [C  $F3 C, ( DI )  NEXT, ;CODE \ enter critical section
CODE C}  $FB C, ( EI )  NEXT, ;CODE \ exit critical section


: ATOMIC@ ( a-addr -- n )  [C @ C] ;
: ATOMIC! ( n a-addr -- )  [C ! C] ;
: ATOMIC+! ( n a-addr -- )  [C +! C] ;
: ATOMIC@!  ( n a-addr -- n' )  DUP [C @ >R  !  R> C] ;






   \ CamelForth BootROM multi-tasking wordset     2 / 4
: TASK  ( ccc"name" --  allocate task )
        ( Execution:  -- task-id )    CREATE TASK% ALLOT ;



