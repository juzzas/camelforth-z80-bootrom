(  Extensions planned to be integrated to ROM source )


: (MEMPOKE)    ( addr --     accept to poke )
    DUP 4 .W ." : "
    C@
    DUP 2 .W
    ." -> "
    BUFFER DUP BUFFERSIZE ACCEPT   ( addr old-c tib-addr u )
    .S CR
    ?DUP 0= IF
        CR .S
        DROP      ( addr old-c )
        2 .W
        DROP      ( empty )
    ELSE
        ." entering" CR     ( addr old-c tib-addr u )
        NUMBER
        CR .S
    THEN
    .S
    ;

: MEMPOKE
    DUP
    (MEMPOKE)
    ;


: AHEAD   POSTPONE FALSE  POSTPONE IF ; IMMEDIATE
: ?EXIT   POSTPONE IF  POSTPONE EXIT  POSTPONE THEN ; IMMEDIATE
: .(   41 PARSE TYPE ; IMMEDIATE