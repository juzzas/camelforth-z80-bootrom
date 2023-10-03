(  Extensions planned to be integrated to ROM source )

: \   ( --  ignores rest of line as comment  )
    13 WORD DROP ; IMMEDIATE

: <ACCEPT  ( c-addr +n -- +n'   get line from term.; no echo )
    OVER + 1- OVER      \ sa ea a
    BEGIN KEY           \ sa ea a c
    DUP 13 <> WHILE
        OVER C! 1+ OVER UMIN
    REPEAT              \ sa ea a c
    DROP NIP SWAP - ;

: NUMBER ( c-addr +n -- d  | +n  )
    0 0 2SWAP >NUMBER
    DROP SWAP DROP ;

: [IHXREC] ( c-addr -- 0 if ok, 1 if end, 2 if error  parse hex record )
    [ HEX ]
    ." start: " .S CR
    DUP C@ [CHAR] : <> IF
        ABORT" IHXREC: no colon"
    THEN
    CHAR+
    2 NUMBER   ( count )
    ." bytes:" .S  CR
    4 NUMBER   ( address )
    ." addr:" .S  CR
    2 NUMBER   ( record type )
    ." record:" .S  CR
    SWAP 0=  IF
        ." not end" CR
    ELSE
        ." END!" CR
    THEN
    ;


\ :07F00000EF7A535F1CE7C922
\ :00000001FF

: HEXLOAD
    ." HEXLOAD test" CR
    [CHAR] : EMIT
    TIB DUP TIBSIZE <ACCEPT
    DROP                             ( drop count )
    [IHXREC] ;
