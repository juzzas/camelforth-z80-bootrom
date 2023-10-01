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
    
: C!  ( fake c-store )
    SWAP . ." -> " . CR ;

: [IHXREC] ( c-addr -- 0 if ok, 1 if end, 2 if error  parse hex record )
    [ HEX ]
    ." start: " .S CR
    DUP C@ [CHAR] : <> IF
        ABORT" IHXREC: no colon"
    THEN
    CHAR+
    2 NUMBER   ( count tib-ptr )
    ." bytes:" .S  CR
    4 NUMBER   ( count hex-addr tib-ptr )
    ." addr:" .S  CR
    2 NUMBER   ( count hex-addr record-type tib-ptr )
    ." record:" .S  CR
    SWAP 0=  IF               ( count hex-addr tib-ptr )
        ." not end" CR
        ." record 0:" .S  CR
        ROT 0 DO
            2 NUMBER
            >R                ( hex-addr hex-byte )
            ." record loop:" .S  CR
            2DUP C!
            DROP             ( hex-addr )
            1+               ( hex-addr+1 )
            R>
        LOOP
    ELSE
        ." END!" CR
    THEN
    ;

256 CONSTANT BUFFERSIZE
CREATE BUFFER BUFFERSIZE CHARS ALLOT

\ :07F00000EF7A535F1CE7C922
\ :00000001FF

: HEXLOAD
    ." HEXLOAD test" CR
    [CHAR] : EMIT
    BUFFER DUP BUFFERSIZE ACCEPT
    DROP                             ( drop count )
    [IHXREC] ;
