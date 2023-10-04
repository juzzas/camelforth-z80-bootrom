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

: NUMBER ( c-addr +n -- d  | +n   tibptr )
    0 0 2SWAP >NUMBER
    DROP SWAP DROP ;

 : C!  ( fake c-store )
    SWAP . ." -> " . CR ;

\ : C@   ( c-addr -- c     fake c-fetch )
\     ( DUP . ." <-- " )
\     255 AND
\    ( DUP . )
\    ;

VARIABLE IHXCRC
HEX

0 CONSTANT IHX-OK
1 CONSTANT IHX-END
2 CONSTANT IHX-ERROR

: IHXCRC+  ( c -- )
    IHXCRC @ + FF AND IHXCRC ! ;

: ?IHXCRC  ( -- c )
    IHXCRC @ NEGATE FF AND ;

: (IHXBYTE)  ( tib-ptr -- u tib-ptr )
    >R 0 S>D R> 2
    >NUMBER       ( du tib-ptr u )
    DROP NIP      ( u tib-ptr )
    ;

: IHXBYTE  ( tib-ptr -- u tib-ptr )
    (IHXBYTE)
    OVER IHXCRC+
    ;

: IHXWORD  ( tib-ptr -- u tib-ptr )
    IHXBYTE IHXBYTE
    ROT 8 LSHIFT ROT +
    SWAP
    ;

: IHXREC ( c-addr -- 0 if ok, 1 if end, 2 if error  parse hex record )
    0 IHXCRC !
    DUP C@ [CHAR] : <> IF
        ABORT" IHXREC: no colon"
    THEN
    CHAR+
    IHXBYTE   ( count tib-ptr )
    IHXWORD   ( count hex-addr tib-ptr )
    IHXBYTE   ( count hex-addr record-type tib-ptr )
    SWAP 0=  IF               ( count hex-addr tib-ptr )
        ROT 0 DO
            IHXBYTE
            >R                ( hex-addr hex-byte )
            2DUP C!
            DROP             ( hex-addr )
            1+               ( hex-addr+1 )
            R>
        LOOP
        (IHXBYTE)             ( hex-addr crc tib-ptr )
        DROP NIP             ( crc )
        ?IHXCRC = IF IHX-OK ELSE IHX-ERROR THEN
    ELSE
        DROP DROP DROP
        IHX-END
    THEN
    ;


: BUFFER:    ( u "<name>" -- ; -- addr )
   CREATE ALLOT
;

: CBUFFER:    ( u "<name>" -- ; -- addr )
   CREATE CHARS ALLOT
;

256 CONSTANT BUFFERSIZE

BUFFERSIZE CBUFFER: BUFFER

\ :07F00000EF7A535F1CE7C922
\ :00000001FF

: HEXLOAD
    ." HEXLOAD test" CR
    [CHAR] : EMIT
    BEGIN
        BUFFER DUP BUFFERSIZE ACCEPT
         CR .S
        DROP                             ( drop count )
        IHXREC
        DUP IHX-OK = IF
            [CHAR] # EMIT
        THEN
    DUP IHX-OK <> UNTIL
    DUP IHX-ERROR = IF
        ABORT" HEXLOAD ERROR"
    THEN
    ;


: (D.W)         ( d width --   width with leading 0's )
    1- DUP 1 < IF DROP 1 THEN <# 0 DO # LOOP #S #> ;

: (.W)         ( n width --   width with leading 0's )
    >R S>D R> (D.W) ;

: .W  (.W) TYPE SPACE ;

: D.R                       ( d width -- right align )
    >R SWAP OVER <# #S SIGN #>
    R> OVER - SPACES TYPE ;

: D.                        ( d --    output double )
    0 D.R SPACE ;

: .R                ( n width -- right align )
    >R S>D R> D.R ;

: PRINTABLE?         ( n - flag  is characte printable? )
    20 7F WITHIN ;

: (MEMDUMP)  ( addr u --      memory dump line )
    DUP >R
    OVER 4 .W ." : "
    0 DO DUP I + C@ 2 .W LOOP
    DROP R>
    0 DO DUP I + C@
        DUP PRINTABLE? INVERT IF DROP [CHAR] . THEN EMIT
    LOOP
    DROP
    ;

: MEMDUMP  ( a-addr u --      memory dump utility )
    OVER +               ( addr addr+u )
    SWAP                 ( addr+u  addr )
    DO
        I
        DUP 10 CR (MEMDUMP)
    10 +LOOP
    ;

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