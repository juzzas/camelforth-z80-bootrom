: MARKER  ( "name" -- )
    CREATE LATEST @ ,
    DOES> @ LATEST !
    ;

: BSAVE   ( c-addr u blk -- )
    ;

: BLOAD   ( blk c-addr u -- )
    ;
