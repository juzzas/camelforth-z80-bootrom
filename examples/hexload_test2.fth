: /BYTESWAP   ( initialise nibswap code )
    HEXLOAD
    :06F00000EF78414FE7C963
    :00000001FF    ;

: BYTESWAP   ( u -- swaps bytes in u )
    [ HEX ] F000 CALL   ;



