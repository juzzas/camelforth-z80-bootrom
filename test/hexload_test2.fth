303 LOAD \ load tester
: /BYTESWAP   ( initialise nibswap code )
    HEXLOAD :06F00000EF78414FE7C963
            :00000001FF    
    ;HEXLOAD   ;

CR .( TESTING hexload compiling... )
T{  /BYTESWAP -> $F000 6 }T

: BYTESWAP   ( u -- swaps bytes in u )
    [ HEX ] F000 CALL   ;

CR .( TESTING hexload execute... )
T{  $1234 BYTESWAP -> $3412 }T

