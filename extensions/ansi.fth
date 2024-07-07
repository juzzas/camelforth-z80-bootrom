( VT-100 compatible commands                       1 / 3      )
7 CONSTANT   BEL               27 CONSTANT  ESC
: VT-ESC  ESC EMIT ;           : BELL    BEL EMIT ;

: RESET  ( -- reset attributes )
    VT-ESC ." 0m" ;

: INVIS  ( -- make cursor invisible )
    VT-ESC ." ?25l" ;

: VIS  ( -- make cursor visible )
    VT-ESC ." [?25h" ;

: REVERSE  ( -- reverse attributes )
    VT-ESC ." 7m" ;

( VT-100 compatible commands                       2 / 3      )
8 CONSTANT #COLOUR
: INK  ( n -- change fg to n 0-7 )
    DUP 0 #COLOURS WITHIN IF
        VT-ESC  30 + (.) TYPE ." m"
    THEN ;

: BRIGHT.INK  ( n -- change fg to n 0-7 )
    DUP 0 #COLOURS WITHIN IF
        VT-ESC  90 + (.) TYPE ." m"
    THEN ;





( VT-100 compatible commands                       3 / 3      )
: PAPER  ( n -- change bg to n 0-7 )
    DUP 0 #COLOURS WITHIN IF
        VT-ESC
        40 + (.) TYPE ." m"
    THEN ;

: BRIGHT.PAPER  ( n -- change bg to n 0-7 )
    DUP 0 #COLOURS WITHIN IF
        VT-ESC
        100 + (.) TYPE ." m"
    THEN ;



