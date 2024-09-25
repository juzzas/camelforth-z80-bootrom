\ VT-100 compatible commands        jskists               0 / 4

This section implements ANSI/VT-100 style escape codes

.( Loading ANSI terminal definitions... )

VOCABULARY ANSI
ALSO ANSI DEFINITIONS



1 3 +THRU 
PREVIOUS DEFINITIONS



   \ VT-100 compatible commands                           1 / 4
0 CONSTANT BLACK    1 CONSTANT RED     2 CONSTANT GREEN
3 CONSTANT YELLOW   4 CONSTANT BLUE    5 CONSTANT MAGENTA
6 CONSTANT CYAN     7 CONSTANT WHITE   9 CONSTANT DEFAULT


VARIABLE BRIGHTNESS   0 BRIGHTNESS !

7 CONSTANT   BEL               27 CONSTANT  ESC

: BRIGHT  -1 BRIGHTNESS !  ;





   \ VT-100 compatible commands                           2 / 4
: VT-ESC  ESC EMIT ;           : BELL    BEL EMIT ;

: RESET  ( -- reset attributes )
    VT-ESC ." 0m" ;

: INVIS  ( -- make cursor invisible )
    VT-ESC ." ?25l" ;

: VIS  ( -- make cursor visible )
    VT-ESC ." [?25h" ;

: REVERSE  ( -- reverse attributes )
    VT-ESC ." 7m" ;


   \ VT-100 compatible commands                           3 / 4
: INK  ( colour -- change fg to colour 0-7 )
    BRIGHNESS @   IF 60 +  0 BRIGHTNESS !  THEN
        VT-ESC  30 + (.) TYPE ." m"
    ;

: PAPER  ( n -- change bg to n 0-7 )
    BRIGHNESS @ IF 60 +  0 BRIGHTNESS ! THEN
        VT-ESC  40 + (.) TYPE ." m"
    ;



