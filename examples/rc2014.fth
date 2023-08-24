VARIABLE SEED

: XORSHIFT ( n -- n   xorshift random number generator )
    DUP 7 LSHIFT XOR
    DUP 9 RSHIFT XOR
    DUP 8 LSHIFT XOR ;

: RND  ( -- n   generate random 16bit value from seed )
    SEED @
    XORSHIFT
    DUP SEED ! ;

: RANDOM (  n -- n  generate random value between 0 and value on stack )
    ( WARNING: Not evenly distributed but should be good )
    RND SWAP MOD ABS ;

: (.)  ( n -- addr c   convert value to string)
    S>D <# #S #> ;

: ESC  ( -- emit escape character )
    27 EMIT ;

: CLS  ( -- clear screen )
    ESC ." [2J" ;

: RESET  ( -- reset attributes )
    ESC ." [0m" ;

VARIABLE 256COLOURS    ( 0 = 8 color, 1 = 256 color support )
0 256COLOURS !

: INK16  ( n -- change fg to n 0-15 )
    DUP 0 8 WITHIN IF
        ESC ." ["
        30 + (.) TYPE ." m"
    ELSE
        DUP 8 16 WITHIN IF
            ESC ." ["
            82 + (.) TYPE ." m"
        ELSE
            DROP
        THEN
    THEN ;

: INK256  ( n -- change fg to n 0-255 )
    DUP 0 256 WITHIN IF
        ESC ." [38;5;"
        (.) TYPE ." m"
    ELSE
        DROP
    THEN ;

: INK  ( n -- change fg to n 0-15, or 0-255 if 256COLOURS is set )
    256COLOURS @ IF
        INK256
    ELSE
        INK16
    THEN ;

: PAPER16  ( n -- change fg colour to n 0-15 )
    DUP 0 8 WITHIN IF
        ESC ." ["
        40 + (.) TYPE ." m"
    ELSE
        DUP 8 16 WITHIN IF
            ESC ." ["
            92 + (.) TYPE ." m"
        ELSE
            DROP
        THEN
    THEN ;

: PAPER256  ( n -- change bg colour to n 0-255 )
    DUP 0 256 WITHIN IF
        ESC ." [48;5;"
        (.) TYPE ." m" ;
    ELSE
        DROP
    THEN ;

: PAPER  ( n -- change bg colour to n 0-15, or 0-255 if 256COLOURS is set )
    256COLOURS @ IF
        PAPER256
    ELSE
        PAPER16
    THEN ;

: AT  ( x y -- move cursor to x,y )
    ESC ." ["
    1+ (.) TYPE
    ." ;"
    1+ (.) TYPE
    ." H" ;

: INVIS  ( -- make cursor invisible )
    ESC ." [?25l" ;

: VIS  ( -- make cursor visible )
    ESC ." [?25h" ;


VARIABLE MAX-X  74
74 MAX-X !

: RC2014  ( -- print RC2014 at random location with random attributes )
    MAX-X @ RANDOM 24 RANDOM AT    ( to fit "RC2014" for MAX-X columns and 24 lines )
    16 RANDOM INK
    16 RANDOM PAPER
    ." RC2014" ;

: RUN  ( set up and run the demo )
    CLS
    INVIS
    100 0 DO
        RC2014
    LOOP
    RESET
    VIS ;

: RUN80  ( setup and run the demo for 80 column terminal )
    74 MAX-X !              ( 80 column display)
    0 256COLOURS !           ( 16 colours )
    RUN ;

: RUN40  ( setup and run the demo for 40 column Pico terminal )
    34 MAX-X !              ( 40 column display)
    1 256COLOURS !           ( 256 colours )
    RUN ;
