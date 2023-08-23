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

: INK  ( n -- change ink to n 0-7 )
    ESC ." ["
    30 + (.) TYPE
    ." m" ;

: PAPER  ( n -- change paper to n 0-7 )
    ESC ." ["
    40 + (.) TYPE
    ." m" ;

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

: RC2014  ( -- print RC2014 at random location with random attributes )
    72 RANDOM 24 RANDOM AT
    8 RANDOM INK
    8 RANDOM PAPER
    ." RC2014" ;

: RUN  ( print 10 random numbers between 0 and 9 )
    CLS
    INVIS
    100 0 DO
        RC2014
    LOOP
    RESET
    VIS ;
