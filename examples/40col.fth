VARIABLE SEED

: ESC  ( -- emit escape character )
    27 EMIT ;

: CLS  ( -- clear screen )
    ESC ." [2J" ;

: RESET  ( -- reset attributes )
    ESC ." [0m" ;

: (.)  ( n -- addr c   convert value to string)
    S>D <# #S #> ;

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

: UDG  ( n addr --   send UDG ESC sequence to Pico )
    SWAP
    ESC ." [?"
    (.) TYPE
    ." U"
    8 0 DO
        DUP I +
        C@ EMIT
    LOOP ;



CREATE TRACK 255 C, 0 C, 0 C, 0 C, 0 C, 0 C, 0 C, 0 C,
CREATE TRAIN 4 C, 242 C, 18 C, 31 C, 33 C, 33 C, 255 C, 102 C,
CREATE CARRIAGE 0 C, 254 C, 130 C, 170 C, 170 C, 138 C, 255 C, 108 C,

: SHOW-TRACK  ( line --   show the track )
    0 SWAP        ( 0 line -- )
    AT
    40 0 DO
        130 EMIT
    LOOP ;

: SHOW-TRAIN  ( col line  --  show the train on the OLED screen at pointer in buffer )
    AT                ( print at col, row )
    BL EMIT
    129 EMIT
    129 EMIT
    129 EMIT
    128 EMIT
    ;

: SHOW-SCENE  ( c --  show the scene with train at column c )
    11 SHOW-TRACK
    10 SHOW-TRAIN                ( -- )
    ;

: DELAY   ( --   delay 10000 cycles )
    10000 0 DO LOOP ;


: RUN
    CLS
    INVIS

    128 TRAIN UDG
    129 CARRIAGE UDG
    130 TRACK UDG

    32 0 DO
        I SHOW-SCENE
        DELAY
    LOOP

    30 12 AT
    ." Choo-choo!"

    0 24 AT
    VIS
    RESET
    ;
