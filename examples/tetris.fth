\ tt.pfe    Tetris for terminals, redone in ANSI-Forth.
\  Written 05Apr94 by Dirk Uwe Zoller,
\   e-mail duz@roxi.rz.fht-mannheim.de.
\ Look&feel stolen from Mike Taylor's "TETRIS FOR TERMINALS"
\
\ Please copy and share this program, modify it for your system
\ and improve it as you like. But don't remove this notice.
\
\   Thank you.
\

ONLY FORTH ALSO DEFINITIONS

VOCABULARY TETRIS  TETRIS ALSO DEFINITIONS

DECIMAL

\ Variables, constants

BL BL 2CONSTANT empty        \ an empty position
VARIABLE wiping         \ if true: wipe brick, else draw brick
2 CONSTANT col0        \ position of the pit
0 CONSTANT row0

10 CONSTANT wide        \ size of pit in brick positions
20 CONSTANT deep

CHAR J   VALUE left-key     \ customize if you don't like them
CHAR K   VALUE rot-key
CHAR L   VALUE right-key
BL       VALUE drop-key
CHAR P   VALUE pause-key
12       VALUE refresh-key
CHAR Q   VALUE quit-key

VARIABLE score
VARIABLE pieces
VARIABLE levels
VARIABLE delay

VARIABLE brow            \ where the brick is
VARIABLE bcol


\ stupid random number generator

: (XORSHIFT) ( n -- n    xorshift random number generator )
     DUP 7 LSHIFT XOR
     DUP 9 RSHIFT XOR
     DUP 8 LSHIFT XOR ;

VARIABLE SEED      42 SEED !
: RND  ( -- n    generate random 16bit value from seed )
     SEED @
     (XORSHIFT)
     DUP SEED ! ;

: RANDOM ( n -- n  generate random value between 0 and TOS )
     ( WARNING: Not evenly distributed but should be good )
     RND SWAP MOD ABS ;


\ Access pairs of characters in memory:

: 2C@        DUP 1+ C@ SWAP C@ ;
: 2C!        DUP >R C! R> 1+ C! ;

: D<>        D= 0= ;
: >=         < 0= ;
: <=         1- < ;

\ Drawing primitives:

: 2EMIT      EMIT EMIT ;

: position   \ row col --- ; cursor to the position in the pit
    2* col0 + SWAP row0 + AT-XY ;

: stone      \ c1 c2 --- ; draw or undraw these two characters
    wiping @ IF  2DROP 2 SPACES  ELSE  2EMIT    THEN ;


\ Define the pit where bricks fall into:

: def-pit    CREATE    wide deep * 2* ALLOT
    DOES>    ROT wide * ROT + 2* + ;

def-pit pit

: empty-pit    deep 0 DO wide 0 DO    empty J I pit 2C!
    LOOP LOOP ;


\ Displaying:

: draw-bottom    \ --- ; redraw the bottom of the pit
    deep -1 position
    [CHAR] + DUP stone
    wide 0 DO  [CHAR] = DUP stone  LOOP
    [CHAR] + DUP stone ;

: draw-frame    \ --- ; draw the border of the pit
    deep 0 DO
     I -1   position [CHAR] | DUP stone
     I wide position [CHAR] | DUP stone
    LOOP    draw-bottom ;

: bottom-msg    \ addr cnt --- ; output a msg at pit bottom
    deep OVER 2/ wide SWAP - 2/ position TYPE ;

: draw-line    \ line ---
    DUP 0 position    wide 0 DO  DUP I pit 2C@ 2EMIT
                             LOOP    DROP ;

: draw-pit    \ --- ; draw the contents of the pit
    deep 0 DO  I draw-line    LOOP ;

: show-key    \ char --- ; visualization of that character
    DUP BL <
    IF  [CHAR] @ OR  [CHAR] ^ EMIT    EMIT    SPACE
    ELSE    [CHAR] ` EMIT    EMIT    [CHAR] ' EMIT
    THEN ;

: show-help    \ --- ; display some explanations
    30  1 AT-XY ." ***** T E T R I S *****"
    30  2 AT-XY ." ======================="
    30  4 AT-XY ." Use keys:"
    32  5 AT-XY left-key    show-key ."  Move left"
    32  6 AT-XY rot-key     show-key ."  Rotate"
    32  7 AT-XY right-key   show-key ."  Move right"
    32  8 AT-XY drop-key    show-key ."  Drop"
    32  9 AT-XY pause-key   show-key ."  Pause"
    32 10 AT-XY refresh-key show-key ."  Refresh"
    32 11 AT-XY quit-key    show-key ."  Quit"
    32 13 AT-XY ." -> "
    30 16 AT-XY ." Score:"
    30 17 AT-XY ." Pieces:"
    30 18 AT-XY ." Levels:"  ;

: update-score    \ --- ; display current score
    38 16 AT-XY score @ 3 .R
    38 17 AT-XY pieces @ 3 .R
    38 18 AT-XY levels @ 3 .R ;

: refresh    \ --- ; redraw everything on screen
    PAGE draw-frame draw-pit show-help update-score ;


\ Define shapes of bricks:

: def-brick    CREATE    4 0 DO
     ' EXECUTE    0 DO    DUP I CHARS + C@ C,    LOOP DROP
     REFILL DROP
    LOOP
    DOES>    ROT 4 * ROT + 2* + ;

def-brick brick1   S"         "
                   S" ######  "
                   S"   ##    "
                   S"         "

def-brick brick2   S"         "
                   S" <><><><>"
                   S"         "
                   S"         "

def-brick brick3   S"         "
                   S"   {}{}{}"
                   S"   {}    "
                   S"         "

def-brick brick4   S"         "
                   S" ()()()  "
                   S"     ()  "
                   S"         "

def-brick brick5   S"         "
                   S"   [][]  "
                   S"   [][]  "
                   S"         "

def-brick brick6   S"         "
                   S" @@@@    "
                   S"   @@@@  "
                   S"         "

def-brick brick7   S"         "
                   S"   %%%%  "
                   S" %%%%    "
                   S"         "

\ this brick is actually in use:

def-brick brick    S"         "
                   S"         "
                   S"         "
                   S"         "

def-brick scratch  S"         "
                   S"         "
                   S"         "
                   S"         "

CREATE bricks   ' brick1 ,   ' brick2 ,   ' brick3 ,
   ' brick4 ,   ' brick5 ,    ' brick6 ,    ' brick7 ,

CREATE brick-val 1 C, 2 C, 3 C, 3 C, 4 C, 5 C, 5 C,


: is-brick    \ brick --- ; activate a shape of brick
    >BODY ['] brick >BODY 32 CMOVE ;

: new-brick    \ --- ; select a new brick by random, count it
    1 pieces +!  7 RANDOM
    bricks OVER CELLS + @ is-brick
    brick-val SWAP CHARS + C@ score +! ;

: rotleft    4 0 DO 4 0 DO
     J I brick 2C@  3 I - J scratch 2C!
    LOOP LOOP
    ['] scratch is-brick ;

: rotright    4 0 DO 4 0 DO
     J I brick 2C@  I 3 J - scratch 2C!
    LOOP LOOP
    ['] scratch is-brick ;

: draw-brick    \ row col ---
    4 0 DO 4 0 DO
     J I brick 2C@  empty D<>
     IF    OVER J + OVER I +  position
    J I brick 2C@    stone
     THEN
    LOOP LOOP  2DROP ;

: show-brick    FALSE wiping !  draw-brick ;
: hide-brick    TRUE wiping !   draw-brick ;

: put-brick    \ row col --- ; put the brick into the pit
    4 0 DO 4 0 DO
     J I brick 2C@  empty D<>
     IF    OVER J +  OVER I +  pit
    J I brick 2C@    ROT 2C!
     THEN
    LOOP LOOP  2DROP ;

: remove-brick    \ row col --- ; remove the brick from posn.
    4 0 DO 4 0 DO
     J I brick 2C@  empty D<>
     IF    OVER J + OVER I + pit empty ROT 2C!  THEN
    LOOP LOOP  2DROP ;

: test-brick    \ row col --- flag ; could the brick be there?
    4 0 DO 4 0 DO
     J I brick 2C@ empty D<>
     IF    OVER J +  OVER I +
    OVER DUP 0< SWAP deep >= OR
    OVER DUP 0< SWAP wide >= OR
    2SWAP pit 2C@    empty D<>
    OR OR IF  UNLOOP UNLOOP 2DROP FALSE  EXIT  THEN
     THEN
    LOOP LOOP  2DROP TRUE ;

: move-brick    \ rows cols --- flag ; try to move the brick
    brow @ bcol @ remove-brick
    SWAP brow @ + SWAP bcol @ + 2DUP test-brick
    IF  brow @ bcol @ hide-brick
     2DUP bcol ! brow !    2DUP show-brick put-brick    TRUE
    ELSE    2DROP brow @ bcol @ put-brick  FALSE
    THEN ;

: rotate-brick    \ flag --- flag ; left/right, success
    brow @ bcol @ remove-brick
    DUP IF    rotright  ELSE    rotleft    THEN
    brow @ bcol @ test-brick
    OVER IF    rotleft    ELSE    rotright  THEN
    IF  brow @ bcol @ hide-brick
     IF    rotright  ELSE    rotleft    THEN
     brow @ bcol @ put-brick
     brow @ bcol @ show-brick    TRUE
    ELSE    DROP FALSE    THEN ;

: insert-brick    \ row col --- flag ; introduce a new brick
    2DUP test-brick
    IF  2DUP bcol ! brow !
     2DUP put-brick  draw-brick    TRUE
    ELSE    FALSE  THEN ;

: drop-brick    \ --- ; move brick down fast
    BEGIN  1 0 move-brick 0=  UNTIL ;

: move-line    \ from to ---
    OVER 0 pit    OVER 0 pit    wide 2*    CMOVE    draw-line
    DUP 0 pit  wide 2*  BLANK    draw-line ;

: line-full    \ line-no --- flag
    TRUE    wide 0
    DO  OVER I pit 2C@ empty D=
     IF    DROP FALSE    LEAVE  THEN
    LOOP NIP ;

: remove-lines    \ ---
    deep deep
    BEGIN
     SWAP
     BEGIN  1- DUP 0< IF  2DROP EXIT  THEN  DUP line-full
     WHILE  1 levels +!    10 score +!  REPEAT
     SWAP 1-
     2DUP <> IF    2DUP move-line    THEN
    AGAIN ;

: to-upper    \ char --- char ; convert to upper case
    DUP [CHAR] a >= OVER [CHAR] z <= AND IF  BL -  THEN ;

: interaction    \ --- flag
    KEY to-upper
    CASE
     left-key    OF  0 -1 move-brick DROP  ENDOF
     right-key    OF  0  1 move-brick DROP  ENDOF
     rot-key    OF  0 rotate-brick DROP  ENDOF
     drop-key    OF  drop-brick    ENDOF
     pause-key    OF  S"    paused " bottom-msg    KEY DROP
     draw-bottom  ENDOF
     refresh-key    OF  refresh  ENDOF
     quit-key    OF  FALSE EXIT    ENDOF
    ENDCASE    TRUE ;

: initialize    \ --- ; prepare for playing
    randomize empty-pit refresh
    0 score !  0 pieces !  0 levels !  100 delay ! ;

: adjust-delay    \ --- ; make it faster with increasing score
    levels @
    DUP  50 < IF  100 OVER -  ELSE
    DUP 100 < IF    62 OVER 4 / -    ELSE
    DUP 500 < IF    31 OVER 16 / -  ELSE  0    THEN THEN THEN
    delay !    DROP ;

: play-game    \ --- ; play one tetris game
    BEGIN
     new-brick
     -1 3 insert-brick
    WHILE
     BEGIN  4 0
    DO  35 13 AT-XY
     delay @ MS KEY?
     IF interaction 0=
      IF  UNLOOP EXIT  THEN
     THEN
    LOOP
    1 0 move-brick    0=
     UNTIL
     remove-lines
     update-score
     adjust-delay
    REPEAT ;

FORTH DEFINITIONS

: tt        \ --- ; play the tetris game
    initialize
    S"  Press any key " bottom-msg KEY DROP draw-bottom
    BEGIN
     play-game
     S"    Again? " bottom-msg KEY to-upper [CHAR] Y =
    WHILE  initialize  REPEAT
    0 23 AT-XY CR ;

ONLY FORTH ALSO DEFINITIONS
