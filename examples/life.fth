\ Implementation of Conway's game of life in Forth.
\ See http://en.wikipedia.org/wiki/Conway's_Game_of_Life

\ constants for board height and width
24 CONSTANT height
32 CONSTANT width
height width * CONSTANT size

\ allocate two arrays to hold current and next generations
CREATE gen_curr size ALLOT
CREATE gen_next size ALLOT

\ iterators and their associated operators
VARIABLE row
VARIABLE col

: >=  1- U> ;
: <=  U> 0= ;

\ Sets the row offset to zero
: rowFirst ( -- ) 0 row ! ;

\ Advances the offset by the width.
: rowNext ( -- )
    width row +! ;

\ At end if current offset exceeds array size.
: rowAtEnd?
    row @ size >= ;

\ Iterator used to apply a function to the rows.
: rowForEach ( xt -- )
    rowFirst
    BEGIN
        DUP EXECUTE rowNext rowAtEnd?
    UNTIL
    DROP ;

\ Returns index of the row after current using wrap around.
: row+ ( -- index )
    row @ width + size MOD ;

\ Returns index of the column before current using wrap around.
: row- ( -- index )
    row @ width - size MOD ;

: colFirst 0 col ! ;

: colNext
    1 col +! ;

: colAtEnd?
    col @ width >= ;

: colForEach ( xt -- )
    colFirst
    BEGIN
        DUP EXECUTE colNext colAtEnd?
    UNTIL
    DROP ;

\ Returns index of the column after current using wrap around.
: col+ ( -- index )
    col @ 1 + width MOD ;

\ Returns index of the column before current using wrap around.
: col- ( -- index )
    col @ 1 - width MOD ;

\ moves bytes from next gen to current.
: moveCurr ( -- )
    gen_next gen_curr size MOVE ;

\ clears curr array to clear out junk in ram
: currErase ( -- )
    gen_curr size ERASE ;

\ retrieve a cell value from the current generation
: curr@ ( col row -- n )
    + gen_curr + C@ ;

\ stores a value into a cell from the current generation
: curr! ( n col row -- )
    + gen_curr + C! ;

\ Parses a pattern string into current board.
\ This function is unsafe and will over write memory.
: >curr ( addr count -- )
    currErase
    rowFirst colFirst
    BOUNDS
    DO
        DUP C@
        DUP '|' <> IF
            BL <> 1 AND
            col @ row @ curr!
            colNext
        ELSE
            DROP
            rowNext
            colFirst
        THEN
    LOOP
    DROP ;

: .cell ( -- )
    col @ row @ curr@
    IF '*' ELSE '.' THEN
    EMIT ;

\ prints the row from the current generation to output
: .currRow ( -- )
    CR ['] .cell colForEach ;

\ Prints the current board generation to standard output
: .curr
    ['] .currRow rowForEach
    CR ;

\ retrieve a cell value from the next generation
: next@ ( col row -- n )
    + gen_next + C@ ;

\ stores a cell into the next generation
: next! ( n col row -- )
    + gen_next + C! ;

\ computes the sum of the neigbors of the current cell.
: calcSum ( -- n )
   col-  row-  curr@
   col @ row-  curr@ +
   col+  row-  curr@ +
   col-  row @ curr@ +
   col+  row @ curr@ +
   col-  row+  curr@ +
   col @ row+  curr@ +
   col+  row+  curr@ + ;

: calcCell ( -- )
    calcSum

    \ Unless explicitly marked live, all cells die in the next generation.
    \ There are two rules we'll apply to mark a cell live.

    \ Is the current cell dead?
    col @ row @ curr@ 0=
    IF
        \ Any dead cell with three live neighbours becomes a live cell.
        3 =
    ELSE
        \ Any live cell with two or three live neighbours survives.
        DUP 2 >= SWAP 3 <= AND
    THEN
    1 AND
    col @ row @ next! ;

: calcRow ( row -- )
    ['] calcCell colForEach ;

: calcGen ( -- )
    ['] calcRow rowForEach
    moveCurr ;

: life ( -- )
    PAGE
    BEGIN calcGen 0 0 AT-XY .curr KEY? UNTIL ;

\ Test cases taken from Rosetta code's implementation
: blinker S" |***" >curr ;
: toad S" ***| ***" >curr ;
: pentomino S" **| **| *" >curr ;
: pi S" **| **|**" >curr ;
: glider S"  *|  *|***" >curr ;
: pulsar S" *****|*   *" >curr ;
: ship S"  ****|*   *|    *|   *" >curr ;
: pentadecathalon S" **********" >curr ;
: clock S"  *|  **|**|  *" >curr ;

