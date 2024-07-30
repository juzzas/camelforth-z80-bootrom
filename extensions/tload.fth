( tloader - load text files embedded into blocks          1 / n)

VARIABLE tload-block
VARIABLE tload-block-num
VARIABLE tload-index
-1 CONSTANT TRUE
0 CONSTANT FALSE









( tloader - load text files embedded into blocks          1 / n)
: tload-refill  ( -- flag )
    tload-index @ 16 = IF 
        -1 tload-block-num +!
        0 tload-index !
        1 BLK +!   THEN

    tload-block-num @ 0= IF 0 EXIT THEN

    BLK @ BLOCK    ( addr )
    tload-index @ C/L * +  ( index )
        C/L 'SOURCE 2!

    0 >IN !
    1 tload-index  +! TRUE    ;

: (TLOAD)  ( #blk -- )
   tload-block-num !
   0 tload-index !
   BEGIN
     REFILL  IF
       SOURCE TYPE  CR  INTERPRET
     ELSE   EXIT
     THEN
   AGAIN  ;





: TLOAD ( blk #blk -- )
   REFILLVEC @ >R
   SWAP BLK !
   ['] tload-refill REFILLVEC !
   SAVE-INPUT
   (TLOAD)
   RESTORE-INPUT
   R> REFILLVEC !
   ;
