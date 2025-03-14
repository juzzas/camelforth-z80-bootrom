\ The fast wrapping requires dimensions that are powers of 2.
 1 6 LSHIFT CONSTANT w \ 64
 1 4 LSHIFT CONSTANT h \ 16
 
 : rows    w * 2* ;
 1 rows CONSTANT row
 h rows CONSTANT size
 
 CREATE world size ALLOT
 world   VALUE old
 old w + VALUE new
 
 VARIABLE gens
 : clear  world size ERASE     0 gens ! ;
 : age  new old TO new TO old  1 gens +! ;
 
 : col+  1+ ;
 : col-  1- DUP w AND + ; \ avoid borrow into row
 : row+  row + ;
 : row-  row - ;
 : wrap ( i -- i ) [ size w - 1- ] LITERAL AND ;
 : w@ ( i -- 0/1 ) wrap old + C@ ;
 : w! ( 0/1 i -- ) wrap old + C! ;
 
 : foreachrow ( xt -- )
   size 0 DO  I OVER EXECUTE  row +LOOP DROP ;
 
 : showrow ( i -- ) CR
   old + w OVER + SWAP DO I C@ IF [CHAR] * ELSE BL THEN
   EMIT LOOP ;
 : show  ['] showrow foreachrow  CR ." Generation "
    gens @ . ;
 
 : sum-neighbors ( i -- i n )
   DUP  col- row- w@
   OVER      row- w@ +
   OVER col+ row- w@ +
   OVER col-      w@ +
   OVER col+      w@ +
   OVER col- row+ w@ +
   OVER      row+ w@ +
   OVER col+ row+ w@ + ;
 : gencell ( i -- )
   sum-neighbors  OVER old + C@
   OR 3 = 1 AND   SWAP new + C! ;
 : genrow ( i -- )
   w OVER + SWAP DO I gencell LOOP ;
 : gen  ['] genrow foreachrow  age ;
 
 : life  BEGIN gen 0 0 AT-XY show KEY? UNTIL ;

 \ patterns
 : pat ( i addr len -- )
   ROT DUP 2SWAP  OVER + SWAP DO
     I C@ '|' = IF DROP row+ DUP ELSE
     I C@ BL  = 1+ OVER w!  col+ THEN
   LOOP 2DROP ;
 
 : blinker S" ***" pat ;
 : toad S" ***| ***" pat ;
 : pentomino S" **| **| *" pat ;
 : pi S" **| **|**" pat ;
 : glider S"  *|  *|***" pat ;
 : pulsar S" *****|*   *" pat ;
 : ship S"  ****|*   *|    *|   *" pat ;
 : pentadecathalon S" **********" pat ;
 : clock S"  *|  **|**|  *" pat ;
