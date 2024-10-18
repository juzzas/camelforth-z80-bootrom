0 VALUE fd-in
0 VALUE fd-out
256 CONSTANT max-block-line
CREATE block-line max-block-line ALLOT
1024 CONSTANT B/BLK
VARIABLE blk-ptr     0 blk-ptr !
VARIABLE blk-cur     0 blk-cur !
VARIABLE blk-offset  0 blk-offset !
VARIABLE verbose     0 verbose !

: open-input ( addr u -- )  R/O OPEN-FILE THROW TO fd-in ;
: open-output ( addr u -- )  W/O CREATE-FILE THROW TO fd-out ;
: close-input ( -- )  fd-in CLOSE-FILE THROW ;
: close-output ( -- )  fd-out CLOSE-FILE THROW ;
: generate-block 
   block-line max-block-line 32 FILL
   L/S 0 DO 
      block-line max-block-line fd-out WRITE-FILE THROW
   LOOP ;
: ?type ( c-addr u -- )  verbose @ IF TYPE CR ELSE 2DROP THEN ;

: GENERATE: ( #blks -- )
   BL PARSE open-output
   ?DUP IF  0 DO
      generate-block
   LOOP THEN
   close-output  ;  IMMEDIATE
: OPEN: BL PARSE USE  ; IMMEDIATE

: current-block ( -- )
   blk-cur @ BLOCK blk-ptr ! ;

: next-block  ( -- ) 
   blk-cur @ 1+ DUP blk-cur !
   BLOCK blk-ptr ! ;

: inc-offset  ( offset -- offset' )
   blk-offset @ 1+
   DUP 1023 > IF
      DROP next-block  0
   THEN 
   blk-offset !  ;



: (write-char) ( c -- )
   blk-ptr @ blk-offset @ + c!
   UPDATE inc-offset ;

: write-char ( c -- )
   current-block
   (write-char) ;

: write-chars ( c-addr u -- )
   current-block
   DUP IF 0 DO DUP C@ (write-char) 1+ LOOP DROP
   ELSE 2DROP  THEN  UPDATE ;

: pad-chars  ( c -- ) 
   ." padding from " blk-offset @ . CR
   current-block
   1024 blk-offset @ -  DUP IF
     0 DO DUP write-char LOOP
   THEN ." new offset " blk-offset @ . ;

: AT-BLOCK:  ( blk "filename" -- )
   DUP . ."  <-- " 
   BL PARSE
   2DUP TYPE CR
   open-input
   blk-cur !
   0 blk-offset !
   begin
      block-line max-block-line 32 FILL
      block-line max-block-line fd-in read-line throw
   while
      DROP block-line C/L 2DUP ?type write-chars
   REPEAT  DROP
   close-input ; IMMEDIATE

: AT-TEXT:  ( blk "filename" -- )
   DUP . ."  <-- " 
   BL PARSE
   2DUP TYPE CR
   open-input
   blk-cur !
   0 blk-offset !
   begin
      block-line max-block-line 32 FILL
      block-line max-block-line fd-in read-line throw
   while
      block-line SWAP 2DUP ?type write-chars
      13 write-char
   REPEAT  2DROP
   26 pad-chars
   close-input ; IMMEDIATE

