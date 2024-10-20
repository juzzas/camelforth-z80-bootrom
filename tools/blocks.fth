0 VALUE fd-in
0 VALUE fd-out
0 VALUE blk-out
256 CONSTANT max-block-line
CREATE block-line max-block-line ALLOT
1024 CONSTANT B/BLK

VARIABLE   blk-ptr
VARIABLE   blkfile-dirty

\ start of BLKFILE extension
BEGIN-STRUCTURE BLKFILE-CONTEXT
   FIELD: blk-origin
   FIELD: blk-cur
   FIELD: blk-offset
END-STRUCTURE

CREATE blkfile BLKFILE-CONTEXT ALLOT

: set-dirty  ( -- )
   -1 blkfile-dirty !  ;

: clear-dirty  ( -- )
   0 blkfile-dirty !  ;

: is-dirty?  ( -- )
   blkfile-dirty @  ;

: current-block ( blkfile-id -- )
   is-dirty? IF UPDATE clear-dirty THEN
   blk-cur @ BLOCK blk-ptr ! ;

: inc-block  ( blkfile-id -- ) 
   1 OVER blk-cur +!   ( blkfile-id )
   0 OVER blk-offset ! ( blkfile-id )
   current-block ;

: inc-offset  ( blkfile-id -- )
   1 OVER blk-offset +! ( blkfile-id )
   DUP blk-offset @ 1023 > IF
      inc-block
   ELSE  DROP  THEN   ;

: (write-char) ( c blkfile-id -- )
   SWAP OVER blk-ptr @  ( blkfile-id c blkfile-id blk-ptr )
   SWAP blk-offset @ + c!   ( blkfile-id )
   inc-offset ;

: write-char ( c blkfile-id )
   DUP current-block
   set-dirty
   (write-char) ;

: write-chars ( c-addr u blkfile-id -- )
   OVER IF
     DUP current-block  ( c-addr u blkfile-id )
     set-dirty
     SWAP 0 DO   ( c-addr blkfile-id )
       OVER I + C@  ( c-addr blkfile-id c )
       OVER (write-char) ( c-addr blkfile-id )
     LOOP
   ELSE
     DROP
   THEN  2DROP  ;

: (read-char) ( blkfile-id -- c )
   blk-ptr @  ( blkfile-id blk-ptr )
   blk-offset @ + c@   ( c )
   inc-offset ;

: read-char ( blkfile-id -- c )
   DUP current-block
   (read-char) ;

: read-chars ( c-addr u blkfile-id -- )
   OVER IF
     DUP current-block  ( c-addr u blkfile-id )
     SWAP 0 DO   ( c-addr blkfile-id )
       2DUP (read-char)  ( c-addr blkfile-id c-addr c )
       SWAP I + C!  ( c-addr blkfile-id )
     LOOP
   ELSE
     DROP
   THEN  2DROP  ;



: (open-blkfile) ( blk blkfile-id -- blkfile-id )
   SWAP OVER 2DUP ( blkfile-id blk blkfile-id blk blkfile-id )
   blk-cur !  blk-origin !  ( blkfile-id )
   clear-dirty
   0 OVER blk-offset ! ;     ( blkfile-id )


: open-blkfile ( blk -- blkfile-id )
   blkfile (open-blkfile) ;

: close-blkfile ( blkfile-id -- )
   is-dirty? IF UPDATE clear-dirty THEN
   FLUSH ;



: blkfile-size ( blkfile-id -- #blks )
   DUP blk-cur @ SWAP blk-origin @ - 1+ ;
\ end of BLKFILE extension


: pad-chars  ( c blkfile-id -- )
   \ ." padding from " DUP blk-offset @ . CR
   DUP current-block
   set-dirty
   1024 OVER blk-offset @ -  DUP IF  ( c blkfile-id )
     0 DO 2DUP (write-char) LOOP
   THEN
   \ ." new offset " DUP blk-offset @ .
   2DROP ;

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


: AT-BLOCK:  ( blk "filename" -- )
   DUP . ." <-- " 
   BL PARSE
   2DUP TYPE ."  ["
   open-input
   R/W BIN open-blkfile TO blk-out
   begin
      block-line max-block-line 32 FILL
      block-line max-block-line fd-in read-line throw
   while
      DROP block-line C/L 2DUP ?type blk-out write-chars
   REPEAT  DROP
   blk-out blkfile-size . ." blocks]" CR
   blk-out close-blkfile
   close-input ; IMMEDIATE

: AT-TEXT:  ( blk "filename" -- )
   DUP . ." <-- "
   BL PARSE
   2DUP TYPE ."  ["
   open-input
   R/W open-blkfile TO blk-out
   begin
      block-line max-block-line 32 FILL
      block-line max-block-line fd-in read-line throw
   while
      block-line SWAP 2DUP ?type blk-out write-chars
      13 blk-out write-char
   REPEAT  2DROP
   26 blk-out pad-chars
   blk-out blkfile-size . ." blocks]" CR
   blk-out close-blkfile

   close-input ; IMMEDIATE

