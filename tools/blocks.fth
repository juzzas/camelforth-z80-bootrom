0 VALUE fd-in
0 VALUE fd-out
0 VALUE blk-out
256 CONSTANT max-block-line
CREATE block-line max-block-line ALLOT
1024 CONSTANT B/BLK

1 CONSTANT R/O
2 CONSTANT W/O
3 CONSTANT R/W

: BIN  ( fam -- fam' )
    32768 +  ;   \ set most significant bit 

\ start of BLKFILE extension
BEGIN-STRUCTURE BLKFILE-CONTEXT
   FIELD: blk-origin
   FIELD: blk-ptr
   FIELD: blk-cur
   FIELD: blk-offset
   FIELD: blkfile-dirty
   FIELD: blkfile-fam
END-STRUCTURE

CREATE blkfile BLKFILE-CONTEXT ALLOT

: set-dirty  ( blkfile-id -- )
   -1 SWAP blkfile-dirty !  ;

: is-dirty?  ( blkfile-id -- )
   blkfile-dirty @  ;

: current-block ( blkfile-id -- )
   DUP blk-cur @ BLOCK SWAP blk-ptr ! ;

: inc-block  ( blkfile-id -- ) 
   1 OVER blk-cur +!   ( blkfile-id )
   0 OVER blk-offset ! ( blkfile-id )
   DUP is-dirty? IF UPDATE THEN
   current-block ;

: inc-offset  ( blkfile-id -- )
   1 OVER blk-offset +! ( blkfile-id )
   DUP blk-offset @ 1023 > IF
      inc-block
   ELSE  DROP  THEN   ;

: (write-char) ( c blkfile-id -- )
   SWAP OVER DUP blk-ptr @  ( blkfile-id c blkfile-id blk-ptr )
   SWAP blk-offset @ + c!   ( blkfile-id )
   inc-offset ;

: write-char ( c blkfile-id )
   DUP current-block
   DUP set-dirty
   (write-char) ;

: write-chars ( c-addr u blkfile-id -- )
   OVER IF
     DUP current-block  ( c-addr u blkfile-id )
     DUP set-dirty
     SWAP 0 DO   ( c-addr blkfile-id )
       OVER I + C@  ( c-addr blkfile-id c )
       OVER (write-char) ( c-addr blkfile-id )
     LOOP
   ELSE
     DROP
   THEN  2DROP  ;

: pad-chars  ( c blkfile-id -- )
   \ ." padding from " DUP blk-offset @ . CR
   DUP current-block
   DUP set-dirty
   1024 OVER blk-offset @ -  DUP IF  ( c blkfile-id )
     0 DO 2DUP (write-char) LOOP
   THEN
   \ ." new offset " DUP blk-offset @ .
   2DROP ;


: (open-blkfile) ( blk fam blkfile-id -- blkfile-id )
   SWAP OVER blkfile-fam !
   SWAP OVER 2DUP  blk-cur !  blk-origin !
   0 OVER blk-ptr !
   0 OVER blk-offset !
   0 OVER blkfile-dirty ! ;


: open-blkfile ( blk fam -- blkfile-id )
   blkfile (open-blkfile) ;

: close-blkfile ( blkfile-id -- )
   DUP is-dirty? IF UPDATE THEN
   FLUSH ;



: blkfile-size ( blkfile-id -- #blks )
   DUP blk-cur @ SWAP blk-origin @ - 1+ ;
\ end of BLKFILE extension

VARIABLE verbose     0 verbose !
VARIABLE blkpos


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

: blkpos+!  blkpos @ +   blkpos !   ;

\ increment to next divisible by 3
: blkpos+3!  blkpos @ 3 +  DUP 3 MOD -  blkpos !   ;

: GENERATE: ( #blks -- )
   BL PARSE open-output
   ?DUP IF  0 DO
      generate-block
   LOOP THEN
   close-output  
   0 blkpos ! ;  IMMEDIATE

\ : OPEN: BL PARSE USE  ; IMMEDIATE
: OPEN: USE  ; IMMEDIATE

: .blkfile-info  ( blkfile-id  -- )
    DUP blk-origin @ . ." ["
    DUP blkfile-size . ." blocks]" CR ;


: (+blocks)    ( addr u  -- )
   2DUP TYPE ."  --> "
   open-input
   blkpos @ R/W BIN open-blkfile TO blk-out
   begin
      block-line max-block-line 32 FILL
      block-line max-block-line fd-in read-line throw
   while
      DROP block-line C/L 2DUP ?type blk-out write-chars
   REPEAT  DROP
   blk-out .blkfile-info
   blk-out close-blkfile
   blkfile-size  blkpos+!
   close-input 
;

: BLOCKS:   ( blk "filename" -- )
    blkpos !  BL PARSE (+blocks)   ; IMMEDIATE

: +BLOCKS:  (  "filename" -- )
    blkpos+3! BL PARSE (+blocks)   ; IMMEDIATE

: (+blkfile)  ( addr u -- )
   2DUP TYPE ."  --> "
   open-input
   blkpos @ R/W open-blkfile TO blk-out
   blk-out
   begin
      block-line max-block-line 32 FILL
      block-line max-block-line fd-in read-line throw
   while
      block-line SWAP 2DUP ?type blk-out write-chars
      13 blk-out write-char
   REPEAT  DROP
   26 blk-out pad-chars
   blk-out .blkfile-info
   blk-out close-blkfile

   blkfile-size  blkpos+!

   close-input 
 ; 

: BLKFILE:  ( blk "filename" -- )
    blkpos !    BL PARSE (+blkfile)  ; IMMEDIATE

: +BLKFILE:  ( "filename" -- )
    blkpos+3!   BL PARSE (+blkfile)  ; IMMEDIATE

