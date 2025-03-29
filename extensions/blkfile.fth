( blkfile - extension to treat blocks as files          0 / n)

CR .( Loading blkfile... )




ONLY FORTH DEFINITIONS   ALSO SYSTEM
1 11 +THRU
/BLKFILE  ONLY FORTH






   ( blkfile - extension to treat blocks as files       1 / n)
\ blkfile structure
: blkfile.flags  ( blkfile -- addr )      ;
: blk.origin  ( blkfile -- addr )    2 +  ;
: blk.cur  ( blkfile -- addr )       4 +  ;
: blk.offset  ( blkfile -- addr )    6 +  ;
: blk.fence  ( blkfile -- addr )     8 +  ;
10 CONSTANT BLKFILE-CONTEXT

4 CONSTANT #BLKFILE
256 CONSTANT BLKFILE-BUFFER-SIZE
CREATE blkfiles BLKFILE-CONTEXT #BLKFILE * ALLOT
CREATE blkfile-buffer BLKFILE-BUFFER-SIZE ALLOT
$8000 CONSTANT flag.open
$0001 CONSTANT flag.binary
$0002 CONSTANT flag.readable
$0004 CONSTANT flag.writable

flag.binary CONSTANT BIN
flag.readable CONSTANT R/O
flag.readable flag.writable +  CONSTANT R/W

0 VALUE 'blkfile


   ( blkfile - extension to treat blocks as files       2 / n)
: i>blkfile  ( n -- blkfileid ) 
   BLKFILE-CONTEXT *  blkfiles + ;
: /BLKFILE    \ initialise BLKFILEs
   #BLKFILE 0 DO
      0  I i>blkfile blkfile.flags !
   LOOP  ;

: BLKFILE!  ( blk offset blkfile-id -- )
   TUCK  blk.offset !  blk.cur ! ;
: BLKFILE@  ( blkfile-id -- blk offset fence )
   DUP >R
   blk.cur @   R@ blk.offset @   R> blk.fence @ ;




   ( blkfile - extension to treat blocks as files       3 / n)
: get-blkfile-id  ( flags -- blkfile-id | 0 )  #BLKFILE 0 DO
   I i>blkfile  blkfile.flags @  flag.open AND  0= IF
      flag.open +
      I i>blkfile DUP >R  blkfile.flags  !
      R>   UNLOOP EXIT THEN
   LOOP   DROP 0 ;

: free-blkfile-id ( blkfile-id -- )
   FALSE SWAP blkfile.flags ! ;







   ( blkfile - extension to treat blocks as files       4 / n)
: ?BLKFILE  ( blkfile-id -- )
   DUP 'blkfile  <> IF ( blkfile-id )
      'blkfile  ?DUP IF
         END-BLKFILE  ( blkfile-id blkfile-id.old blk offset )
         ROT BLKFILE!  ( blkfile-id )
      THEN
      DUP TO 'blkfile   BLKFILE@ BEGIN-BLKFILE ELSE DROP THEN ;








   ( blkfile - extension to treat blocks as files       5 / n)
: OPEN-BLKFILE ( blk fence fam -- blkfileid ior )
   get-blkfile-id ?DUP IF    ( blk fence blkfile-id )
     TUCK  blk.fence !
     SWAP   ( blkfile-id blk )
     OVER 2DUP   blk.cur !  blk.origin !   ( blkfile-id )
     0 OVER blk.offset !   0
   ELSE DROP -69  THEN ;

: CLOSE-BLKFILE ( blkfileid -- ior )
   'blkfile  ?DUP IF END-BLKFILE BLKFILE!  0 TO 'blkfile  THEN
   free-blkfile-id   0 ;





   ( blkfile - extension to treat blocks as files       6 / n)

: READ-BLKFILE ( c-addr u blkfileid -- u ior )
   ?BLKFILE  GETCHARS  0 ;

: READLINE-BLKFILE ( c-addr u blkfileid -- u f ior ) 
   ?BLKFILE GETLINE 0 ;

: WRITE-BLKFILE ( c-addr u blkfileid -- ior )
   ?BLKFILE  PUTCHARS  0 ;

: WRITELINE-BLKFILE ( c-addr u blkfileid -- u f ior ) 
   ?BLKFILE PUTCHARS  13 PUTCH  0 ;



   ( blkfile - extension to treat blocks as files       7 / n)
: TLIST ( blk -- )
   -1  R/O OPEN-BLKFILE THROW  ( blkfile-id )
   BEGIN
     DUP blkfile-buffer BLKFILE-BUFFER-SIZE ROT
          READLINE-BLKFILE THROW
   WHILE
     blkfile-buffer SWAP TYPE CR
   REPEAT
   DROP
   CLOSE-BLKFILE THROW  ;





   ( blkfile - extension to treat blocks as files       8 / n)
: tload-refill  ( -- flag )
    blkfile-buffer BLKFILE-BUFFER-SIZE SOURCE-ID 
       READLINE-BLKFILE THROW
    IF
       blkfile-buffer SWAP  'SOURCE 2!
       0 >IN !  TRUE
    ELSE DROP  FALSE  THEN ;








   ( blkfile - extension to treat blocks as files       9 / n)
: (TLOAD) ( blk -- )
   -1  R/O  OPEN-BLKFILE THROW  ( blkfile-id )
   'SOURCE-ID  !
   0 BLK !
   BEGIN
     REFILL  IF
       ( SOURCE TYPE  CR )
       INTERPRET
     ELSE  'SOURCE-ID @ 
           CLOSE-BLKFILE  THROW  EXIT
     THEN
   AGAIN  ;



   ( blkfile - extension to treat blocks as files       10 / n)

: TLOAD ( blk -- )
   SAVE-INPUT N>R
   ['] tload-refill 'REFILL !
   (TLOAD)
   NR> RESTORE-INPUT THROW  ;
