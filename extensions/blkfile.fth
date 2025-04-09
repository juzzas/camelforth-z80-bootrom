( blkfile - extension to treat blocks as files          0 / n)

CR .( Loading blkfile... )




ONLY FORTH DEFINITIONS   ALSO SYSTEM
1 13 +THRU
/BLKFILE  ONLY FORTH






   ( blkfile - extension to treat blocks as files       1 / n)
\ blkfile structure
: blkfile.flags  ( blkfile -- addr )      ;
: blkfile.origin  ( blkfile -- addr )    2 +  ;
: blkfile.cur  ( blkfile -- addr )       4 +  ;
: blkfile.offset  ( blkfile -- addr )    6 +  ;
: blkfile.fence  ( blkfile -- addr )     8 +  ;
10 CONSTANT BLKFILE-CONTEXT








   ( blkfile - extension to treat blocks as files       1 / n)
4 CONSTANT #BLKFILE
256 CONSTANT BLKFILE-BUFFER-SIZE
CREATE blkfiles BLKFILE-CONTEXT #BLKFILE * ALLOT
CREATE blkfile-buffer BLKFILE-BUFFER-SIZE ALLOT
$8000 CONSTANT flag.open
$0001 CONSTANT flag.binary
$0002 CONSTANT flag.readable
$0004 CONSTANT flag.writable

flag.readable CONSTANT R/O
flag.writable CONSTANT W/O
flag.readable flag.writable +  CONSTANT R/W
: BIN   flag.binary + ;

0 VALUE 'blkfile

   ( blkfile - extension to treat blocks as files       2 / n)
: i>blkfile  ( n -- blkfileid ) 
   BLKFILE-CONTEXT *  blkfiles + ;
: /BLKFILE    \ initialise BLKFILEs
   #BLKFILE 0 DO
      0  I i>blkfile blkfile.flags !
   LOOP  ;

: BLKFILE!  ( blk offset blkfile-id -- )
   TUCK  blkfile.offset !  blkfile.cur ! ;
: BLKFILE@  ( blkfile-id -- blk offset fence )
   DUP >R
   blkfile.cur @   R@ blkfile.offset @   R> blkfile.fence @ ;



   ( blkfile - extension to treat blocks as files       3 / n)
: (find-free-blkfile-id)  ( -- blkfile-id | 0 )
   blkfiles
   #BLKFILE 0 DO
      DUP blkfile.flags @  flag.open AND 
      0= IF   UNLOOP EXIT   THEN
      BLKFILE-CONTEXT +  LOOP
   DROP 0  ;

: get-blkfile-id  ( flags -- blkfile-id | 0 ) 
   (find-free-blkfile-id)                ( flags blkfile-id|0 )
   DUP IF
      SWAP flag.open +  OVER
      blkfile.flags  !
   ELSE  NIP  THEN  ;

: free-blkfile-id ( blkfile-id -- )
   FALSE SWAP blkfile.flags ! ;






   ( blkfile - extension to treat blocks as files       4 / n)
: ?BLKFILE  ( blkfile-id -- )
   DUP 'blkfile  <> IF ( blkfile-id )
      'blkfile  ?DUP IF
         END-BLKFILE  ( blkfile-id blkfile-id.old blk offset )
         ROT BLKFILE!  ( blkfile-id )
      THEN
      DUP TO 'blkfile   BLKFILE@ BEGIN-BLKFILE 
   ELSE DROP THEN ;







   ( blkfile - extension to treat blocks as files       5 / n)
: OPEN-BLKFILE ( blk fence fam -- blkfileid )
   get-blkfile-id ?DUP IF    ( blk fence blkfile-id )
     TUCK  blkfile.fence !
     SWAP   ( blkfile-id blk )
     OVER 2DUP  blkfile.cur !  blkfile.origin !  ( blkfile-id )
     0 OVER blkfile.offset !
     DUP ?BLKFILE
   ELSE   -69 THROW   THEN ;

: CLOSE-BLKFILE ( blkfileid -- )
   DUP ?BLKFILE
   'blkfile  ?DUP IF END-BLKFILE BLKFILE!  0 TO 'blkfile  THEN
   free-blkfile-id ;




   ( blkfile - extension to treat blocks as files       6 / n)

: READ-BLKFILE ( c-addr u blkfileid -- u )
   ?BLKFILE  GETCHARS ;

: READLINE-BLKFILE ( c-addr u blkfileid -- u f ) 
   ?BLKFILE GETLINE ;

: WRITE-BLKFILE ( c-addr u blkfileid --  )
   ?BLKFILE  PUTCHARS  ;

: WRITELINE-BLKFILE ( c-addr u blkfileid -- u f ) 
   ?BLKFILE PUTCHARS  13 PUTCH ;



   ( blkfile - extension to treat blocks as files       7 / n)
: TLIST ( blk -- )
   -1  R/O OPEN-BLKFILE   ( blkfile-id )
   BEGIN
     DUP blkfile-buffer BLKFILE-BUFFER-SIZE ROT
          READLINE-BLKFILE 
   WHILE
     blkfile-buffer SWAP TYPE CR
   REPEAT
   DROP
   CLOSE-BLKFILE   ;





   ( blkfile - extension to treat blocks as files       8 / n)
1 VALUE line-index

: tload-refill  ( -- flag )
    blkfile-buffer BLKFILE-BUFFER-SIZE SOURCE-ID 
       READLINE-BLKFILE 
    IF
       line-index 1+ TO line-index
       blkfile-buffer SWAP  'SOURCE 2!
       0 >IN !  TRUE
    ELSE DROP  FALSE  THEN ;






   ( blkfile - extension to treat blocks as files       9 / n)
: (TLOAD) ( blk -- )
   -1  R/O  OPEN-BLKFILE  ( blkfile-id )
   'SOURCE-ID  !
   0 BLK !
   BEGIN
     REFILL  IF
       ( SOURCE TYPE  CR )
       INTERPRET
     ELSE  'SOURCE-ID @ 
           CLOSE-BLKFILE  EXIT
     THEN
   AGAIN  ;



   ( blkfile - extension to treat blocks as files       10 / n)

: TLOAD ( blk -- )
   SAVE-INPUT N>R
   line-index >R
   0 TO line-index
   ['] tload-refill 'REFILL !
   ['] (TLOAD)  CATCH ?DUP IF
       >R CR ." Line: " line-index .
       R> THROW THEN
   R> TO line-index 
   NR> RESTORE-INPUT THROW  ;
