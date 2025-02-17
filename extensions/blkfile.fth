( blkfile - extension to treat blocks as files          0 / n)

.( Loading blkfile... ) CR


1 CONSTANT BIN 
2 CONSTANT R/O
4 CONSTANT R/W

ONLY FORTH DEFINITIONS   ALSO UTILS
1 10 +THRU
/BLKFILE  ONLY FORTH




   ( blkfile - extension to treat blocks as files       1 / n)
\ start of BLKFILE extension
BEGIN-STRUCTURE BLKFILE-CONTEXT
   FIELD: blkfile.open
   FIELD: blk.origin
   FIELD: blk.cur
   FIELD: blk.offset
   FIELD: blkfile.fam
   FIELD: blkfile.eof
END-STRUCTURE

4 CONSTANT #BLKFILE
256 CONSTANT BLKFILE-BUFFER-SIZE
CREATE blkfiles BLKFILE-CONTEXT #BLKFILE * ALLOT
CREATE blkfile-buffer BLKFILE-BUFFER-SIZE ALLOT
VARIABLE 'blkfile   0 'blkfile !
   ( blkfile - extension to treat blocks as files       2 / n)

   \ blkfile-id  manipulation
: i>blkfile BLKFILE-CONTEXT *  blkfiles + ;
: /BLKFILE    \ initialise BLKFILEs
    #BLKFILE 0 DO
       FALSE  I i>blkfile blkfile.open !
    LOOP  ;
: BLKFILE!  ( blk offset blkfile-id -- )
   TUCK  blk.offset !  blk.cur ! ;
: BLKFILE@  ( blkfile-id -- blk offset )
   DUP blk.cur @  SWAP blk.offset @ ;




   ( blkfile - extension to treat blocks as files       3 / n)
: get-blkfile-id  ( -- blkfile-id | 0 )  #BLKFILE 0 DO
    I i>blkfile blkfile.open @ 0= IF
        I i>blkfile TRUE OVER blkfile.open ! UNLOOP EXIT THEN
  LOOP  0 ;

: free-blkfile-id ( blkfile-id -- ) FALSE SWAP blkfile.open ! ;









   ( blkfile - extension to treat blocks as files       4 / n)
: ?BLKFILE  ( blkfile-id -- )
   DUP 'blkfile @ <> IF ( blkfile-id )
      'blkfile @ ?DUP IF
         END-BLKFILE  ( blkfile-id blkfile-id.old blk offset )
         ROT BLKFILE!  ( blkfile-id )
      THEN
      DUP 'blkfile !  BLKFILE@ BEGIN-BLKFILE ELSE DROP THEN ;








   ( blkfile - extension to treat blocks as files       5 / n)
: OPEN-BLKFILE ( blk fam -- blkfileid ior )
   get-blkfile-id ?DUP IF    ( blk fam blkfile-id )
     SWAP OVER  blkfile.fam !   ( blk blkfile-id )
     SWAP   ( blkfile-id blk )
     OVER 2DUP blk.cur !  blk.origin !   ( blkfile-id )
     0 OVER blk.offset !   0
   ELSE -69 THROW  THEN ;

: CLOSE-BLKFILE ( blkfileid -- ior )
   'blkfile @ ?DUP IF END-BLKFILE BLKFILE! 0 'blkfile !  THEN
   free-blkfile-id   0 ;




   ( blkfile - extension to treat blocks as files       6 / n)

\ READ-BLKFILE 
: READ-BLKFILE ( c-addr u blkfileid -- u ior )
   ?BLKFILE  GETCHARS  0 ;
VARIABLE blkfile-eof

: READLINE-BLKFILE ( c-addr u blkfileid -- u f ior ) 
   ?BLKFILE GETLINE 0 ;







   ( blkfile - extension to treat blocks as files       7 / n)
: TLIST ( blk -- )
   R/O  OPEN-BLKFILE THROW  ( blkfile-id )
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
   R/O  OPEN-BLKFILE THROW  ( blkfile-id )
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
