( blkfile - extension to treat blocks as files          1 / n)

.( Loading blkfile... ) CR


1 CONSTANT BIN 
2 CONSTANT R/O
4 CONSTANT R/W
49 LOAD  
ONLY FORTH DEFINITIONS   ALSO UTILS
1 8 +THRU
/BLKFILE  ONLY FORTH




\ start of BLKFILE extension
BEGIN-STRUCTURE BLKFILE-CONTEXT
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
8 STACK: blkfile-ids
\ initialise blkfile 
: /BLKFILE
    #BLKFILE 0 DO
       I BLKFILE-CONTEXT *  blkfiles +  blkfile-ids >S
    LOOP  ;
: BLKFILE!  ( blk offset blkfile-id -- )
   TUCK  blk.offset !  blk.cur ! ;
: BLKFILE@  ( blkfile-id -- blk offset )
   DUP blk.cur @  SWAP blk.offset @ ;
: ?BLKFILE  ( blkfile-id -- )
   DUP 'blkfile @ <> IF ( blkfile-id )
      'blkfile @ ?DUP IF
         END-BLKFILE  ( blkfile-id blkfile-id.old blk offset )
         ROT BLKFILE!  ( blkfile-id )
      THEN
      DUP 'blkfile !  BLKFILE@ BEGIN-BLKFILE ELSE DROP THEN ;
\ start of BLKFILE extension
: OPEN-BLKFILE ( blk fam -- blkfileid ior )
   blkfile-ids SEMPTY? IF -69 THROW THEN
   blkfile-ids S>   ( blk fam blkfile-id )
   SWAP OVER  blkfile.fam !   ( blk blkfile-id )
   SWAP   ( blkfile-id blk )
   OVER 2DUP blk.cur !  blk.origin !   ( blkfile-id )
   0 OVER blk.offset !   0 ;

: CLOSE-BLKFILE ( blkfileid -- ior )
   'blkfile @ ?DUP IF END-BLKFILE BLKFILE! 0 'blkfile !  THEN
   blkfile-ids >S   0 ;




\ READ-BLKFILE 
: READ-BLKFILE ( c-addr u blkfileid -- u ior )
   ?BLKFILE  GETCHARS  0 ;
VARIABLE blkfile-eof












: READLINE-BLKFILE ( c-addr u blkfileid -- u f ior ) 
   ?BLKFILE GETLINE 0 ;













\ TLISTer
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





\ TLOADer
: tload-refill  ( -- flag )
    blkfile-buffer BLKFILE-BUFFER-SIZE SOURCE-ID 
       READLINE-BLKFILE THROW
    IF 
       blkfile-buffer SWAP  'SOURCE 2!
       0 >IN !  TRUE
    ELSE DROP  FALSE  THEN ;

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


: TLOAD ( blk -- )
   SAVE-INPUT N>R
   ['] tload-refill 'REFILL !
   (TLOAD)
   NR> RESTORE-INPUT THROW  ;
