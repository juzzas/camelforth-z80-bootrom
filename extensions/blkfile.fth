( blkfile - extension to treat blocks as files          0 / n)

CR .( Loading blkfile... )




ONLY FORTH DEFINITIONS   ALSO SYSTEM
1 16 +THRU
/BLKFILE  ONLY FORTH






   ( blkfile - extension to treat blocks as files       1 / n)
4 CONSTANT #BLKFILE
128 CONSTANT BLKFILE-BUFFER-SIZE

$8000 CONSTANT flag.open          $0001 CONSTANT flag.binary
$0002 CONSTANT flag.readable      $0004 CONSTANT flag.writable

flag.readable CONSTANT R/O        flag.writable CONSTANT W/O
flag.readable flag.writable +  CONSTANT R/W
: BIN   flag.binary + ;

0 VALUE 'blkfile




   ( blkfile - extension to treat blocks as files       2 / n)
\ blkfile structure
: blkfile.flags  ( blkfile -- addr )          ;
: blkfile.origin  ( blkfile -- addr )    2 +  ;
: blkfile.cur  ( blkfile -- addr )       4 +  ;
: blkfile.offset  ( blkfile -- addr )    6 +  ;
: blkfile.fence  ( blkfile -- addr )     8 +  ;
: blkfile.buffer  ( blkfile -- addr )    10 +  ;
12  CONSTANT BLKFILE-CONTEXT

CREATE blkfiles   BLKFILE-CONTEXT #BLKFILE *   ALLOT





   ( blkfile - extension to treat blocks as files       3 / n)
128 CONSTANT buff%

0 VALUE buffpool

: (buffpool-allot)  ( -- ptr )
    HERE 0  ,  buff% ALLOT ;

: (buffpool-get) ( -- ptr )
   buffpool >R
   buffpool @ TO buffpool  R>  ;

: (buffpool-free)  ( ptr -- )
   DUP buffpool  !   TO buffpool  ;

: buffpool-get   ( -- c-addr )
   buffpool   IF   (buffpool-get)
   ELSE   (buffpool-allot)   THEN    \ ptr to buffer item 
   CELL+  ;

: buffpool-free  ( c-addr -- )
   CELL-  ( ptr )
   (buffpool-free)  ;


   ( blkfile - extension to treat blocks as files       3 / n)
: i>blkfile  ( n -- blkfileid ) 
   BLKFILE-CONTEXT *  blkfiles + ;
: /BLKFILE    \ initialise BLKFILEs
   #BLKFILE 0 DO
      I i>blkfile BLKFILE-CONTEXT  0 FILL
   LOOP  ;

: BLKFILE!  ( blk offset blkfile-id -- )
   TUCK  blkfile.offset !  blkfile.cur ! ;
: BLKFILE@  ( blkfile-id -- blk offset fence )
   DUP >R
   blkfile.cur @   R@ blkfile.offset @   R> blkfile.fence @ ;



   ( blkfile - extension to treat blocks as files       4 / n)
: (find-free-blkfile-id)  ( -- blkfile-id | 0 )
   blkfiles    #BLKFILE 0 DO
      DUP blkfile.flags @  flag.open AND 
      0= IF   UNLOOP EXIT   THEN
      BLKFILE-CONTEXT +  LOOP
   DROP 0  ;

: get-blkfile-id  ( flags -- blkfile-id | 0 ) 
   (find-free-blkfile-id)                ( flags blkfile-id|0 )
   DUP IF   SWAP flag.open +  OVER    blkfile.flags  !
   ELSE  NIP  THEN  ;

: free-blkfile-id ( blkfile-id -- )
   FALSE SWAP blkfile.flags ! ;

   ( blkfile - extension to treat blocks as files       5 / n)

: ?BLKFILE  ( blkfile-id -- )
   DUP 'blkfile  <> IF ( blkfile-id )
      'blkfile  ?DUP IF
         END-BLKFILE  ( blkfile-id blkfile-id.old blk offset )
         ROT BLKFILE!  ( blkfile-id )
      THEN
      DUP TO 'blkfile   BLKFILE@ BEGIN-BLKFILE 
   ELSE DROP THEN ;






   ( blkfile - extension to treat blocks as files       6 / n)
: OPEN-BLKFILE ( blk fence fam -- blkfileid )
   get-blkfile-id ?DUP IF    ( blk fence blkfile-id )
     TUCK  blkfile.fence !
     SWAP   ( blkfile-id blk )
     OVER 2DUP  blkfile.cur !  blkfile.origin !  ( blkfile-id )
     buffpool-get OVER blkfile.buffer !
     0 OVER blkfile.offset !
     DUP ?BLKFILE
   ELSE   -69 THROW   THEN ;

: (CLOSE-FILE) ( blkfileid -- )
   DUP ?BLKFILE
   'blkfile  ?DUP IF END-BLKFILE BLKFILE!  0 TO 'blkfile  THEN
   DUP blkfile.buffer @ buffpool-free
   free-blkfile-id ;


   ( blkfile - extension to treat blocks as files       7 / n)
: (WRITE-FILE) ( c-addr u blkfileid --  )
   ?BLKFILE  PUTCHARS  ;

: (WRITE-LINE) ( c-addr u blkfileid -- u f ) 
   ?BLKFILE PUTCHARS  13 PUTCH ;

: (READ-FILE) ( c-addr u blkfileid -- u )
   ?BLKFILE  GETCHARS ;

: (READ-LINE) ( c-addr u blkfileid -- u f ) 
   ?BLKFILE GETLINE ;




   ( blkfile - extension to treat blocks as files       8 / n)
: READ-FILE ( c-addr u fileid -- u ior ) 
   ['] (READ-FILE) CATCH
   DUP IF >R 0 R> THEN ;

: READ-LINE ( c-addr u fileid -- u f ior ) 
   ['] (READ-LINE) CATCH
   DUP  IF >R 0 0 R> THEN ;

: WRITE-FILE ( c-addr u fileid -- ior ) 
   ['] (WRITE-FILE)  CATCH ;

: WRITE-LINE ( c-addr u fileid -- ior ) 
   ['] (WRITE-LINE) CATCH ;


   ( blkfile - extension to treat blocks as files       9 / n)

: CLOSE-FILE    ( fileid -- ior )
    ['] (CLOSE-FILE) CATCH  ;












   ( blkfile - extension to treat blocks as files      10 / n)
: TLIST ( blk -- )
   -1  R/O OPEN-BLKFILE   ( blkfile-id )
   BEGIN
     DUP DUP blkfile.buffer @  BLKFILE-BUFFER-SIZE ROT
          (READ-LINE)
   WHILE       ( blkfile-id chrs )
     OVER blkfile.buffer @  SWAP TYPE CR
   REPEAT
   DROP
   (CLOSE-FILE)   ;





   ( blkfile - extension to treat blocks as files      11 / n)
1 VALUE line-index

: tload-refill  ( -- flag )
    SOURCE-ID blkfile.buffer @   BLKFILE-BUFFER-SIZE 
       SOURCE-ID  (READ-LINE)
    IF
       line-index 1+ TO line-index
       SOURCE-ID  blkfile.buffer @   SWAP  'SOURCE 2!
       0 >IN !  TRUE
    ELSE DROP  FALSE  THEN ;





   ( blkfile - extension to treat blocks as files      12 / n)
: (INCLUDE-BLK) ( blk -- )
   -1  R/O  OPEN-BLKFILE  ( blkfile-id )
   'SOURCE-ID  !
   0 BLK !
   BEGIN
     REFILL  IF
       ( SOURCE TYPE  CR )
       INTERPRET
     ELSE  'SOURCE-ID @ 
           (CLOSE-FILE)  EXIT
     THEN
   AGAIN  ;



   ( blkfile - extension to treat blocks as files      13 / n)

: INCLUDE-BLKFILE ( blk -- )
   SAVE-INPUT N>R
   line-index >R
   0 TO line-index
   ['] tload-refill 'REFILL !
   ['] (INCLUDE-BLK)  CATCH ?DUP IF
       >R CR ." Line: " line-index .
       R> THROW THEN
   R> TO line-index 
   NR> RESTORE-INPUT THROW  ;




   ( blkfile - extension to treat blocks as files       14 / n)
: FILE-POSITION  ( fileid -- ud ior )
   DUP ?BLKFILE
   DUP blkfile.cur @   ( fileid curr  )
   OVER blkfile.origin @ -  >R  ( fileid  ; r: #blks )
   blkfile.offset @  S>D  
   R>  1024 UM*  D+   0 ;

: REPOSITION-FILE ( ud fileid -- ior )
   DUP ?BLKFILE   >R
   1024 UM/MOD SWAP  ( #blks offset ; r: fileid )
   R@  blkfile.offset  !
   R@  blkfile.origin  @  +
   R>  blkfile.cur  !    0 ;

: FILE-SIZE ( fileid -- ud ior )
   DROP        0 0 -66 ;

: FLUSH-FILE ( fileid -- ior ) 
   DROP FLUSH   0 ;

: INCLUDE-FILE ( i * x fileid -- j * x )  
   DROP         ;

