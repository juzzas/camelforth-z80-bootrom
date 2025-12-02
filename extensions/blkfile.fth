( blkfile - extension to treat blocks as files          0 / n)

CR .( Loading blkfile... )




ONLY FORTH DEFINITIONS   ALSO SYSTEM
1 16 +THRU
ONLY FORTH






   ( blkfile - extension to treat blocks as files       1 / n)
4 CONSTANT #BLKFILE

$0001 CONSTANT flag.binary
$0002 CONSTANT flag.readable      $0004 CONSTANT flag.writable

flag.readable CONSTANT R/O        flag.writable CONSTANT W/O
flag.readable flag.writable +  CONSTANT R/W
: BIN   flag.binary + ;

128 CONSTANT buff%
0 VALUE 'blkfile
0 VALUE blkfidpool



   ( blkfile - extension to treat blocks as files       2 / n)
\ blkfile structure
BEGIN-STRUCTURE BLKFILE-CONTEXT%
 BLKF% +FIELD  blkfile.blkfid 
 FIELD:        blkfile.flags
 buff% +FIELD  blkfile.buffer
END-STRUCTURE









   ( blkfile - extension to treat blocks as files       3 / n)
: (blkfidpool-allot)  ( -- ptr )
    HERE 0  ,  BLKFILE-CONTEXT% ALLOT ;
: (blkfidpool-get) ( -- ptr )
   blkfidpool >R
   blkfidpool @ TO blkfidpool  R>  ;
: (blkfidpool-free)  ( ptr -- )
   DUP blkfidpool  !   TO blkfidpool  ;
: blkfidpool-get   ( -- c-addr )
   blkfidpool   IF   (blkfidpool-get)
   ELSE   (blkfidpool-allot)   THEN    \ ptr to buffer item 
   CELL+  ;
: blkfidpool-free  ( c-addr -- )
   CELL-  ( ptr )
   (blkfidpool-free)  ;

   ( blkfile - extension to treat blocks as files       6 / n)
: OPEN-BLKFILE ( blk fam -- blkfileid )
   blkfidpool-get ?DUP IF    ( blk fam blkfile-id )
     TUCK  blkfile.flags !     ( blk blkfile-id )
     TUCK /BLKF 
   ELSE   -69 THROW   THEN ;

: OPEN-LIMIT-BLKFILE ( blk nblks fam -- blkfileid )
   >R OVER R> 
   OPEN-BLKFILE     ( blk nblks  blkfile-id )
   DUP >R
   ?DUP IF  BLKF.SLICE SUBSLICE   ELSE  2DROP  THEN
   R>  ;
: OPEN-FENCE-BLKFILE  ( blkstart blkend fam -- blkfileid )
   >R OVER  -  R>  OPEN-LIMIT-BLKFILE   ;

   ( blkfile - extension to treat blocks as files       6 / n)
: (CLOSE-FILE) ( blkfileid -- )
   BLKF-FLUSH
   blkfidpool-free  ;


CREATE eol$ 1 C, 13 C,

: (WRITE-FILE) ( c-addr u blkfileid --  )
   BLKF-PUTCHARS DROP ;

: (WRITE-LINE) ( c-addr u blkfileid -- )
   DUP >R BLKF-PUTCHARS  DROP
   eol$ COUNT R> BLKF-PUTCHARS  DROP  ;


   ( blkfile - extension to treat blocks as files       7 / n)
: (READ-FILE) ( c-addr u blkfileid -- u )
   BLKF-GETCHARS  ;

: scan-eol   ( c-addr u -- u' f )
   0 -ROT
   BOUNDS   ?DO  
      I C@  DUP  32 <  IF
         13 OVER =  IF  DROP TRUE  LEAVE  THEN 
         26 OVER =  IF  DROP FALSE LEAVE  THEN 
      THEN
      DROP  1+
   LOOP  ;



   ( blkfile - extension to treat blocks as files       7 / n)
: adjust-fpos  ( n  blkfileid -- )
   DUP >R  BLKF>POSITION@  ( n d    r: blkfileid )
   ROT M+  R>  BLKF>POSITION!  ;

: ((READ-LINE))   ( c-addr u blkfileid -- u' f )
   2>R DUP 2R>      ( c-addr c-addr u blkfileid )
   BLKF-GETCHARS   scan-eol    ( u' f )     ;

: (READ-LINE)   ( c-addr u blkfileid -- u f )
   DUP >R   OVER >R
   ((READ-LINE))    ( u' f   r: blkfileid  u )
   OVER     ( u' f u'    r: blkfileid  u )
   R>  -   OVER   IF  1+  THEN  R>   adjust-fpos  ;


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
   R/O OPEN-BLKFILE   ( blkfile-id )
   BEGIN
     DUP DUP blkfile.buffer  buff% ROT
          (READ-LINE)   ( blkfile-id chrs f )
   WHILE       ( blkfile-id chrs )
     OVER blkfile.buffer  SWAP TYPE CR
   REPEAT
   DROP (CLOSE-FILE)    ;






   ( blkfile - extension to treat blocks as files      11 / n)
1 VALUE line-index

: tload-refill  ( -- flag )
    SOURCE-ID blkfile.buffer   buff%
       SOURCE-ID  (READ-LINE)
    IF
       line-index 1+ TO line-index
       SOURCE-ID  blkfile.buffer   SWAP  'SOURCE 2!
       0 >IN !  TRUE
    ELSE DROP  FALSE  THEN ;





   ( blkfile - extension to treat blocks as files      12 / n)
: (INCLUDE-BLK) ( blk -- )
   R/O  OPEN-BLKFILE  ( blkfile-id )
   'SOURCE-ID  !
   0 BLK !
   BEGIN
     REFILL  IF
       ( SOURCE TYPE  CR )  \ debug print of line
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

: blkofs>bytes  ( blk off -- ud )
   S>D ROT 1024 UM* D+ ;


   ( blkfile - extension to treat blocks as files       14 / n)
: FILE-POSITION  ( fileid -- ud ior )
   BLKF>POSITION@  0 ;

: REPOSITION-FILE ( ud fileid -- ior )
   BLKF>POSITION!  0 ;

: FILE-SIZE ( fileid -- ud ior )
   DROP        0 0 -66 ;

: FLUSH-FILE ( fileid -- ior ) 
   DROP FLUSH   0 ;

: INCLUDE-FILE ( i * x fileid -- j * x )  
   DROP         ;

