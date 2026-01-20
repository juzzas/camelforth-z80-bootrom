( blkfile - extension to treat blocks as files         0 / n )

CR .( Loading blkfile... )




ONLY FORTH DEFINITIONS   ALSO SYSTEM
1 22 +THRU
ONLY FORTH DEFINITIONS






   ( blkfile - extension to treat blocks as files      1 / n )
WORDLIST CONSTANT blkfile-private-wid
blkfile-private-wid >ORDER   blkfile-private-wid SET-CURRENT
4 CONSTANT #BLKFILE
$0001 CONSTANT flag.binary
$0002 CONSTANT flag.readable      $0004 CONSTANT flag.writable

FORTH-WORDLIST SET-CURRENT
flag.readable CONSTANT R/O        flag.writable CONSTANT W/O
flag.readable flag.writable +  CONSTANT R/W
: BIN   flag.binary + ;

blkfile-private-wid SET-CURRENT
128 CONSTANT buff%
0 VALUE 'blkfile
0 VALUE blkfidpool
   ( blkfile - extension to treat blocks as files      2 / n )
\ blkfile structure
blkfile-private-wid SET-CURRENT
32 CONSTANT blkfile.name%
BEGIN-STRUCTURE BLKFILE-CONTEXT%
   BLKF% +FIELD           blkfile.blkfid 
   FIELD:                 blkfile.flags
   blkfile.name% +FIELD   blkfile.name
   2 CELLS +FIELD         blkfile.filesize
   2 CELLS +FIELD         blkfile.bufferpos
   buff% +FIELD           blkfile.buffer
END-STRUCTURE





   ( blkfile - extension to treat blocks as files      3 / n )
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
   CELL+  DUP  BLKFILE-CONTEXT% 0 FILL  ;
: blkfidpool-free  ( c-addr -- )
   CELL-  ( ptr )
   (blkfidpool-free)  ;

   ( blkfile - extension to treat blocks as files      4 / n )
FORTH-WORDLIST SET-CURRENT
: OPEN-BLKFILE ( blk fam -- blkfileid )
   blkfidpool-get ?DUP IF    ( blk fam blkfile-id )
     TUCK  blkfile.flags !     ( blk blkfile-id )
     DUP -1 S>D ROT blkfile.filesize 2!     ( blk blkfile-id )
     TUCK /BLKF 
   ELSE   -69 THROW   THEN ;

: OPEN-LIMIT-BLKFILE ( blk nblks fam -- blkfileid )
   0 SWAP OPEN-BLKFILE     ( blk nblks  blkfile-id )
   DUP >R
   ?DUP IF  BLKF.SLICE SUBSLICE  ELSE  2DROP  THEN
   R>  ;

: OPEN-FENCE-BLKFILE  ( blkstart blkend fam -- blkfileid )
   >R OVER  -  R>  OPEN-LIMIT-BLKFILE   ;
   ( blkfile - extension to treat blocks as files      5 / n )
: adjust-fpos  ( n  blkfileid -- )
   DUP >R  BLKF>POSITION@  ( n d    r: blkfileid )
   ROT M+  R>  BLKF>POSITION!  ;

: update-filesize  ( blkfileid -- )
   DUP >R  BLKF>POSITION@ 
   R@  blkfile.filesize 2@   DMAX
   R>  blkfile.filesize 2!  ;







   ( blkfile - extension to treat blocks as files      6 / n )
blkfile-private-wid SET-CURRENT
: (CLOSE-BLKFILE) ( blkfileid -- )
   BLKF-FLUSH
   blkfidpool-free  ;

: (WRITE-FILE) ( c-addr u blkfileid --  )
   DUP >R
   BLKF-PUTCHARS DROP 
   R> update-filesize  ;

CREATE eol$ 1 C, 13 C,
: (WRITE-LINE) ( c-addr u blkfileid -- )
   DUP >R BLKF-PUTCHARS  DROP
   eol$ COUNT R@ BLKF-PUTCHARS  DROP  
   R> update-filesize  ;
   ( blkfile - extension to treat blocks as files      7 / n )

: scan-eof   ( c-addr u -- c-addr u' ; trim to eof )
   2DUP  26  SCAN    ( c-addr u c-addr' u' )
      NIP -  ;
: scan-eol   ( c-addr u -- c-addr u' f ; true if eol found )
   2DUP  13  SCAN    ( c-addr u c-addr' u' )
      DUP 0<> >R   NIP -  R>   ;
: eof?   ( c-addr -- f )
   C@ 26 =   ;

: binary?  ( blkfileid -- f )
   blkfile.flags flag.binary AND 0<>  ;

: readable?  ( blkfileid -- f )
   blkfile.flags flag.readable AND 0<>  ;

: readable?  ( blkfileid -- f )
   blkfile.flags flag.writable AND 0<>  ;

: ?calc-to-eof  ( u blkfileid --  u' )
   DUP blkfile.filesize 2@
   ROT BLKF>POSITION@  D-
   IF   DROP ELSE UMIN  THEN  ;

: at-eof?  ( blkfileid -- f )
   DUP blkfile.filesize 2@
   ROT BLKF>POSITION@  D=  ;

   ( blkfile - extension to treat blocks as files      8 / n )
: (READ-FILE) ( c-addr u blkfileid -- u )
   DUP >R ?calc-to-eof R>   BLKF-GETCHARS  ;

: (READ-LINE)   ( c-addr u blkfileid -- u f )
   DUP >R ?calc-to-eof R>
   DUP at-eof? IF  DROP 2DROP 0 FALSE EXIT  THEN

   DUP DUP >R  BLKF>POSITION@  2>R
                      ( c-addr u blkfid  r: blkfid filepos )
   2>R DUP 2R>   ( c-addr c-addr u blkfid  r: blkfid filepos )
   BLKF-GETCHARS   ( c-addr c-addr u'  r: blkfid filepos )

   OVER eof? IF  
      2R> R>  BLKF>POSITION!   2DROP 0 FALSE EXIT  THEN

   scan-eof   ( c-addr u"   r: blkfid filepos )
   scan-eol   ( c-addr u"' f   r: blkfid filepos )
   ROT DROP   ( u"' f  r: blkfid filepos )

   OVER SWAP IF   1+   THEN
       2R> ROT M+  R> BLKF>POSITION! 
   TRUE  ;

   ( blkfile - extension to treat blocks as files      9 / n )
FORTH-WORDLIST SET-CURRENT
: READ-FILE ( c-addr u fileid -- u ior ) 
   ['] (READ-FILE) CATCH
   DUP IF >R 2DROP DROP 0 R> THEN ;

: READ-LINE ( c-addr u fileid -- u f ior ) 
   ['] (READ-LINE) CATCH
   DUP  IF >R 2DROP DROP 0 FALSE  R> THEN ;







   ( blkfile - extension to treat blocks as files     10 / n )
: WRITE-FILE ( c-addr u fileid -- ior ) 
   ['] (WRITE-FILE)  CATCH
   DUP IF NIP NIP NIP THEN ;

: WRITE-LINE ( c-addr u fileid -- ior ) 
   ['] (WRITE-LINE) CATCH 
   DUP IF NIP NIP NIP THEN ;








   ( blkfile - extension to treat blocks as files     11 / n )
: CLOSE-BLKFILE    ( fileid -- ior )
   ['] (CLOSE-BLKFILE) CATCH
   DUP IF NIP THEN ;








   ( blkfile - extension to treat blocks as files     12 / n )
: TLIST ( blk -- )
   R/O OPEN-BLKFILE   ( blkfile-id )
   BEGIN
     DUP DUP blkfile.buffer  buff% ROT
          (READ-LINE)   ( blkfile-id chrs f )
   WHILE       ( blkfile-id chrs )
     OVER blkfile.buffer  SWAP TYPE CR
   REPEAT
   DROP (CLOSE-BLKFILE)   ;






   ( blkfile - extension to treat blocks as files     13 / n )
blkfile-private-wid SET-CURRENT
1 VALUE line-index
16 STACK: inputs-stack

2VARIABLE input-position   0 0 input-position 2!

: source-file?  ( source-id -- f )
   ?DUP IF  -1 <>  ELSE  FALSE  THEN ;

FORTH-WORDLIST SET-CURRENT
: SAVE-INPUT 
   'REFILL @   BLK @  SLICE-ID @   'SOURCE 2@  >IN @   6
   inputs-stack STACK-SET

   SOURCE-ID  source-file? IF
      input-position 2@
      inputs-stack >S   inputs-stack >S
   THEN

   SOURCE-ID inputs-stack >S
   inputs-stack STACK-GET  ;


: RESTORE-INPUT
   inputs-stack STACK-SET

   inputs-stack S>  DUP  >R  ( r: source-id )
   source-file? IF
      inputs-stack S>  inputs-stack S>
      2DUP input-position 2!
      R@ BLKF>POSITION!
      R@ blkfile.buffer buff% R@ (READ-LINE) 2DROP
   THEN

   inputs-stack STACK-GET
   6 = IF 
      >IN !  'SOURCE 2!  SLICE-ID !   BLK !   'REFILL !
      R> 'SOURCE-ID !
      BLK @  ?DUP IF
         BLOCK B/BLK 'SOURCE 2!
      THEN
      FALSE
   ELSE
      R>  DROP
      TRUE
   THEN  ;

blkfile-private-wid SET-CURRENT
: tload-refill  ( -- flag )
    SOURCE-ID BLKF>POSITION@
          input-position 2!
    SOURCE-ID blkfile.buffer   buff%
       SOURCE-ID  (READ-LINE)
    IF
       line-index 1+ TO line-index
       SOURCE-ID  blkfile.buffer   SWAP  'SOURCE 2!
       0 >IN !  TRUE
    ELSE DROP  FALSE  THEN ;




   ( blkfile - extension to treat blocks as files     14 / n )
: (INCLUDE-BLK) ( blk -- )
   R/O  OPEN-BLKFILE  ( blkfile-id )
   'SOURCE-ID  !
   0 BLK !
   BEGIN
     REFILL  IF
       ( SOURCE TYPE  CR )  \ debug print of line
       INTERPRET
     ELSE  'SOURCE-ID @ 
           (CLOSE-BLKFILE)  EXIT
     THEN
   AGAIN  ;



   ( blkfile - extension to treat blocks as files     15 / n )
FORTH-WORDLIST SET-CURRENT
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

blkfile-private-wid SET-CURRENT
: blkofs>bytes  ( blk off -- ud )
   S>D ROT 1024 UM* D+ ;
   ( blkfile - extension to treat blocks as files     16 / n )
FORTH-WORDLIST SET-CURRENT
: FILE-POSITION  ( fileid -- ud ior )
   BLKF>POSITION@   0 ;

: REPOSITION-FILE ( ud fileid -- ior )
   BLKF>POSITION!   0 ;

: FILE-SIZE ( fileid -- ud ior )
   blkfile.filesize 2@   0 ;

: RESIZE-FILE ( ud fileid -- ior )
   blkfile.filesize 2!   0 ;

: FLUSH-FILE ( fileid -- ior )
   DROP FLUSH   0 ;

   ( blkfile - extension to treat blocks as files     17 / n )
SYSTEM-WORDLIST SET-CURRENT
: .BLKF  ( blkfid -- )
   ." BLKF:" DUP U. CR
   ."  OFFSET : "  DUP BLKF.OFFSET @ U. CR
   ."  BLK    : "  DUP BLKF.BLK    @ U. CR
   ."  ORIGIN : "  DUP BLKF.ORIGIN @ U. CR
   ."  SLICE  : "      BLKF.SLICE    U. CR  ;

: .BLKFILE  ( blkfid -- )
   DUP .BLKF
   ." BLKFILE:"       DUP U. CR
   ."  FLAGS  : "     DUP blkfile.flags     @   U. CR
   ."  NAME : "       DUP blkfile.name      COUNT TYPE CR
   ."  FILESIZE : "   DUP blkfile.filesize  2@  D. CR
   ."  BUFFER : "         blkfile.buffer        U. CR  ;
   ( blkfile - extension to treat blocks as files     18 / n )
: .SLICE   ( sliceid -- )
   ." SLICE:" DUP U. CR
   ."  DRIVE  : "  DUP SLICE.DRIVE   @ U. CR
   ."  OFFSET : "  DUP SLICE.OFFSET 2@ D. CR
   ."  LIMIT  : "      SLICE.LIMIT   @ U. CR   ;

