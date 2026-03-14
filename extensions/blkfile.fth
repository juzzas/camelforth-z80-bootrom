( blkfile - extension to treat blocks as files         0 / n )

CR .( Loading blkfile... )




ONLY FORTH DEFINITIONS   ALSO SYSTEM
1 28 +THRU
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


   ( blkfile - extension to treat blocks as files      2 / n )
\ blkfile structure
blkfile-private-wid SET-CURRENT
32 CONSTANT blkfile.name%

BEGIN-STRUCTURE pool%
   FIELD:  pool.head
   FIELD:  pool.node%
END-STRUCTURE







   ( blkfile - extension to treat blocks as files      2 / n )

: create-pool:  ( node-size ccc"name" -- )
   CREATE 0 , ,  ;

: node-next@    @  ;
: node-next!    !  ;
: node>item    CELL+  ;
: item>node    CELL-  ;

: pool-head@  ( pool-id -- node )    pool.head @  ;
: pool-head!  ( node pool-id -- node )    pool.head !  ;
: pool-node%  ( pool-id -- u )  pool.node% @  ;
: pool-empty?  ( pool-id -- f )    pool-head@ 0=  ;


   ( blkfile - extension to treat blocks as files      2 / n )
: pool-push ( node pool-id -- )  
    SWAP  >R        ( pool-id  r: node )
    DUP pool-head@  R@  node-next!   ( pool-id  r: node )
    R> SWAP pool-head!  ;

: pool-allot  ( pool-id -- node )
   >R  HERE  0 ,  R> pool-node% ALLOT  ;

: pool-pop  ( pool-id -- node )
   DUP pool-head@  ( pool head )
   DUP >R node-next@   ( pool next ;  r: node )
   SWAP pool-head!  R>  ;



   ( blkfile - extension to treat blocks as files      2 / n )
: pool-get   ( pool -- node )
   DUP  pool-empty? IF
      pool-allot
   ELSE
      pool-pop
   THEN  ;

: .pool  ( pool -- )
   CR ." pool: " DUP U.
   pool-head@
   BEGIN
   DUP  WHILE
      CR ."    node: "  DUP U.
      node-next@
   REPEAT   DROP ;
   ( blkfile - extension to treat blocks as files      2 / n )
BEGIN-STRUCTURE BLKFILE-CONTEXT%
   BLKF% +FIELD           blkfile.blkfid 
   FIELD:                 blkfile.flags
   blkfile.name% +FIELD   blkfile.name
   2 CELLS +FIELD         blkfile.filesize
END-STRUCTURE









   ( blkfile - extension to treat blocks as files      4 / n )
BLKFILE-CONTEXT% create-pool:  blkfidpool

: blkfidpool-get  ( -- c-addr )
   blkfidpool pool-get  node>item
   DUP  blkfidpool pool-node% 0 FILL ;

: blkfidpool-free  ( c-addr -- )
   item>node blkfidpool pool-push  ;

: blkfidpool-reserve ( u -- )
   DUP >R 
   0 ?DO  blkfidpool-get  LOOP
   R> 0 ?DO blkfidpool-free LOOP  ;
8 blkfidpool-reserve

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


   ( blkfile - extension to treat blocks as files      5 / n )
: OPEN-FENCE-BLKFILE  ( blkstart blkend fam -- blkfileid )
   >R OVER  -  R>  OPEN-LIMIT-BLKFILE   ;
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








   ( blkfile - extension to treat blocks as files     13 / n )
blkfile-private-wid SET-CURRENT
1 VALUE line-index

BEGIN-STRUCTURE BLKFILE-SOURCE%
   SOURCE% +FIELD         source.source
   FIELD:                 source.blkfile
   DFIELD:                source.bufferpos
   buff% +FIELD           source.buffer
END-STRUCTURE

   ( blkfile - extension to treat blocks as files      3 / n )

BLKFILE-SOURCE% create-pool:  sourcepool
: sourcepool-get  ( -- c-addr )
   sourcepool pool-get  node>item
   DUP  sourcepool pool-node% 0 FILL ;

: sourcepool-free  ( c-addr -- )
   item>node sourcepool pool-push  ;

: sourcepool-reserve ( u -- )
   DUP >R 
   0 ?DO  sourcepool-get  LOOP
   R> 0 ?DO sourcepool-free LOOP  ;
8 sourcepool-reserve

: source-file?  ( source-id -- f )
   ?DUP IF  -1 <>  ELSE  FALSE  THEN ;

FORTH-WORDLIST SET-CURRENT

   ( blkfile - extension to treat blocks as files     12 / n )
blkfile-private-wid SET-CURRENT

: source>blkfile  ( source-ctx -- blkfile-id )
   source.blkfile @   ;

: source>blkf-position@  ( source-ctx -- d )
   source>blkfile BLKF>POSITION@  ;

: source>blkf-position!  ( d source-ctx -- )
   source>blkfile BLKF>POSITION!  ;

: source>bufferpos@  ( source-ctx -- d )
   source.bufferpos 2@  ;

: source>bufferpos!  ( d source-ctx -- )
   source.bufferpos 2!  ;

: TLIST ( blk -- )
   R/O OPEN-BLKFILE   ( blkfile-id )
   sourcepool-get DUP >R
   source.blkfile !
   BEGIN
     R@ source.buffer  buff%
     R@ source>blkfile (READ-LINE)   ( chrs f )
   WHILE       ( chrs )
     R@ source.buffer  SWAP TYPE CR
   REPEAT   DROP
   R@ source>blkfile  (CLOSE-BLKFILE)
   R> sourcepool-free  ;


: tload-refill  ( -- flag )
   SOURCE-ID  DUP >R      ( source-ctx   r: source-ctx )
   source>blkf-position@    R@ source>bufferpos!

   R@ source.buffer  buff%   R> source>blkfile  (READ-LINE)

   IF
      line-index 1+ TO line-index
      SOURCE-ID  source.buffer   SWAP  'SOURCE 2!
      0 >IN !  TRUE
   ELSE DROP  FALSE  THEN 
;

: tload-refetch  ( -- )
   CR ." tload refetch called"
   SOURCE-ID  DUP >R source>blkfile IF
      R@  source>bufferpos@
            R@  source>blkfile BLKF>POSITION!
      R@  source.buffer buff%
            R@  source>blkfile (READ-LINE)
                    2DROP
   THEN  R> DROP
;

: tload-getpos ( -- d )   SOURCE-ID  source>bufferpos@ ;
: tload-setpos ( d -- )   SOURCE-ID  source>bufferpos! ;

   ( blkfile - extension to treat blocks as files     14 / n )
: (INCLUDE-BLK) ( blk -- )
   R/O  OPEN-BLKFILE  ( blkfile-id )
   SOURCE-ID  source.blkfile !
   BEGIN
     REFILL  IF
       ( SOURCE TYPE  CR )  \ debug print of line
       INTERPRET
     ELSE  SOURCE-ID  source.blkfile
           (CLOSE-BLKFILE)  EXIT
     THEN
   AGAIN  ;



   ( blkfile - extension to treat blocks as files     15 / n )
FORTH-WORDLIST SET-CURRENT
: INCLUDE-BLKFILE ( blk -- )
   SAVE-INPUT N>R
   line-index >R
   0 TO line-index
   ['] tload-refill ['] tload-refetch
   sourcepool-get DUP >R    /SOURCE
   ['] tload-getpos   R@ source.source SOURCE.GETPOS !
   ['] tload-setpos   R@ source.source SOURCE.SETPOS !
   R>  SET-SOURCE
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
   ."  FILESIZE : "       blkfile.filesize  2@  D. CR   ;

   ( blkfile - extension to treat blocks as files     18 / n )
: .SLICE   ( sliceid -- )
   ." SLICE:" DUP U. CR
   ."  DRIVE  : "  DUP SLICE.DRIVE   @ U. CR
   ."  OFFSET : "  DUP SLICE.OFFSET 2@ D. CR
   ."  LIMIT  : "      SLICE.LIMIT   @ U. CR   ;

CR  .( Blkfile loaded. )
