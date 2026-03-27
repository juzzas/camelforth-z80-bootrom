\ BLKFILE tools 

ONLY FORTH ALSO SYSTEM

blkfile-private-wid >ORDER   blkfile-private-wid SET-CURRENT


: LIST-BLKFILE ( blk -- )
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

SYSTEM-WORDLIST SET-CURRENT
: .BLKF  ( blkfid -- )
   CR ." BLKF:" DUP U.
   CR ."  OFFSET : "  DUP BLKF>OFFSET @ U.
   CR ."  BLK    : "  DUP BLKF>BLK    @ U.
   CR ."  ORIGIN : "  DUP BLKF>ORIGIN @ U.
   CR ."  SLICE  : "      BLKF>SLICE    U.
;

: .BLKFILE  ( blkfid -- )
   DUP .BLKF
   CR ." BLKFILE:"       DUP U. CR
   CR ."  FLAGS  : "     DUP blkfile.flags     @   U.
   CR ."  NAME : "       DUP blkfile.name      COUNT TYPE
   CR ."  FILESIZE : "       blkfile.filesize  2@  D.
;

   ( blkfile - extension to treat blocks as files     18 / n )
: .SLICE   ( sliceid -- )
   CR ." SLICE:" DUP U.
   CR ."  DRIVE  : "  DUP SLICE>DRIVE   @ U.
   CR ."  OFFSET : "  DUP SLICE>OFFSET 2@ D.
   CR ."  LIMIT  : "      SLICE>LIMIT   @ U.
;

: .SOURCE  ( source-id -- )
   CR ." SOURCE: " DUP U.
   CR ."  REFILL: " DUP  SOURCE>REFILL   @ DUP U.  .ID 
   CR ."  GETPOS " DUP  SOURCE>GETPOS   @ DUP U.  .ID 
   CR ."  SETPOS "      SOURCE>GETPOS   @ DUP U.  .ID 
;

ONLY FORTH


