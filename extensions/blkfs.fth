( blkfs - extension to treat blocks as a filesystem      0 / n)

CR .( Loading blkfs "filesystem"... )






ONLY FORTH DEFINITIONS
1 16 +THRU
ONLY FORTH DEFINITIONS




\ blkfilefs
blkfile-private-wid >ORDER   blkfile-private-wid SET-CURRENT
$FEED CONSTANT FSMAGIC   32 CONSTANT #NAMECHARS
\ blkfilefs structure
: meta>magic  ( blkfilefs -- addr )        ;
: meta>type  ( blkfilefs -- addr )    2 +  ;
: meta>fence  ( blkfilefs -- addr )   4 +  ;
: meta>here  ( blkfilefs -- addr )    6 +  ;
8 CONSTANT BLKFILEFS-CONTEXT

0 CONSTANT bffstype.dir         1 CONSTANT bffstype.file
2 CONSTANT bffstype.dir_free    3 CONSTANT bffstype.file_free

B/BLK 32 - CONSTANT BASESIZ
VARIABLE cwd   0 cwd !     VARIABLE root  0 root !


  \ blkfilefs
: (magic) ( ptr -- )   FSMAGIC SWAP   meta>magic !   UPDATE ;
: ((>meta))   BASESIZ + ;
: (>meta) ( ptr -- ptr' )   ((>meta))
   DUP meta>magic @ FSMAGIC -  ABORT" Corrupt block" ;
: (initfree) ( blk type -- )   SWAP BUFFER   DUP B/BLK ERASE
   ((>meta))   TUCK meta>type !   (magic) ;
: (inithead) ( #blk blk type -- ) 
   >R   DUP BUFFER   DUP B/BLK ERASE
   ((>meta))   R> OVER meta>type !   >R TUCK + R> TUCK 
      meta>fence !
   SWAP 1+ OVER meta>here !   (magic) ;
: (initbody) ( #blk blk type -- blk )   -ROT TUCK TUCK + SWAP 
   1+ ?DO   OVER I SWAP (initfree)   LOOP NIP ;
FORTH-WORDLIST SET-CURRENT
: MKFS ( base #blk -- )   2DUP SWAP bffstype.dir (inithead)
   SWAP bffstype.dir_free (initbody)   DROP ;
blkfile-private-wid SET-CURRENT
  \ blkfilefs
: dir>name  ( dirent -- addr )  ;
: dir>base  ( dirent -- addr )  #NAMECHARS +  ;
: dir>fence ( dirent -- addr )  #NAMECHARS + 2 + ;
: dir>type  ( dirent -- addr )  #NAMECHARS + 4 + ;
: dir>filesize  ( dirent -- addr )  #NAMECHARS + 6 + ;
#NAMECHARS 10 +  CONSTANT BLKFILEDIR-CONTEXT

BASESIZ BLKFILEDIR-CONTEXT /   CONSTANT #DIRFILES
: (cwd@) ( -- blk )   cwd @ DUP 0= ABORT" No CWD" ;
: (cwd>) ( -- ptr )   (cwd@) BLOCK ;
: (cwd>meta) ( -- ptr )   (cwd>) (>meta) ;
: (block>meta) ( blk -- ptr )   BLOCK (>meta) ;
: 3dup 2 PICK 2 PICK 2 PICK ;
: 3drop DROP 2DROP ;

  \ blkfilefs
: ($namecmp) ( c-addr u dest -- f ) COUNT ROT
   MAX STRCMP 0= ;
: ($dirent) ( c-addr u -- dirent | 0 )   (cwd>) 
                                     ( c-addr u blk-addr )
   #DIRFILES 0 ?DO   DUP dir>base @ 0= IF
       UNLOOP 3drop 0 EXIT THEN
   3dup dir>name ($namecmp)   IF  UNLOOP NIP NIP EXIT  THEN
   BLKFILEDIR-CONTEXT + LOOP   3drop 0 ;
: ($lookup) ( c-addr u -- block | 0 )   ($dirent) DUP 0= IF
      EXIT   THEN
   dir>base @ ;
: cwd! ( blk -- )   DUP (block>meta)
   meta>type @ bffstype.dir -  ABORT" Not dir"
   root @ 0= IF DUP root ! THEN   cwd ! ;
: found? ( blk -- blk )   DUP 0= IF -38 THROW THEN ;
  \ blkfilefs
: $cd ( c-addr u -- )   ($lookup) found?   cwd! ;
FORTH-WORDLIST SET-CURRENT
: CD ( "name" -- )   PARSE-NAME $cd ;
: CD/ ( -- )   root @ DUP 0= ABORT" No root"   cwd! ;
: ROOT!   DUP cwd!   root ! ;










  \ blkfilefs
blkfile-private-wid SET-CURRENT
: (slot) ( blkptr -- dirptr )   #DIRFILES 0 DO
      DUP dir>base @ 0= IF   UNLOOP EXIT   THEN
   BLKFILEDIR-CONTEXT  + LOOP   1 ABORT" Dir full" ;

: (blkallot) ( nblk metaptr -- )   TUCK meta>here @ +
   OVER meta>fence @ OVER < ABORT" Out of space"
   SWAP meta>here ! ;








  \ blkfilefs
: ($mkent) ( nblk c-addr u -- nblk blk dirent )
   2 PICK 0< ABORT" Bad size"   2DUP ($dirent) ABORT" Exists"
   2>R    DUP                        ( nblk nblk  R: c-addr u )
   (cwd>)                     ( nblk nblk blkptr  R: c-addr u )
   DUP (>meta)        ( nblk nblk blkptr metaptr  R: c-addr u )
   2R> ROT  >R        ( nblk nblk blkptr c-addr u  R: metaptr )
   ROT   (slot)      ( nblk nblk  c-addr u dirptr  R: metaptr )
   DUP >R     ( nblk nblk  c-addr u dirptr  R: metaptr dirptr )
   dir>name PLACE  R>           ( nblk nblk dirent R: metaptr )
   R@ meta>here @   -ROT   SWAP R> (blkallot)
                                      ( nblk nblk here dirent )
   OVER -ROT DUP >R dir>base ! R>  UPDATE ;



  \ blkfilefs
: (initfile) ( nblk block -- )
    SWAP BOUNDS ?DO I WIPE UPDATE LOOP ;

: $creat ( nblk c-addr u -- block fence ) 
   ($mkent)  ( nblk blk dirent )
   >R 2DUP +   ( nblk blk fence )   DUP R@ dir>fence !  
   bffstype.file R> dir>type ! UPDATE
   -ROT TUCK  (initfile) SWAP ;

FORTH-WORDLIST SET-CURRENT
: CREAT   PARSE-NAME $creat DROP ;





  \ blkfilefs
blkfile-private-wid SET-CURRENT
: (.type) ( u -- )  
   DUP bffstype.dir = IF ."  Dir   " DROP EXIT THEN
   DUP bffstype.file = IF ." File   " DROP EXIT THEN
   DUP bffstype.dir_free = IF ." (dfree)" DROP EXIT THEN
   DUP bffstype.file_free = IF ." (free) " DROP EXIT THEN 
      5 U.R ;

: entriesDo ( arg 'fn -- )
   #DIRFILES 0 DO   2DUP
      (cwd>) I BLKFILEDIR-CONTEXT * +   
          ( arg 'fn arg 'fn dirent )
     DUP dir>base @ 0= IF   2DROP 2DROP DROP UNLOOP EXIT  THEN
     SWAP EXECUTE   LOOP 2DROP ;


  \ blkfilefs
: .entry ( dirent blkno -- )
   DUP (block>meta) 
   SWAP >R ( dirent meta R: blkno )
   DUP meta>type @ (.type)   R@ 6 U.R   SPACE
   DUP meta>here @ R@ - 4 U.R   ." /"
   meta>fence @ R> - 4 U.R    10 SPACES
   dir>name COUNT TYPE  CR ;

: .fileentry ( dirent blkno -- )
   >R ( dirent R: blkno )
   DUP dir>type @ (.type)   R@ 6 U.R  6 SPACES
   DUP dir>fence @ R> - 4 U.R   2 SPACES
   DUP dir>filesize 2@   8 D.R   2 SPACES
   dir>name COUNT TYPE  CR ;


  \ blkfilefs
: (.ls) ( 0 dirent -- )   
   NIP   DUP dir>base @   ( dirent blk )
   OVER dir>type @
   CASE
      bffstype.file_free OF 2DROP ENDOF
      bffstype.file OF .fileentry ENDOF
      >R .entry R>
   ENDCASE   ;

: .head ( -- )   CR ." Type     Start   Length  "
                    ." Filesize  Name" CR ;
: .ls ( -- )   .head   0   ['] (.ls)   entriesDo ;

FORTH-WORDLIST SET-CURRENT
: LS   .ls ;

  \ blkfilefs
\ Return "open" file
blkfile-private-wid SET-CURRENT
: ($open) ( type c-addr u -- blk fence )
   ($dirent) found? ( type dirent )
   DUP dir>type @ ROT -
        ABORT" Wrong type of entry"
   DUP dir>base @   SWAP   dir>fence @  ;
: $open ( c-addr u -- blk fence )
   bffstype.file -ROT ($open) ;
FORTH-WORDLIST SET-CURRENT
: OPEN# ( "name" -- lo-blk hi-blk )
   PARSE-NAME $open   1- ;
: OPEN% ( "name" -- blk nblks )   PARSE-NAME $open OVER - ;
: OPEN ( "name" -- blk )   PARSE-NAME $open DROP ;


  \ blkfilefs
\ Create directory
blkfile-private-wid SET-CURRENT
: (initdir) ( nblk block -- )   bffstype.dir (inithead) ;
: $mkdir ( nblk c-addr u -- )   ($mkent)
   >R 2DUP + R@ dir>fence !  
   bffstype.dir R> dir>type ! UPDATE
   2DUP (initdir)
   bffstype.dir_free (initbody)    DROP ;
FORTH-WORDLIST SET-CURRENT
: MKDIR ( nblk "name" -- )   PARSE-NAME $mkdir ;



\ File size
blkfile-private-wid SET-CURRENT
: ($filesize) ( c-addr u -- d )
   ($dirent) ?DUP IF
      dir>filesize 2@
   ELSE   -66 THROW   THEN  ;

: ($filesize!) ( d c-addr u -- )
   ($dirent) ?DUP IF
      dir>filesize 2!
   ELSE   -64   THEN  ;

FORTH-WORDLIST SET-CURRENT
: DELETE-FILE ( c-addr u -- ior )
   ($dirent) ?DUP IF
      bffstype.file_free SWAP dir>type !
      UPDATE FLUSH   0
   ELSE   -64   THEN  ;

: RENAME-FILE ( c-addr1 u1 c-addr2 u2 -- ior )
   2SWAP ($dirent) ?DUP IF
      dir>name PLACE   0
      UPDATE FLUSH
   ELSE
   2DROP      -72 THEN ;

: RESIZE-FILE ( ud fileid -- ior ) 
   2DROP DROP       -74 ;

: FILE-STATUS ( c-addr u -- x ior )
   ($dirent)  DUP IF  0 ELSE  -67 THEN  ;


