( blkfs - extension to treat blocks as a filesystem      0 / n)

CR .( Loading blkfs "filesystem"... )






ONLY FORTH DEFINITIONS
1 14 +THRU





\ blkfilefs

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
VARIABLE cwd   VARIABLE root


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
: MKFS ( base #blk -- )   2DUP SWAP bffstype.dir (inithead)
   SWAP bffstype.dir_free (initbody)   DROP ;
  \ blkfilefs
: dir>name  ( blkfilefs -- addr )    ;
: dir>base  ( blkfilefs -- addr )    #NAMECHARS +  ;
: dir>fence  ( blkfilefs -- addr )  #NAMECHARS + 2 + ;
: dir>type  ( blkfilefs -- addr )  #NAMECHARS + 4 + ;
#NAMECHARS 6 +  CONSTANT BLKFILEDIR-CONTEXT

BASESIZ BLKFILEDIR-CONTEXT /   CONSTANT #DIRFILES
: (cwd@) ( -- blk )   cwd @ DUP 0= ABORT" No CWD" ;
: (cwd>) ( -- ptr )   (cwd@) BLOCK ;
: (cwd>meta) ( -- ptr )   (cwd>) (>meta) ;
: (block>meta) ( blk -- ptr )   BLOCK (>meta) ;



  \ blkfilefs
: ($namecmp) ( source dest -- f ) COUNT ROT COUNT ROT
   MAX STRCMP 0= ;
: ($dirent) ( str -- dirent | 0 )   (cwd>)   ( str blk-addr )
   #DIRFILES 0 ?DO   DUP dir>base @ 0= IF
       UNLOOP 2DROP 0 EXIT THEN
   2DUP dir>name ($namecmp)   IF  UNLOOP NIP EXIT  THEN
   BLKFILEDIR-CONTEXT + LOOP   2DROP 0 ;
: ($lookup) ( str -- block | 0 )   ($dirent) DUP 0= IF
      EXIT   THEN
   dir>base @ ;
: cwd! ( blk -- )   DUP (block>meta)
   meta>type @ bffstype.dir -  ABORT" Not dir"
   root @ 0= IF DUP root ! THEN   cwd ! ;
: found? ( blk -- blk )   DUP 0= ABORT" Not found" ;

  \ blkfilefs
: $cd ( s -- )   ($lookup) found?   cwd! ;

: CD ( <name> )   BL WORD $cd ;
: CD/ ( -- )   root @ DUP 0= ABORT" No root"   cwd! ;
: MOUNT   DUP cwd!   root ! ;










  \ blkfilefs
: (slot) ( blkptr -- dirptr )   #DIRFILES 0 DO
      DUP dir>base @ 0= IF   UNLOOP EXIT   THEN
   BLKFILEDIR-CONTEXT  + LOOP   1 ABORT" Dir full" ;
: (blkallot) ( nblk metaptr -- )   TUCK meta>here @ +
   OVER meta>fence @ OVER < ABORT" Out of space"
   SWAP meta>here ! ;
: ($namecopy) ( source dest ) SWAP COUNT ROT PLACE ;
: ($mkent) ( nblk name -- nblk blk dirent )
   OVER 0< ABORT" Bad size"   DUP ($dirent) ABORT" Exists"
   SWAP TUCK (cwd>)  ( nblk name nblk blkptr )  DUP (>meta) >R
   (slot) ROT OVER dir>name ($namecopy)
                                    ( nblk dirent R: metaptr )
   R@ meta>here @ -ROT   SWAP R> (blkallot)  
                                          ( nblk here dirent )
   OVER -ROT DUP >R dir>base ! R>  UPDATE ;
  \ blkfilefs
: (initfile) ( nblk block -- )
    SWAP BOUNDS ?DO I WIPE UPDATE LOOP ;

: $creat ( nblk name -- block ) 
   ($mkent)  ( nblk blk dirent )
   >R 2DUP + R@ dir>fence !  
   bffstype.file R> dir>type ! UPDATE
   (initfile)  ;

: CREAT   BL WORD $creat ;





  \ blkfilefs
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
   meta>fence @ R> - 4 U.R    2 SPACES
   dir>name COUNT TYPE  CR ;

: .fileentry ( dirent blkno -- )
   >R ( dirent R: blkno )
   DUP dir>type @ (.type)   R@ 6 U.R  6 SPACES
   DUP dir>fence @ R> - 4 U.R   2 SPACES
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

: .head ( -- )   CR ." Type     Start   Length  Name" CR ;
: .ls ( -- )   .head   0   ['] (.ls)   entriesDo ;

: LS   .ls ;


  \ blkfilefs
\ Return "open" file
: ($open) ( type str -- blk )   
   ($dirent) found? ( type dirent )
   DUP dir>type @ ROT - 
        ABORT" Wrong type of entry"  dir>base @ ;
: $open ( str -- blk )   bffstype.file SWAP ($open) ;

: OPEN ( -- blk )   BL WORD $open ;
: OPEN# ( -- blklow blkhigh )   
   OPEN   DUP (block>meta) meta>here @ 1- ;





  \ blkfilefs
\ Create directory
: (initdir) ( nblk block -- )   bffstype.dir (inithead) ;
: $mkdir ( nblk name -- )   ($mkent)
   >R 2DUP + R@ dir>fence !  
   bffstype.dir R> dir>type ! UPDATE
   2DUP (initdir)
   bffstype.dir_free (initbody)    DROP ;

: MKDIR ( nblk spaces"ccc" -- )   BL WORD $mkdir ;


