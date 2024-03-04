: STRUCT 0 ;  ( start a new structure )
: FIELD ( # n ++ #'  define a field with offset # and size n )
    CREATE OVER , +
    DOES> @ + ;  ( addr1 -- addr2 ; calculate address of field )
: END-STRUCT ( # "name" -- ) CONSTANT ;











( Block filesystem -- based on vandys )
VOCABULARY FS
ONLY FORTH ALSO   FS DEFINITIONS
4085 CONSTANT FSMAGIC  ( 0x0ff5 )
32 CONSTANT #NAMECHARS
992 CONSTANT BASESIZ

STRUCT
    1 CELLS FIELD meta>magic     1 CELLS FIELD meta>type
    1 CELLS FIELD meta>fence     1 CELLS FIELD meta>here
END-STRUCT %meta

0 CONSTANT FST_dir
1 CONSTANT FST_file
2 CONSTANT FST_dir_free
3 CONSTANT FST_file_free
( Block filesystem -- based on vandys )
: (magic) ( ptr -- )   FSMAGIC SWAP   meta>magic !   UPDATE ;
: ((>meta))   BASESIZ + ;
: (>meta) ( ptr -- ptr' )   ((>meta))
   DUP meta>magic @ FSMAGIC - ABORT" CORRUPT BLOCK" ;
: (initfree) ( blk type -- )   SWAP BUFFER   DUP B/BLK ERASE
   ((>meta))   TUCK meta>type !   (magic) ;
: (inithead) ( #blk blk type -- )   >R   DUP BUFFER   DUP B/BLK
   ERASE ((>meta))   R> OVER meta>type !   >R TUCK + R> TUCK
   meta>fence !  SWAP 1+ OVER meta>here !   (magic) ;
: (initbody) ( #blk blk type -- blk )   ROT ROT TUCK TUCK + SWAP
    1+ DO OVER I SWAP (initfree)   LOOP NIP ;
: MKFS ( base #blk -- )   2DUP SWAP FST_dir (inithead)
   SWAP FST_dir_free (initbody)   DROP ;


(                                                    vandys  )
STRUCT
    #NAMECHARS CHARS FIELD dir>name   1 CELL FIELD dir>base
END-STRUCT %dir
BASESIZ %dir /   CONSTANT #DIRFILES
VARIABLE cwd     0 cwd !
: (cwd@) ( -- blk )   cwd @ DUP 0= ABORT" NO CWD" ;
: (cwd>) ( -- ptr )   (cwd@) BLOCK ;
: (cwd>meta) ( -- ptr )   (cwd>) (>meta) ;
: (block>meta) ( blk -- ptr )   BLOCK (>meta) ;
: root   cwd CELL+ ;
: ($dirent) ( str -- dirent | 0 )   (cwd>)
   #DIRFILES 0 DO   DUP dir>base @ 0= IF
        UNLOOP 2DROP 0 EXIT THEN
      2DUP dir>name #NAMECHARS S= 0= IF   UNLOOP NIP EXIT   THEN
   %dir + LOOP   2DROP 0 ;
(                                                    vandys  )
: ($lookup) ( str -- block | 0 )   ($dirent) DUP 0=
    IF   EXIT   THEN    dir>base @ ;
: cwd! ( blk -- )   DUP (block>meta)
   meta>type @ FST_dir - ABORT" NOT DIR"
   root @ 0= IF DUP root ! THEN   cwd ! ;
: found? ( blk -- blk )   DUP 0= ABORT" NOT FOUND" ;
: $cd ( s -- )   ($lookup) found?   cwd! ;
: CD ( <name> )   32 WORD   $cd ;
: CD/ ( -- )   root @ DUP 0= ABORT" NO ROOT"   cwd! ;
: ROOT!   DUP cwd!   root ! ;




( creat 1 )
: ($strcpy) ( src dest -- )   OVER COUNT NIP 1+ MOVE  ;
: (slot) ( blkptr -- dirptr )   #DIRFILES 0 DO
      DUP dir>base @ 0= IF   UNLOOP EXIT   THEN
   dir.size + LOOP   1 ABORT" DIR FULL" ;
: (blkallot) ( nblk metaptr -- )   TUCK meta>here @ +
   OVER meta>fence @ OVER < ABORT" OUT OF SPACE"
   SWAP meta>here ! ;
: ($mkent) ( nblk name -- nblk blk )
   OVER 0< ABORT" BAD SIZE"   DUP ($dirent) ABORT" EXISTS"
   SWAP TUCK (cwd>)   ( nblk name nblk blkptr )   DUP (>meta) >R
   (slot) ROT OVER dir>name SWAP ($strcpy)
                                      ( nblk dirent R: metaptr )
   R@ meta>here @ ROT ROT   SWAP R> (blkallot)
                                            ( nblk here dirent )
   OVER ROT ROT dir>base !   UPDATE ;

( creat 2 )
: (blankbod) ( blk -- )   BUFFER   DUP BASESIZ BLANK
   [CHAR] \ SWAP C! ;
: (initfile) ( nblk block -- )   TUCK   FST_file (inithead)
   (blankbod)   UPDATE ;
: $creat ( nblk name -- block )   ($mkent)   2DUP (initfile)
   FST_file_free (initbody) ;
: CREAT   BL WORD $creat ;










