ONLY FORTH

MARKER __ffs__

: STRUCT 0 ;  ( start a new structure )
: FIELD ( # n ++ #'  define a field with offset # and size n )
    CREATE OVER , +
    DOES> @ + ;  ( addr1 -- addr2 ; calculate address of field )
: END-STRUCT ( # "name" -- ) CONSTANT ;


VOCABULARY FFS
ONLY FORTH ALSO FFS DEFINITIONS



( camelforth file-system -- constants                    1 / n )
4085 CONSTANT FFS_MAGIC  ( 0x0ff5 )
0 CONSTANT FFS_NULL
1 CONSTANT FFS_SUPERBLOCK
2 CONSTANT FFS_DIRECTORY
3 CONSTANT FFS_INODE
4 CONSTANT FFS_FREEBLOCKS

256 CONSTANT FFSINODE-MAX-BLOCKS

VARIABLE FFS.ROOT    0 FFS.ROOT !
VARIABLE FFS.CWD     0 FFS.CWD !




( camelforth file-system -- dirent                       1 / n )

STRUCT
    CELL FIELD dirent-ino
    14 CHARS FIELD dirent-name
END-STRUCT %dirent    ( 16 bytes )










( camelforth file-system -- node                         1 / n )
STRUCT
    CELL FIELD ffsnode-magic
    CELL FIELD ffsnode-type
    CELL FIELD ffsnode-crc
    CELL FIELD ffsnode-reserved
END-STRUCT %ffsnode

: /FFSNODE ( ffstype ffsnode -- )
    DUP B/BLK ERASE
    FFS_MAGIC OVER ffsnode-magic !
    ffsnode-type !  ;
: FFSNODE? ( ffstype ffsnode -- flag )  ( is node valid? )
    DUP ffsnode-magic @ FFS_MAGIC =  ( ffstype ffsnode f )
    SWAP ffsnode-type @  ( ffstype f type )   ROT  =  AND  ;



( camelforth file-system -- superblock                  1 / n )
32 CONSTANT #SUPER-DIRENTS
STRUCT
    %ffsnode FIELD super-node
    CELL FIELD super-nblocks
    CELL FIELD super-nfreeblocks
    CELL FIELD super-freeblock
    %dirent #SUPER-DIRENTS * FIELD super-dirents
END-STRUCT %ffssuper







: /FFSSUPER ( ffssuper -- )
    DUP   FFS_SUPERBLOCK SWAP /FFSNODE
    0 super-nblocks !
    0 super-nfreeblocks !
    0 super-freeblock !
    #SUPER-DIRENTS 0 DO
        DUP %dirent I *  +  0 SWAP !
    LOOP  ;
: FFSSUPER?  (ffssuper -- )
    DUP FFS_SUPERBLOCK /FFSNODE
    FFSNODE?  ;







: FFS-BLKFREE ( blk ffssuper -- )
    DUP   FFS_SUPERBLOCK SWAP /FFSNODE
    0 super-nblocks !
    0 super-nfreeblocks !
    0 super-freeblock !
    #SUPER-DIRENTS 0 DO
        DUP %dirent I *  +  0 SWAP !
    LOOP  ;








( camelforth file-system -- ffsinode                     1 / n )
STRUCT
    %ffsnode FIELD free-node
    CELL FIELD ffsinode-next
    CELL FIELD ffsinode-head
    CELL FIELD ffsinode-tail
    FFSINODE-MAX-BLOCKS CELLS FIELD ffsinode-blocks
END-STRUCT %ffsinode








: /FFSFREE ( ffsfree -- )
    DUP   FFS_FREEBLOCKS SWAP /FFSNODE
    0 OVER ffsinode-next !
    0 ffsinode-blocks OVER ffsinode-head !
    DUP ffsinode-head @ OVER ffsinode-tail !  ;

: FFSINODE-EMPTY? ( ffsinode -- flag )  ( is inode empty? )
    DUP ffsinode-head @
    SWAP ffsinode-tail @  =  ;

: FFSINODE-FULL? ( ffsinode -- flag )  ( is inode full? )
    ffsinode-tail @ FFSINODE-MAX-BLOCKS < INVERT ;




( camelforth file-system -- ffsinode                     1 / n )
: FFSINODE-PUSH ( blk ffsinode -- )  ( push a blk to blk list )
    >R      ( save inode addr )
    R@ ffsinode-tail @  1 CELLS +  ( get tail and increment )
    R@ ffsinode-tail !        ( set tail )

    R@ ffsinode-tail @  R>  ffsinode-blocks +  !
    ;








( camelforth file-system -- ffsinode                     1 / n )
: FFSINODE-POP ( ffsinode -- blk )  ( pop a blk from blk list )
    DUP  FFSINODE-EMPTY?   IF
        DROP 0  ( return null )
    ELSE
        DUP ffsinode-head @  OVER ffsinode-blocks +  @
        SWAP
        DUP ffsinode-head @  1 CELLS +  ( increment head )
        DUP ffsinode-head !  ( set new tail )
    THEN  ;








( camelforth file-system -- mount a file system at blk   1 / n )
: MOUNT ( blk -- )  ( set blk as the superblock )
   DUP BLOCK FFS_SUPERBLOCK SWAP FFSNODE? IF
       FFS.ROOT !  ( set root directory )
   ELSE
       ." Invalid superblock" CR
   THEN  ;

: MKFS  ( #blks blk -- )  ( create a filesystem at blk )
    DUP BLOCK >R /FFSSUPER            ( create superblock )
    DUP super-free
    DUP 1+ BLOCK /FFSFREE UPDATE   ( create free nodes )
    0 DO I /FFSINODE-PUSH  LOOP  ( create free nodes );










( camelforth file-system -- directory                   1 / n )
STRUCT
    %ffsnode FIELD dir-node
    %dirent 32 * FIELD ffsdir-dirents
END-STRUCT %ffsdir











( camelforth file-system -- inode                       1 / n )
STRUCT
    %ffsnode FIELD inode-node
    CELL FIELD ffsinode-nblocks
    256 CELLS FIELD ffsinode-blocks
END-STRUCT %ffsinode










( camelforth file-system -- dirent                      1 / n )















( camelforth file-system                                1 / n )

: MOUNT ( blk -- )  ( set blk as the superblock )   ;


: CD   ( "name" -- )  ( change directory )   ;
: CD\   ( -- )  ( change to root directory )   ;
: LS   ( -- )  ( list directory )   ;
: MKDIR ( "name" -- )  ( make a directory )   ;
: RMDIR ( "name" -- )  ( remove a directory )   ;
: RM    ( "name" -- )  ( remove a file )   ;
: CAT   ( "name" -- )  ( print a file )   ;
: TOUCH ( "name" -- )  ( create a file )   ;
: CP    ( "name" "name" -- )  ( copy a file )   ;
: MV    ( "name" "name" -- )  ( move a file )   ;
( camelforth file-system                                1 / n )
: USING ( "name" -- )  ( use a file as virtual blocks )   ;
: VBLOCK> ( vblk inode -- blk )  ( convert raw block num )   ;