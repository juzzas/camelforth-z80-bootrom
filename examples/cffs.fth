: STRUCT 0 ;  ( start a new structure )
: FIELD ( # n ++ #'  define a field with offset # and size n )
    CREATE OVER , +
    DOES> @ + ;  ( addr1 -- addr2 ; calculate address of field )
: END-STRUCT ( # "name" -- ) CONSTANT ;











( camelforth file-system -- constants                    1 / n )
4085 CONSTANT FFS_MAGIC  ( 0x0ff5 )
0 CONSTANT FFS_NULL
1 CONSTANT FFS_SUPERBLOCK
2 CONSTANT FFS_DIRECTORY
3 CONSTANT FFS_INODE
4 CONSTANT FFS_FREEBLOCKS

( camelforth file-system -- dirent                       1 / n )

STRUCT
    CELL VAR dirent-ino
    14 CHARS VAR dirent-name
END-STRUCT %dirent





( camelforth file-system -- node                         1 / n )
STRUCT
    CELL FIELD ffsnode-magic
    CELL FIELD ffsnode-type
END-STRUCT %ffsnode

: /FFSNODE ( ffstype ffsnode -- )
    DUP B/BLK ERASE
    FFS_MAGIC OVER ffsnode-magic !
    ffsnode-type !  ;









( camelforth file-system -- superblock                  1 / n )
STRUCT
    CELL VAR superblock-nblocks
    CELL VAR superblock-nfreeblocks
    CELL VAR superblock-freeblock
    dirent% 32 * VAR superblock-dirents
END-STRUCT %ffssuperblock









( camelforth file-system -- directory                   1 / n )
ffsnode% CLASS
    dirent% 32 * VAR ffsdir-dirents
END-CLASS ffsdir%









( camelforth file-system -- inode                       1 / n )
ffsnode% CLASS
    CELL VAR ffsinode-nblocks
    256 CELLS VAR ffsinode-blocks
END-CLASS ffsinode%














( camelforth file-system -- freeblocks                  1 / n )
ffsnode% CLASS
    CELL VAR ffsfreeblocks-next
    CELL VAR ffsfreeblocks-nblocks
    CELL VAR ffsfreeblocks-index
    256 CELLS VAR ffsfreebocks-blocks
END-CLASS ffsfreeblocks%















( camelforth filesystem )
( camelforth file-system -- dirent                      1 / n )















( camelforth file-system                                1 / n )

: MKFS  ( blk #blks -- )  ( create a filesystem at blk )   ;
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

: USING ( "name" -- )  ( use a file as virtual blocks )   ;
: VBLOCK> ( vblk inode -- blk )  ( convert raw block num )   ;