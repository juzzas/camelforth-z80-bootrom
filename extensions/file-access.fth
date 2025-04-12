\ file-access wordset

\ requires blkfile and blkfs wordsets
CR .( Loading file access words... )
30 VALUE DEFAULT-FILESIZE

1 6 +THRU









   \ file-access wordset: OPEN-FILE CLOSE-FILE  CREATE-FILE
: (OPEN-FILE) ( c-addr u fam -- fileid ) 
    >R $open R>  OPEN-BLKFILE  ;
: OPEN-FILE ( c-addr u fam -- fileid ior ) 
    ['] (OPEN-FILE) CATCH  DUP IF 0 SWAP THEN ;


: (CREATE-FILE) ( c-addr u fam -- fileid )
   >R DEFAULT-FILESIZE -ROT $creat  R> ( blk fence fam )
   OPEN-BLKFILE   ;
: CREATE-FILE ( c-addr u fam -- fileid ior )
    ['] (CREATE-FILE) CATCH  DUP IF >R 0 R> THEN ;


   \ file-access wordset: DELETE-FILE RENAME-FILE RESIZE-FILE
: DELETE-FILE ( c-addr u -- ior )
   2DROP            -64  ;

: RENAME-FILE ( c-addr1 u1 c-addr2 u2 -- ior )
   2DROP 2DROP      -72 ;

: RESIZE-FILE ( ud fileid -- ior ) 
   2DROP DROP       -74 ;


: INCLUDE ( i * x "name" -- j * x ) 
   OPEN  INCLUDE-BLKFILE  ;

: INCLUDED ( i * x c-addr u -- j * x ) 
   $open DROP INCLUDE-BLKFILE  ;

: REQUIRE ( i * x "name" -- i * x )
   OPEN  INCLUDE-BLKFILE  ;

: REQUIRED ( i * x c-addr u -- i * x )
   $open DROP INCLUDE-BLKFILE  ;

: (    \ multi-line comment
   BEGIN
      BEGIN
         PARSE-NAME  DUP
      WHILE
         1 =   SWAP   C@ ')' =  AND   IF   EXIT   THEN
      REPEAT  2DROP
      REFILL 0=
   UNTIL  ;  IMMEDIATE

