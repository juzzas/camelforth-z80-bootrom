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


: (CLOSE-FILE) ( fileid -- )
   CLOSE-BLKFILE   ;
: CLOSE-FILE    ( fileid -- ior )
    ['] (CLOSE-FILE) CATCH  ;


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

: READ-FILE ( c-addr u fileid -- u ior ) 
   ['] READ-BLKFILE CATCH
   DUP IF >R 0 R> THEN ;

: READ-LINE ( c-addr u fileid -- u f ior ) 
   ['] READLINE-BLKFILE CATCH
   DUP  IF >R 0 0 R> THEN ;

: WRITE-FILE ( c-addr u fileid -- ior ) 
   ['] WRITE-BLKFILE  CATCH ;

: WRITE-LINE ( c-addr u fileid -- ior ) 
   ['] WRITELINE-BLKFILE CATCH ;

: FILE-POSITION  ( fileid -- ud ior )
   DROP        0 0  -65 ;

: REPOSITION-FILE ( ud fileid -- ior )
   DROP 2DROP      -73 ;

: FILE-SIZE ( fileid -- ud ior )
   DROP        0 0 -66 ;

: FILE-STATUS ( c-addr u -- x ior )
   2DROP       0   -67 ;

: FLUSH-FILE ( fileid -- ior ) 
   DROP FLUSH   0 ;

: INCLUDE-FILE ( i * x fileid -- j * x )  
   DROP         ;

: INCLUDE ( i * x "name" -- j * x ) 
   OPEN  TLOAD  ;

: INCLUDED ( i * x c-addr u -- j * x ) 
   $open DROP TLOAD  ;

: REQUIRE ( i * x "name" -- i * x )
   OPEN  TLOAD  ;

: REQUIRED ( i * x c-addr u -- i * x )
   $open DROP TLOAD  ;

: (    \ multi-line comment
   BEGIN
      BEGIN
         PARSE-NAME  DUP
      WHILE
         1 =   SWAP   C@ ')' =  AND   IF   EXIT   THEN
      REPEAT  2DROP
      REFILL 0=
   UNTIL  ;  IMMEDIATE

