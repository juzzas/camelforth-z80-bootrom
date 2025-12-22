\ file-access wordset

\ requires blkfile and blkfs wordsets
CR .( Loading file access words... )

ONLY FORTH DEFINITIONS
1 6 +THRU
ONLY FORTH DEFINITIONS








   \ file-access wordset: OPEN-FILE CLOSE-FILE  CREATE-FILE
blkfile-private-wid >ORDER   blkfile-private-wid SET-CURRENT
: (OPEN-FILE) ( c-addr u fam -- fileid ) 
    >R 2DUP $open R>  OPEN-FENCE-BLKFILE   ( c-addr u blkfid )
    >R 2DUP R@  blkfile.name PLACE
    ($filesize)  R>  blkfile.filesize 2!
;

FORTH-WORDLIST SET-CURRENT
: OPEN-FILE ( c-addr u fam -- fileid ior ) 
    ['] (OPEN-FILE) CATCH  DUP IF >R 0 R> THEN ;

30 VALUE DEFAULT-FILESIZE



blkfile-private-wid SET-CURRENT
: (CREATE-FILE)  ( c-addr u fam -- fileid )
   >R DEFAULT-FILESIZE -ROT $creat  R> ( blk fence fam )
   OPEN-FENCE-BLKFILE   ;

FORTH-WORDLIST SET-CURRENT
: CREATE-FILE ( c-addr u fam -- fileid ior )
    ['] (CREATE-FILE) CATCH  DUP IF >R 0 R> THEN ;








   \ file-access wordset: CLOSE-FILE
: (CLOSE-FILE)  ( fileid -- )
   DUP >R blkfile.filesize 2@  
   R@ blkfile.name COUNT ($filesize!)
   R> (CLOSE-BLKFILE) ;

: CLOSE-FILE  ( fileid -- ior )
   ['] (CLOSE-FILE) CATCH
   DUP IF NIP THEN ;







   \ file-access wordset: DELETE-FILE RENAME-FILE RESIZE-FILE
blkfile-private-wid SET-CURRENT
WORDLIST CONSTANT INCLUDED-WID

: add-included-name  ( c-addr u -- )
    INCLUDED-WID (CREATE-WID) ;

: name-not-included? ( c-addr u -- f )
    INCLUDED-WID SEARCH-WORDLIST 0= ;

FORTH-WORDLIST SET-CURRENT
: INCLUDED ( i * x c-addr u -- j * x ) 
   2DUP name-not-included?  IF
       2DUP add-included-name
   THEN
   $open DROP  ( blk )    INCLUDE-BLKFILE  ;

: INCLUDE ( i * x "name" -- j * x ) 
   PARSE-NAME INCLUDED ;


: REQUIRED ( i * x c-addr u -- i * x )
   2DUP name-not-included?  IF
       2DUP add-included-name
       $open DROP INCLUDE-BLKFILE
   ELSE 2DROP THEN  ;

: REQUIRE ( i * x "name" -- i * x )
   PARSE-NAME REQUIRED  ;


: (    \ multi-line comment
   BEGIN
      BEGIN
         PARSE-NAME  DUP
      WHILE
         1 =   SWAP   C@ ')' =  AND   IF   EXIT   THEN
      REPEAT  2DROP
      REFILL 0=
   UNTIL  ;  IMMEDIATE

