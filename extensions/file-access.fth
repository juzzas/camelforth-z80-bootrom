\ file-access wordset

\ requires blkfile and blkfs wordsets
CR .( Loading file access words... )


1 6 +THRU









   \ file-access wordset: DELETE-FILE RENAME-FILE RESIZE-FILE
WORDLIST CONSTANT INCLUDED-WID

: add-included-name  ( c-addr u -- )
    INCLUDED-WID (CREATE-WID) ;

: name-not-included? ( c-addr u -- f )
    INCLUDED-WID SEARCH-WORDLIST 0= ;

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

