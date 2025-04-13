\ file-access wordset

\ requires blkfile and blkfs wordsets
CR .( Loading file access words... )


1 6 +THRU









   \ file-access wordset: DELETE-FILE RENAME-FILE RESIZE-FILE
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

