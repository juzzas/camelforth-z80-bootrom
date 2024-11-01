\ file-access wordset


1 CONSTANT BIN 
2 CONSTANT R/O
4 CONSTANT R/W

: OPEN-FILE ( c-addr u fam -- fileid ior ) ;
: CLOSE-FILE ( fileid -- ior );
: CREATE-FILE ( c-addr u fam -- fileid ior ) ;
: DELETE-FILE ( c-addr u fam -- ior ) ;
: RENAME-FILE ( c-addr1 u1 c-addr2 u2 -- ior ) ;
: RESIZE-FILE ( u fileid -- fileid ior ) ;

: READ-FILE ( c-addr u fileid -- u ior ) ;
: READ-LINE ( c-addr u fileid -- u f ior ) ;
: WRITE-FILE ( c-addr u fileid -- ior ) ;
: WRITE-LINE ( c-addr u fileid -- ior ) ;

: FILE-POSITION ;
: REPOSITION-FILE ;
: FILE-SIZE ;
: FILE-STATUS ;

: INCLUDE-FILE ;
: INCLUDE ;
: INCLUDED ;
: REQUIRE ;
: REQUIRED ;

