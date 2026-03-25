ONLY FORTH ALSO SYSTEM
blkfile-private-wid >ORDER


128 CONSTANT loadbuffer%

loadbuffer% BUFFER: loadbuffer
0 VALUE pr-ptr
0 VALUE pr-end

: PR-PLACE ( src n dst --  )    \    copy to counted str in paged ram
   2DUP PRC! OVER IF
      CHAR+ SWAP PRMOVE
   ELSE  2DROP DROP  THEN  ;

: PR-COUNT  ( c-addr -- c-addr n )
   DUP PRC@  >R CHAR+ R> ;

: string-to-pr ( c-addr u -- )
   DUP >R pr-ptr PR-PLACE   R>
   pr-ptr + 1+ TO pr-ptr  ;

: copy-blkfile-to-ram   ( blkfileid -- )
   0 TO pr-ptr
   BEGIN
     DUP loadbuffer  loadbuffer%  ROT
          (READ-LINE)   ( blkfile-id chrs f )
   WHILE       ( blkfile-id chrs )
     loadbuffer  SWAP  string-to-pr
   REPEAT  2DROP
   pr-ptr TO pr-end
;

: pr-to-string  ( dest --  u f )
   pr-ptr pr-end < IF
      pr-ptr PR-COUNT  DUP >R   ( dest src len  r: len )
      DUP IF  ROT SWAP PRMOVE  ELSE
        2DROP DROP THEN
      pr-ptr R@ + 1+  TO pr-ptr
      R> TRUE
   ELSE
      DROP 0 FALSE
   THEN  ;


: source-input  ( source-ctx -- )
   SAVE-INPUT N>R
   SET-SOURCE
   BEGIN
      REFILL
   WHILE
       \ SOURCE TYPE  CR   \ debug print of line
       INTERPRET
   REPEAT
   NR> RESTORE-INPUT DROP  ;

: loadram-refill  ( -- f )
   loadbuffer  pr-to-string
   DUP >R  IF  ( chars )
      loadbuffer SWAP 'SOURCE 2!
      0 >IN !
   ELSE
      DROP
   THEN   R>   ;

: loadram-getpos ( -- d )   pr-end pr-ptr ;
: loadram-setpos ( d -- )   TO pr-ptr   TO pr-end ;

CREATE loadram-source  SOURCE% ALLOT
   ' loadram-refill loadram-source SOURCE>REFILL !
   ' loadram-getpos loadram-source SOURCE>GETPOS !
   ' loadram-setpos loadram-source SOURCE>SETPOS !


: load-from-ram  ( -- )
   CR ." LOADing from paged ram"
   0 TO pr-ptr
   loadram-source  source-input
   CR ." LOADed from paged ram"
;

FORTH-WORDLIST SET-CURRENT

: RAMLOAD  ( blk -- )
   CR ." RAMLOADing from block " DUP U.
   R/O OPEN-BLKFILE   ( blkfile-id )
   ?DUP IF 
      DUP copy-blkfile-to-ram
      CLOSE-BLKFILE  THROW
      load-from-ram
   ELSE
      CR ." Unable to open file"
   THEN
   CR ." RAMLOAD complete"
;

ONLY FORTH DEFINITIONS
