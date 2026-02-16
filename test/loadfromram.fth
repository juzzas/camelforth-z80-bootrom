90 LOAD    \ load assembler
927 LOAD   \ load pagedram wordset

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
   REPEAT  DROP
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
      0 FALSE
   THEN  ;

BEGIN-STRUCTURE source-ctx%
   SOURCE% +FIELD   source.source
   FIELD:   source.handle
END-STRUCTURE

: source-init ( refill-xt refetch-xt ctx -- )
   /SOURCE  ;


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

CREATE loadram-source  source-ctx%

: loadram-refill  ( -- f )
   loadbuffer  pr-to-string
   DUP >R  IF  ( chars )
      loadbuffer SWAP 'SOURCE 2!
      0 >IN !
   ELSE
      DROP
   THEN   R>   ;

: load-from-ram  ( -- )
   ." LOADing from paged ram " CR
   ['] loadram-refill 0 loadram-source source-init
   0 TO pr-ptr
   loadram-source  source-input
   ." LOADed from paged ram " CR
;

FORTH-WORDLIST SET-CURRENT

: RAMLOAD  ( blk -- )
   ." RAMLOADing from block " DUP U.  CR
   R/O OPEN-BLKFILE   ( blkfile-id )
   ?DUP IF 
      DUP copy-blkfile-to-ram
      CLOSE-BLKFILE  THROW
      load-from-ram
   ELSE
      ." Unable to open file" CR
   THEN
   ." RAMLOAD complete" CR
;

ONLY FORTH DEFINITIONS
