include ./ffl/tis.fs
include ./ffl/tos.fs
include ./ffl/str.fs
include ./ffl/snl.fs
include ./ffl/sni.fs

0 VALUE fd-in
0 VALUE fd-out

256 CONSTANT MaxLine
CREATE line-buffer MaxLine ALLOT


\ define string list 

begin-structure strnode%   ( -- n = Get the required space for a string node node )
  snn% 
  +field strnode>snn            \ Extend the snn structure with ..
  str% +field strnode>str       \ .. a str
end-structure

: strnode-init     ( c-addr u strnode -- = Initialise the node with cell data x )
  dup  snn-init
       strnode>str str-init
;


: strnode-new      ( c-addr u -- strnode = Create a new node on the heap with cell data x )
   strnode% allocate  throw  dup >r strnode-init
   r@  strnode>str   str-set
   r>  ;


: strnode-free     ( strnode -- = Free the node from the heap )
  dup strnode>str  str-free
  free throw
;

: strnode-compare  ( strnode strnode -- -1, 0, 1 = compare node )
   >r strnode>str   r> strnode>str    str^icompare  ;


\ string list definitions

begin-structure strlist%       ( -- n = Get the required space for a scl variable )
  snl% 
  +field  strlist>snl            \ Extend the base list with ..
  field:  strlist>compare        \ .. a compare token
end-structure


( List creation, initialisation and destruction )

: strlist-init     ( strlist -- = Initialise the scl list )
  dup snl-init
  ['] strnode-compare swap strlist>compare ! 
;


: strlist-(free)   ( strlist -- = Free the nodes from the heap )
  ['] strnode-free swap snl-(free)
;


: strlist-create   ( "<spaces>name" -- ; -- scl = Create a named scl list in the dictionary )
  create   here   strlist% allot   strlist-init
;


: strlist-new      ( -- scl = Create a new scl list on the heap )
  strlist% allocate  throw  dup strlist-init
;


: strlist-free     ( strlist -- = Free the list from the heap )
  dup strlist-(free)
  free  throw
;

: strlist-append   ( c-addr u strlist -- = append a new string to the list )
   >r strnode-new r> snl-append
;

: strlist-sort ( strlist -- )
   ['] strnode-compare swap snl-sort
;


: strlist-dump ( strlist -- = dump the contents of strlist )
   snl-dump ;

\ extra convenience definitions

: str-define  ( c-addr u  "<spaces>name" -- )
   create   here   str% allot   str-init
   str-set ; 


strlist-new value source-files

strlist-new value glossary-core-list
strlist-new value glossary-ext-list
strlist-new value glossary-utils-list
strlist-new value glossary-private-list

source-files sni-create source-file-iter
0 VALUE stream-in


: tis-reader  ( fileid -- c-addr u | 0 )
  line-buffer MaxLine ROT READ-FILE THROW
  DUP IF
    line-buffer SWAP
  THEN
;

: open-input ( str-filename -- )  
   str-get R/O OPEN-FILE THROW DUP TO fd-in
   tis-new TO stream-in
   ['] tis-reader   stream-in tis-set-reader  ;
: close-input ( -- )  stream-in tis-free fd-in CLOSE-FILE THROW 
   0 TO stream-in ;

: open-output ( addr u -- )  W/O CREATE-FILE THROW TO fd-out ;
: close-output ( -- )  fd-out CLOSE-FILE THROW ;


: ?slurp-glossary-core  ( tis -- )
   >R  S" ;C " R@ tis-scan-string
   IF
      2DROP r@ tis-get   glossary-core-list strlist-append
   THEN
   R> DROP
;

: ?slurp-glossary-ext  ( tis -- )
   >R  S" ;X " R@ tis-scan-string
   IF
      2DROP r@ tis-get   glossary-ext-list strlist-append
   THEN
   R> DROP
;

: ?slurp-glossary-utils  ( tis -- )
   >R  S" ;U " R@ tis-scan-string
   IF
      2DROP r@ tis-get   glossary-utils-list strlist-append
   THEN
   R> DROP
;

: ?slurp-glossary-private  ( tis -- )
   >R  S" ;Z " R@ tis-scan-string
   IF
      2DROP r@ tis-get   glossary-private-list strlist-append
   THEN
   R> DROP
;

: (slurp-glossary-line) ( c-addr u -- )
   tis-new DUP >R tis-set
   R@ ?slurp-glossary-core
   R@ ?slurp-glossary-ext
   R@ ?slurp-glossary-utils
   R@ ?slurp-glossary-private
   R> tis-free
;

: (slurp-glossary)   ( -- )
   BEGIN
      stream-in  tis-eof?  0=
   WHILE
      stream-in tis-read-line 
      ?dup   IF (slurp-glossary-line) THEN
   REPEAT
;

: open-glossary:  ( "filename" -- )
   BL PARSE open-output ; IMMEDIATE

: (add-source-file)  ( c-addr u -- )
   2DUP ." adding source: "   TYPE CR
   source-files strlist-append
;

: add-source-file:  ( "filename" -- )
   BL PARSE (add-source-file)
; IMMEDIATE

: (gen-glossary)   ( str-file -- )
   dup str-get ." Slurping: "  type cr
   open-input
   (slurp-glossary)
   close-input
;

: gen-glossary ( -- )
   source-file-iter  sni-first
   ?DUP IF
      BEGIN
         strnode>str (gen-glossary)
         source-file-iter  sni-next
         ?DUP 0=
      UNTIL
   THEN
;

: clear-glossary ( strlist -- )
   BEGIN
      dup snl-empty?  0=
   WHILE
      dup snl-remove-first
          ?dup if ( ." removing "  strnode>str str-get type cr ) drop then
   REPEAT
   DROP
;

: dump-glossary ( strlist -- )
   sni-new DUP >R  sni-first
   ?DUP IF
      BEGIN
         strnode>str str-get TYPE CR
         R@  sni-next
         ?DUP 0=
      UNTIL
   THEN
   R> DROP
;

: sort-glossary ( strlist -- )
   dup  snl-length@  IF
      strlist-sort
   ELSE
      drop
   THEN
;

: output-glossary  ( strlist -- )
   DUP snl-length@  ." - " .  ." words." cr
   DUP sort-glossary
   dump-glossary
;

: output-glossary-core  ( -- )
   ." Generating Core glossary..." CR
   glossary-core-list output-glossary
   glossary-core-list clear-glossary
;

: output-glossary-extensions  ( -- )
   ." Generating Extensions glossary..." CR
   glossary-ext-list output-glossary
   glossary-ext-list clear-glossary
;

: output-glossary-utils  ( -- )
   ." Generating Utils glossary..." CR
   glossary-utils-list output-glossary
   glossary-utils-list clear-glossary
;

: output-glossary-private  ( -- )
   ." Generating Private glossary..." CR
   glossary-private-list output-glossary
   glossary-private-list clear-glossary
;

: close-glossary  ( -- )
   close-output 
;


