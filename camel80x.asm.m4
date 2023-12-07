; RC2014 Extension Words - 16K
;
; ===============================================
; CamelForth for the Zilog Z80
; Copyright (c) 1994,1995 Bradford J. Rodriguez
; Copyright (c) 2020 Justin Skists
;
; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 3 of the License, or
; (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program.  If not, see <http://www.gnu.org/licenses/>.

; Commercial inquiries should be directed to the author at 
; 115 First St., #105, Collingwood, Ontario L9Y 4W3 Canada
; or via email to bj@camelforth.com
;
; ===============================================
; RC2014 Extension Words
; 
;   Forth words are documented as follows:
;*   NAME     stack -- stack    description
;   Word names in upper case are from the ANS
;   Forth Core word set.  Names in lower case are
;   "internal" implementation words & extensions.
; ===============================================


SECTION code_user_16k

; RC2014 EXTENDED STRUCTURES ====================

;C CELL-    a-addr1 -- a-addr2    subtract cell size
;   2 - ;
    head(CELLMINUS,CELL-,docode)
        dec bc
        dec bc
        next

; http://www.forth.org/svfig/Len/softstak.htm

dnl ; During compilation, create a STACK (software stack) with n cells.
dnl ; On execution, return the address of the stack.
dnl ;     lifo+0 -> ptr to top of stack + 2
dnl ;     lifo+2 -> bottom of stack
dnl ; : STACK ( n -- ) ( -- adr)
dnl ;      CREATE HERE CELL+ , CELLS ALLOT
dnl ;   DOES>  ;
    head(STACK,STACK,docolon)
        DW CREATE,HERE,CELLPLUS,COMMA,CELLS,ALLOT
        DW XDOES
        call dodoes
        dw EXIT

; Push number onto STACK
; : >STACK ( n lifo -- )
;      SWAP OVER @ ! CELL SWAP +! ;
    head(TOSTACK,>STACK,docolon)
        DW SWOP,OVER,FETCH,STORE,CELL,SWOP,PLUSSTORE
        DW EXIT

; Pop number from STACK
; : STACK> ( lifo -- x )
;      CELL NEGATE    ( lifo -2 )
;      OVER           ( lifo -2 lifo )
;      +!             ( lifo )
;      @ ;
    head(STACKFROM,STACK>,docolon)
        DW CELL,NEGATE,OVER,PLUSSTORE,FETCH
        DW EXIT

; Fetch the value at the top of the STACK
; : STACK@ ( lifo -- x )
;      @ CELL- @ ;
    head(STACKFETCH,STACK@,docolon)
        DW FETCH,CELLMINUS,FETCH
        DW EXIT

; Replace the value at the top of the STACK
; : STACK! ( x lifo -- )
;      @ CELL- ! ;
    head(STACKSTORE,STACK!,docolon)
        DW FETCH,CELLMINUS,STORE
        DW EXIT

; Clear STACK
; : STACK.CLEAR ( lifo -- )
;      DUP    ( lifo lifo )
;      CELL+  ( lifo lifo+2 )
;      SWAP   ( lifo+2 lifo )
;      !     ;
    head(STACKCLEAR,STACK.CLEAR,docolon)
        DW DUP,CELLPLUS,SWOP,STORE
        DW EXIT

; : STACK-DEPTH ( lifo -- n )
;      STACK.BOUNDS - CELL /  ;
    head(STACKDEPTH,STACK.DEPTH,docolon)
        DW STACKBOUNDS,MINUS
        DW TWOSLASH      ;  optimize "DW CELL,SLASH" for 16bit
        DW EXIT

; : STACK.EMPTY? ( lifo -- flag )
;      STACK.BOUNDS = ;
    head(STACKEMPTYQ,STACK.EMPTY?,docolon)
        DW STACKBOUNDS,EQUAL
        DW EXIT

; Create parameters for a ?DO loop that will scan every item currently in STACK. The intended use is:
;      ( lifo )   STACK.BOUNDS ?DO -- CELL +LOOP
; : STACK.BOUNDS ( lifo -- addr1 addr2 )
;     DUP @ SWAP CELL+ ;
    head(STACKBOUNDS,STACK.BOUNDS,docolon)
        DW DUP,FETCH,SWOP,CELLPLUS
        DW EXIT

; Set the stack from the data stack
; : STACK.SET     ( rec-n .. rec-1 n lifo )
;    OVER IF
;      2DUP SWAP CELLS + CELL+ ( n lifo tos+2 )
;      DUP ROT !           ( ... n tos+2 )
;      SWAP 0 DO           ( rec-n rec-1 tos+2)
;         CELL- DUP >R ! R>   ( rec-n ... rec-i addr )
;      LOOP   DROP
;    ELSE
;      NIP STACK.CLEAR
;    THEN    ;
    head(STACKSET,STACK.SET,docolon)
        DW OVER,qbranch,STACKSET2
        DW TWODUP,SWOP,CELLS,PLUS,CELLPLUS
        DW DUP,ROT,STORE
        DW SWOP,lit,0,xdo
STACKSET1:
        DW CELLMINUS,DUP,TOR,STORE,RFROM
        DW xloop,STACKSET1
        DW DROP
        DW EXIT
STACKSET2:
        DW NIP,STACKCLEAR
        DW EXIT

; Get the STACK onto the data stack
; : STACK.GET  ( lifo -- rec-n .. rec-1 n )
;     DUP STACK.DEPTH    ( lifo n )
;     DUP IF             ( lifo n )
;         >R             ( lifo )
;         STACK.BOUNDS   ( addr1 addr2 )
;         DO              (  )
;             I @         ( rec-i )
;             CELL
;         +LOOP           ( rec-i )
;         R>              ( rec-i n )
;     ELSE                ( lifo n )
;         NIP             ( n )
;     THEN                ( )
;     ;
    head(STACKGET,STACK.GET,docolon)
        DW DUP,STACKDEPTH,DUP,qbranch,STACKGET2
        DW TOR,STACKBOUNDS,xdo
STACKGET1:
        DW II,FETCH,CELL,xplusloop,STACKGET1
        DW RFROM
        DW EXIT

STACKGET2:
        DW NIP
        DW EXIT

; : STACK.FOREACH  ( xt lifo -- )  ( execute xt for every item in stack )
; \ XT should not return anything on stack
;      DUP @ SWAP STACK.DEPTH   ( xt tos+2 n )
;      ?DUP IF   0 DO           ( xt tos+2 )
;         CELL-                 ( xt tos )
;         2DUP @ SWAP EXECUTE
;      LOOP   THEN
;      DROP DROP  ;
    head(STACKFOREACH,STACK.FOREACH,docolon)
        DW DUP,FETCH,SWOP,STACKDEPTH
        DW QDUP,qbranch,STACKFOREACH2
        DW lit,0,xdo
STACKFOREACH1:
        DW CELLMINUS,TWODUP,FETCH,SWOP,EXECUTE
        DW xloop,STACKFOREACH1
STACKFOREACH2:
        DW DROP,DROP
        DW EXIT

; : STACK.UNTIL  ( xt lifo -- x*i flag )  ( execute xt for every item in stack, until xt returns true )
; \ XT should return flag on stack
;      DUP @ SWAP STACK.DEPTH   ( xt tos+2 n )
;      ?DUP IF   0 DO           ( xt tos+2 )
;         CELL-                 ( xt tos )
;         2DUP >R >R            ( xt tos ; tos xt )
;         @ SWAP EXECUTE   ( i*x flag ; tos xt )
;         R> R> ROT        ( i*x xt tos flag )
;         ?DUP IF
;            NIP NIP UNLOOP EXIT      ( i*x flag )
;         THEN
;      LOOP   THEN
;      DROP DROP 0 ;
    head(STACKUNTIL,STACK.UNTIL,docolon)
        DW DUP,FETCH,SWOP,STACKDEPTH
        DW QDUP,qbranch,STACKUNTIL3
        DW lit,0,xdo
STACKUNTIL1:
        DW CELLMINUS,TWODUP,TOR,TOR
        DW FETCH,SWOP,EXECUTE
        DW RFROM,RFROM,ROT
        DW QDUP,qbranch,STACKUNTIL2
        DW NIP,NIP,UNLOOP,EXIT
STACKUNTIL2:
        DW xloop,STACKUNTIL1
STACKUNTIL3:
        DW DROP,DROP,lit,0
        DW EXIT

; RC2014 EXTENSION CONSTANTS ====================

;C C/L      -- n         columns per line
    head(C_L,C/L,docon)
        dw 64

;C L/B      -- n         lines per block
    head(L_B,L/B,docon)
        dw 16

;C B/BLK      -- n       bytes per block
    head(B_BLK,B/BLK,docon)
        dw 1024


; RC2014 EXTENSION (plan for 8K) ================

; 8 CONSTANT #COLOURS
    head(NCOLOURS,``#COLOURS'',docon)
        dw 8

;: INK  ( n -- change fg to n 0-7 )
;    DUP 0 #COLOURS WITHIN IF
;        VT-ESC
;        30 + (.) TYPE ." m"
;    THEN ;
    head(INK,INK,docolon)
        dw DUP,lit,0,NCOLOURS,WITHIN,qbranch,INK1
        dw VT_ESC
        dw lit,30,PLUS,XDOT,TYPE
        dw lit,'m',EMIT
INK1:
        dw EXIT

;: BRIGHT.INK  ( n -- change fg to n 0-7 )
;    DUP 0 #COLOURS WITHIN IF
;        VT-ESC
;        90 + (.) TYPE ." m"
;    THEN ;
    head(BRIGHTINK,BRIGHT.INK,docolon)
        dw DUP,lit,0,NCOLOURS,WITHIN,qbranch,BRINK1
        dw VT_ESC
        dw lit,90,PLUS,XDOT,TYPE
        dw lit,'m',EMIT
BRINK1:
        dw EXIT

;: PAPER  ( n -- change bg to n 0-7 )
;    DUP 0 #COLOURS WITHIN IF
;        VT-ESC
;        40 + (.) TYPE ." m"
;    THEN ;
    head(PAPER,PAPER,docolon)
        dw DUP,lit,0,NCOLOURS,WITHIN,qbranch,PAPER1
        dw VT_ESC
        dw lit,40,PLUS,XDOT,TYPE
        dw lit,'m',EMIT
PAPER1:
        dw EXIT

;: BRIGHT.PAPER  ( n -- change bg to n 0-7 )
;    DUP 0 #COLOURS WITHIN IF
;        VT-ESC
;        100 + (.) TYPE ." m"
;    THEN ;
    head(BRIGHTPAPER,BRIGHT.PAPER,docolon)
        dw DUP,lit,0,NCOLOURS,WITHIN,qbranch,BRPAPER1
        dw VT_ESC
        dw lit,100,PLUS,XDOT,TYPE
        dw lit,'m',EMIT
BRPAPER1:
        dw EXIT

;Z REVERSE  ( -- reverse attributes )
;    VT-ESC ." 7m" ;
    head(REVERSE,REVERSE,docolon)
        dw VT_ESC, XSQUOTE
        db 2,"7m"
        dw TYPE
        dw EXIT

; DEBUG LED implementation ========================

;Z /LED ( -- )                       initialise LED
;      0 led_state C!
;      0 0 PC!   ;
    head(SLASHLED,/LED,docolon)
        DW lit,0,lit,led_state,CSTORE
        DW lit,0,lit,0,PCSTORE
        DW EXIT

;Z @LED ( -- c )                    fetch LED value
;      led_state C@ ;
    head(LEDFETCH,@LED,docolon)
        DW led_state,CFETCH
        DW EXIT

;Z !LED ( -- c )                    store LED value
;      DUP led_state C! 0 PC! ;
    head(LEDSTORE,!LED,docolon)
        DW DUP,lit,led_state,CSTORE
        DW lit,0,PCSTORE
        DW EXIT

;Z +LED ( n -- )                       enable LED n
;       DUP 0 8 WITHIN IF
;           1 SWAP LSHIFT LED@ OR LED!
;       ELSE
;           DROP
;       THEN ;
    head(PLUSLED,+LED,docolon)
        DW DUP,lit,0,lit,8,WITHIN,qbranch,PLUSLED1
        DW lit,1,SWOP,LSHIFT,LEDFETCH,OR,LEDSTORE
        DW EXIT
PLUSLED1:
        DW DROP,EXIT

;Z -LED ( n -- )                      disable LED n
;       DUP 0 8 WITHIN IF
;           1 SWAP LSHIFT INVERT LED@ AND LED!
;       ELSE
;           DROP
;       THEN ;
    head(MINUSLED,-LED,docolon)
        DW DUP,lit,0,lit,8,WITHIN,qbranch,MINUSLED1
        DW lit,1,SWOP,LSHIFT,INVERT,LEDFETCH,AND,LEDSTORE
        DW EXIT
MINUSLED1:
        DW DROP,EXIT

SECTION data_user

led_state:
        DEFB 0

SECTION code_user_16k


; RC2014 EXTENSION (MISC) =======================

;C ERASE       ( a-addr u --   fill with 0's )
;    0 FILL
;    ;
    head(ERASE,ERASE,docolon)
        dw lit,0,FILL
        dw EXIT

;C BLANKS       ( a-addr u --   fill with spaces )
;    BL FILL
;    ;
    head(BLANKS,BLANKS,docolon)
        dw lit,32,FILL
        dw EXIT

;Z (XORSHIFT) ( n -- n   xorshift random number generator )
;    DUP 7 LSHIFT XOR
;    DUP 9 RSHIFT XOR
;    DUP 8 LSHIFT XOR ;
    head(XXORSHIFT,(XORSHIFT),docolon)
        DW DUP,lit,7,LSHIFT,XOR
        DW DUP,lit,9,RSHIFT,XOR
        DW DUP,lit,8,LSHIFT,XOR
        DW EXIT

;: RND  ( -- n   generate random 16bit value from seed )
;    SEED @
;    (XORSHIFT)
;    DUP SEED ! ;
    head(RND,RND,docolon)
        DW SEED,FETCH
        DW XXORSHIFT
        DW DUP,SEED,STORE
        DW EXIT

;: RANDOM (  n -- n  generate random value between 0 and value on stack )
;    ( WARNING: Not evenly distributed but should be good )
;    RND SWAP MOD ABS ;
    head(RANDOM,RANDOM,docolon)
        DW RND,SWOP,MOD,ABS
        DW EXIT

;C NOOP        ( -- )        no operation
;    ;
    head(NOOP,NOOP,docolon)
        dw EXIT

;C DEFER    ( "name" -- )        create a deferred word
;    CREATE ['] NOOP ,
;   DOES>
;    @ EXECUTE ;
    head(DEFER,DEFER,docolon)
        DW CREATE,lit,NOOP,COMMA

        DW XDOES
        call dodoes
        DW FETCH,EXECUTE
        dw EXIT

;C DEFER!   ( xt2 xt1 -- )             store xt2 in xt1
;   >BODY ! ;
    head(DEFERSTOR,DEFER!,docolon)
        DW TOBODY,STORE
        DW EXIT

;C DEFER@   ( xt2 xt1 -- )             store xt2 in xt1
;   >BODY ! ;
    head(DEFERFETCH,DEFER@,docolon)
        DW TOBODY,FETCH
        DW EXIT

;C IS       ( xt "name" -- )     define a deferred word
;   STATE @ IF
;      POSTPONE ['] POSTPONE DEFER!
;   ELSE
;      ' DEFER!
;   THEN ; IMMEDIATE
    immed(IS,IS,docolon)
        DW STATE,FETCH,qbranch,IS1
        DW BRACTICK,lit,DEFERSTOR,COMMAXT
        DW branch,IS2
IS1:
        DW TICK,DEFERSTOR
IS2:
        DW EXIT

;C ACTION-OF  ( "name -- xt" )     get the action of a deferred word
;   STATE @ IF
;      POSTPONE ['] POSTPONE DEFER@
;   ELSE
;      ' DEFER@
;   THEN ; IMMEDIATe
    immed(ACTION_OF,ACTION-OF,docolon)
        DW STATE,FETCH,qbranch,AOF1
        DW BRACTICK,lit,DEFERFETCH,COMMAXT
        DW branch,AOF2
AOF1:
        DW TICK,DEFERFETCH
AOF2:
        DW EXIT


; RC2014 EXTENSIONS (TERMINAL) ==================

;Z D.R                       ( d width -- right align )
;    >R (D.)
;    R> OVER - SPACES TYPE ;
    head(DDOTR,D.R,docolon)
        dw TOR,XDDOT
        dw RFROM,OVER,MINUS,SPACES,TYPE
        dw EXIT

;Z .R                ( n width -- right align )
;    >R S>D R> D.R ;
    head(DOTR,.R,docolon)
        dw TOR,STOD,RFROM,DDOTR
        dw EXIT

;Z (D.W)         ( d width --   width with leading 0's )
;    1- DUP 1 < IF DROP 1 THEN <# 0 DO # LOOP #S #> ;
    head(XDDOTW,(D.W),docolon)
        dw ONEMINUS,DUP,lit,1,LESS,qbranch,XDDOTW1
        dw DROP,lit,1
XDDOTW1:
        dw LESSNUM,lit,0,xdo
XDDOTW2:
        dw NUM,xloop,XDDOTW2
        dw NUMS,NUMGREATER
        dw EXIT

;Z (.W)         ( n width --   width with leading 0's )
;    >R S>D R> (D.W) ;
    head(XDOTW,(.W),docolon)
        dw TOR,STOD,RFROM,XDDOTW
        dw EXIT

;Z .W           ( n width --   width with leading 0's )
;    (.W) TYPE SPACE ;
    head(DOTW,.W,docolon)
        dw XDOTW,TYPE,SPACE
        dw EXIT

;Z (U.W)        ( u width --   width with leading 0's )
;    0 SWAP (D.W) ;
    head(XUDOTW,(U.W),docolon)
        dw lit,0,SWOP,XDDOTW
        dw EXIT

;Z U.W         ( u width --   width with leading 0's )
;    (U.W) TYPE SPACE ;
    head(UDOTW,U.W,docolon)
        dw XUDOTW,TYPE,SPACE
        dw EXIT

;Z PRINTABLE?         ( n - flag  is characte printable? )
;    20 7F WITHIN ;
    head(PRINTABLEQ,PRINTABLE?,docolon)
        dw lit,0x20,lit,0x7f,WITHIN
        dw EXIT

; VOCABULARY implementation =====================
; Vocabularies are implemented in this version of CamelForth the ANS Forth '94 way.
; The search order is implemented as a stack of word list identifiers (wid).
; The current word list that is compiled into is identified by the CURRENT variable.
; A word list identifier is a pointer to the call that contains the pointer to
; the NFA of the "latest" wordlist word entry.
; The FORTH wordlist is equivalent to LATEST in CamelForth

; https://www.taygeta.com/forth/dpans16.htm#16.6.2.1965

SECTION data_user

STACK_WORDLISTS:
        ds 34    ; 16 cells + stack top pointer

STACK_RECOGNIZER:
        ds 18    ; 8 cells + stack top pointer

SECTION code_user_16k

;WORDLIST ( -- wid )
; Create a new empty word list, returning its word list identifier wid.
; The new word list may be returned from a pool of preallocated word lists
; or may be dynamically allocated in data space. A system shall allow the
; creation of at least 8 new word lists in addition to any provided as part
; of the system.
;     HERE >R  0 ,  R> ;
        head(WORDLIST,WORDLIST,docolon)
            dw HERE,TOR,lit,0,COMMA,RFROM,EXIT

;SEARCH-WORDLIST ( c-addr u wid -- 0 | xt 1 | xt -1 )
; Find the definition identified by the string c-addr u in the word list
; identified by wid. If the definition is not found, return zero. If the
; definition is found, return its execution token xt and one (1) if the
; definition is immediate, minus-one (-1) otherwise.

;: FORTH-WORDLIST ( -- wid )
; Return wid, the identifier of the word list that includes all standard
; words provided by the implementation. This word list is initially the
; compilation word list and is part of the initial search order.
        head(FORTH_WORDLIST,FORTH-WORDLIST,docolon)
            dw lit,FORTH_WORDLIST_WID
            dw EXIT

;: EDITOR-WORDLIST ( -- wid )
        head(EDITOR_WORDLIST,EDITOR-WORDLIST,docolon)
            dw lit,EDITOR_WORDLIST_WID
            dw EXIT

;: VOCAB-WORDLIST ( -- wid )
        head(VOCAB_WORDLIST,VOCAB-WORDLIST,docolon)
            dw lit,VOCAB_WORDLIST_WID
            dw EXIT

;: GET-ORDER  ( -- wid1 .. widn n )
        head(GET_ORDER,GET-ORDER,docolon)
            dw lit,STACK_WORDLISTS,STACKGET
            dw EXIT

;: SET-ORDER  ( wid1 .. widn n -- )
        head(SET_ORDER,SET-ORDER,docolon)
            dw lit,STACK_WORDLISTS,STACKSET
            dw EXIT

;: >ORDER  ( wid1 .. widn n -- )
        head(TOORDER,>ORDER,docolon)
            dw lit,STACK_WORDLISTS,TOSTACK
            dw EXIT

;: SET-CURRENT  ( wid -- )
;    CURRENT !  ;
        head(SET_CURRENT,SET-CURRENT,docolon)
            dw CURRENT,STORE
            dw EXIT

;: GET-CURRENT  ( -- wid )
;    CURRENT @  ;
        head(GET_CURRENT,GET-CURRENT,docolon)
            dw CURRENT,FETCH
            dw EXIT

;: CONTEXT      ( -- wid )
;    STACK_WORDLIST STACK@
        head(CONTEXT,CONTEXT,docolon)
            dw lit,STACK_WORDLISTS,STACKFETCH
            dw EXIT


;C FIND-NAME-IN   c-addr len wid   --  c-addr len 0       if not found
;C                                     c-addr len nfa     if found
;   @ BEGIN                    -- a len nfa
;       2DUP                   -- a len nfa len nfa
;       C@  =                   -- a len nfa f
;       IF                     -- a len nfa
;         >R OVER              -- a len a ; nfa
;         R@ COUNT                -- a len a nfa+1 nfa-len ; nfa
;         S= R> SWAP              -- a len nfa f
;       ELSE
;         -1                      -- a len nfa f
;       THEN
;       DUP IF
;           DROP
;           NFA>LFA @      -- a len link
;       THEN
;   0= UNTIL                   -- a len nfa  OR  a len 0
;      ;
    head(FIND_NAME_IN,FIND-NAME-IN,docolon)
        DW FETCH
FINDIN1:
        DW TWODUP,CFETCH,EQUAL
        DW qbranch,FINDIN2
        DW TOR,OVER
        DW RFETCH,COUNT
        DW sequal,RFROM,SWOP
        DW branch,FINDIN3
FINDIN2:
        DW lit,-1
FINDIN3:
        DW DUP,qbranch,FINDIN4
        DW DROP,NFATOLFA,FETCH,DUP
FINDIN4:
        DW ZEROEQUAL,qbranch,FINDIN1
FINDIN5:
        DW EXIT

;C FIND-NAME   c-addr len      -- 0   if not found
;C                                nt  if found
;    ' FIND-NAME-IN STACK_WORDLISTS STACK.UNTIL
;                         ( c-addr len 0       if not found )
;                         ( c-addr len nfa     if found )
;    NIP NIP ;
    head(FIND_NAME,FIND-NAME,docolon)
        DW lit,FIND_NAME_IN,lit,STACK_WORDLISTS,STACKUNTIL
        DW NIP,NIP
        DW EXIT

;C FIND   c-addr -- c-addr 0   if not found
;C                  xt  1      if immediate
;C                  xt -1      if "normal"
;   DUP COUNT FIND-NAME     c-addr nt | c-addr 0
;   DUP IF
;       NIP DUP NFA>CFA        -- nfa xt
;       SWAP IMMED?            -- xt iflag
;       0= 1 OR                -- xt 1/-1
;   THEN ;
    head(FIND,FIND,docolon)
        DW DUP,COUNT,FIND_NAME
        DW DUP,qbranch,FIND1
        DW NIP,DUP,NFATOCFA
        DW SWOP,IMMEDQ
        DW ZEROEQUAL,lit,1,OR
FIND1:
        DW EXIT

;WORDLIST CONSTANT ROOT   ROOT SET-CURRENT

;: DO-VOCABULARY  ( -- ) \ Implementation factor
;    DOES>  @ >R           (  ) ( R: widnew )
;     GET-ORDER  SWAP DROP ( wid1 ... widn-1 n )
;     R> SWAP SET-ORDER
;  ;

;: DISCARD  ( x1 .. xu u - ) \ Implementation factor
;   ?DUP IF  0 DO DROP LOOP  THEN        \ DROP u+1 stack items
;  ;

;CREATE FORTH  FORTH-WORDLIST , DO-VOCABULARY
    head_vocab(FORTH,FORTH,docolon)
        dw FORTH_WORDLIST
        dw lit,STACK_WORDLISTS,STACKSTORE
        dw EXIT

;CREATE EDITOR  EDITOR-WORDLIST , DO-VOCABULARY
    head_vocab(EDITOR,EDITOR,docolon)
        dw EDITOR_WORDLIST
        dw lit,STACK_WORDLISTS,STACKSTORE
        dw EXIT


;: VOCABULARY  ( name -- )  WORDLIST CREATE ,
;   VOC-LINK @ @ , 0 C,         link & `immed' field
;   HERE VOC-LINK @ !           new "latest" link
;   BL WORD C@ 1+ ALLOT         name field
;   docreate ,CF                code field
;   WORDLIST ,
;
;    DOES>  @ STACK_WORDLISTS,TOSTACK
;  ;
    head(VOCABULARY,VOCABULARY,docolon)
        dw VOCLINK,FETCH,FETCH,COMMA,lit,0,CCOMMA
        dw HERE,VOCLINK,FETCH,STORE
        dw BL,WORD,CFETCH,ONEPLUS,ALLOT
        dw lit,docreate,COMMACF
        dw WORDLIST,COMMA

        dw XVOCDOES
        call dodoes
        DW lit,STACK_WORDLISTS,STACKSTORE
        dw EXIT

; patch latest entry on VOCLINK with code from VOCDOES
    head(XVOCDOES,(VOCDOES>),docolon)
        DW RFROM,VOCLINK,FETCH,FETCH,NFATOCFA,STORECF
        DW EXIT

;: ALSO  ( -- )  STACK_WORDLISTS DUP STACK@ >STACK ;
    head(ALSO,ALSO,docolon)
        dw lit,STACK_WORDLISTS,STACKFETCH
        dw lit,STACK_WORDLISTS,TOSTACK
        dw EXIT

;: PREVIOUS  ( --  )  STACK_WORDLISTS >STACK, DROP ;
    head(PREVIOUS,PREVIOUS,docolon)
        dw lit,STACK_WORDLISTS,STACKFROM,DROP
        dw EXIT

;: DEFINITIONS  ( -- )  STACK_WORDLISTS STACK@ SET-CURRENT ;
    head(DEFINITIONS,DEFINITIONS,docolon)
        dw lit,STACK_WORDLISTS,STACKFETCH,SET_CURRENT
        dw EXIT

;: ONLY ( -- )  FORTH-WORDLIST 1 SET-ORDER ;
    head(ONLY,ONLY,docolon)
        dw VOCAB_WORDLIST,FORTH_WORDLIST,lit,2,SET_ORDER
        dw EXIT

;: VOCS  ( -- )      list all vocabularies in dict
;   VOCLINK @ (WORDS) ;
    head(VOCS,VOCS,docolon)
        DW VOCLINK,FETCH,XWORDS,EXIT

;: VLIST  ( -- )      list all words in search order
;   (WORDS) STACK_WORDLISTS STACK.FOREACH ;
    head(VLIST,VLIST,docolon)
        DW lit,XWORDS,lit,STACK_WORDLISTS,STACKFOREACH
        DW EXIT

; BLOCK implementation ==========================

;Z BLKFIRST      -- a-adrs      address of first block buffer
    head(BLKFIRST,BLKFIRST,docon)
        dw 0x8800

;Z BLOCK-READ  ( -- f )   Compact Flash read block  BLK and DSK
; Reads the block from the Compact Flash card into memory
; address found at 'adrs'. 'dks' and 'blk' are the disk
; and block numbers respectively
    head(BLOCK_READ,BLOCK-READ,docolon)
        dw DSK,FETCH,BLK,FETCH,BLKBUFFER,FETCH
        dw lit,cflash_read_block,CALL
        dw INVERT,XSQUOTE
        db 10,"DISK ERROR"
        dw QABORT,EXIT

;Z BLOCK-WRITE  ( -- f )  Compact Flash read write BLK and DSK
; Reads the block from the Compact Flash card into memory
; address found at 'adrs'. 'dks' and 'blk' are the disk
; and block numbers respectively
    head(BLOCK_WRITE,BLOCK-WRITE,docolon)
        dw DSK,FETCH,BLK,FETCH,BLKBUFFER,FETCH
        dw lit,cflash_write_block,CALL
        dw INVERT,XSQUOTE
        db 10,"DISK ERROR"
        dw QABORT,EXIT

;C BUFFER        n -- addr         push buffer address
;     FLUSH
;     BLK !
;     BLKFIRST BLKBUFFER !
;     BLKBUFFER @      ( push buffer address ) ;
    head(BUFFER,BUFFER,docolon)
        dw FLUSH
        dw BLK,STORE
        dw BLKFIRST,BLKBUFFER,STORE
        dw BLKBUFFER,FETCH
        dw EXIT

;C BLOCK                    n -- addr    load block
;     DUP BLK @ = IF
;       DROP BLKBUFFER @
;     ELSE
;       BUFFER BLOCK-READ
;     THEN ;
    head(BLOCK,BLOCK,docolon)
        dw DUP,BLK,FETCH,EQUAL,qbranch,BLOCK1
        dw DROP,BLKBUFFER,FETCH
        dw branch,BLOCK2
BLOCK1:
        dw BUFFER,BLOCK_READ
BLOCK2:
        dw EXIT

;C UPDATE                    --    mark block to update
;     -1 BLKUPDATE ! ;
    head(UPDATE,UPDATE,docolon)
        dw lit,0xffff,BLKUPDATE,STORE
        dw EXIT

;C UPDATED?                 n -- f   is block updated?
;     BLK @ = IF
;         BLKUPDATE FETCH
;     ELSE 0 THEN ;
    head(UPDATEDQ,UPDATED?,docolon)
        dw BLK,FETCH,EQUAL,qbranch,UPDATEDQ1
        dw BLKUPDATE,FETCH,branch,UPDATEDQ2
UPDATEDQ1:
        dw lit,0
UPDATEDQ2:
        dw EXIT

;C FLUSH                    --    flush blocks to disk
;     BLK @ UPDATED? IF BLOCK-WRITE  0 BLKUPDATE ! THEN ;
    head(FLUSH,FLUSH,docolon)
        dw BLK,FETCH,UPDATEDQ,qbranch,FLUSH1
        dw BLOCK_WRITE,lit,0,BLKUPDATE,STORE
FLUSH1:
        dw EXIT

;C LOAD                  n  --    load block n
;     BLK @ >R
;     DUP 0= IF    BLOCK DROP
;            ELSE  BLOCK B/BLK EVALUATE  THEN
;     R> BLOCK DROP  ;
    head(LOAD,LOAD,docolon)
        dw BLK,FETCH,TOR
        dw DUP,lit,0,EQUAL,qbranch,LOAD1
        dw BLOCK,DROP,branch,LOAD2
LOAD1:
        dw BLOCK,B_BLK,EVALUATE
LOAD2:
        dw RFROM,BLOCK,DROP
        dw EXIT

;C THRU            n1 n2  --    load blocks n1 to n2
;     1+ SWAP DO I LOAD LOOP ;
    head(THRU,THRU,docolon)
        dw ONEPLUS,SWOP,xdo
THRU1:
        dw II,DUP,DOT,LOAD,xloop,THRU1
        dw EXIT

; RC2014 EXTENSION (SCREENS) ====================

;Z (BLOCK)                 -- a-addr  load block in SCR
;     SCR @ BLOCK ;
    head(XBLOCK,(BLOCK),docolon)
        dw SCR,FETCH,BLOCK
        dw EXIT

;Z (LINE)           line# -- c-addr   address of line in block
;     C/L * (BLOCK) + ;
    head(XLINE,(LINE),docolon)
        dw C_L,STAR,XBLOCK,PLUS
        dw EXIT

;Z LL               line# --      List Line
;     (LINE) C/L TYPE CR ;
    head(LL,LL,docolon)
        dw XLINE,C_L,TYPE,CR
        dw EXIT


;Z  (LIST)            --    runtime for list screen
;     L/B 0 DO I 2 .R SPACE I LL LOOP ;
    head(XLIST,(LIST),docolon)
        dw L_B,lit,0,xdo
XLIST1:
        dw II,lit,2,DOTR,SPACE,II,LL,xloop,XLIST1
        dw EXIT

;C LIST ( n -- )            list screen number
;     DUP SCR ! ." SCR # " . CR (LIST) ;
    head(LIST,LIST,docolon)
        dw DUP,SCR,STORE,XSQUOTE
        db 6,"SCR # "
        dw TYPE,DOT,CR
        dw XLIST
        dw EXIT

;C INDEX ( from to -- )     print first line of each screen
;     CR 1+ SWAP DO I 2 .R SPACE I DUP SCR ! BLOCK DROP 0 LL LOOP ;
    head(INDEX,INDEX,docolon)
        dw CR,ONEPLUS,SWOP,xdo
INDEX1:
        dw II,lit,2,DOTR,SPACE
        dw II,DUP,SCR,STORE,BLOCK,DROP
        dw lit,0,LL,xloop,INDEX1
        dw EXIT

;Z WIPE     ( n -- erase block n )
;    BUFFER 1024 BLANKS
;    UPDATE ;
    head(WIPE,WIPE,docolon)
        dw BUFFER,lit,1024,BLANKS
        dw UPDATE
        dw EXIT

;: MARKER  ( "name" -- )
;    HERE >R
;    CREATE LATEST @ NFA>LFA FETCH ,
;    R> ,
;    DOES> DUP  @ LATEST !
;            CELL+  @ DP ! ;
    head(MARKER,MARKER,docolon)
        DW HERE,TOR
        DW CREATE,LATEST,FETCH
        DW NFATOLFA,FETCH,COMMA
        DW RFROM,COMMA

        DW XDOES
        call dodoes
        DW DUP,FETCH,LATEST,STORE
        DW CELLPLUS,FETCH,DP,STORE
        dw EXIT

; (BSAVE) ( c-addr u -- )  save block to file
;    BLK @ BUFFER                               ( c-addr u buffer )
;    SWAP MOVE
;    UPDATE  ;
    head(XBSAVE,(BSAVE),docolon)
        dw BLK,FETCH,BUFFER
        dw SWOP,MOVE
        dw UPDATE
        dw EXIT

;: BSAVE   ( c-addr u blk -- )
;    BUFFER DROP    ( c-addr u -- ; blk in BLK)
;
;    B/BLK /MOD    ( c-addr rem #blks )
;    SWAP   >R      (c-addr #blks ; rem )
;    ?DUP IF
;      0 DO                  ( c-addr ; rem )
;        DUP B/BLK (BSAVE)   ( c-addr ; rem )
;        B/BLK +             ( c-addr' ; rem )
;        1 BLK +!            ( c-addr' ; rem )
;      LOOP
;    THEN           ( c-addr ; rem )
;
;    R> (BSAVE) ( c-addr rem )
;    FLUSH  ;
    head(BSAVE,BSAVE,docolon)
        dw BUFFER,DROP
        dw B_BLK,SLASHMOD
        dw SWOP,TOR
        dw QDUP,qbranch,BSAVE1
        dw lit,0,xdo
BSAVE2:
        dw DUP,B_BLK,XBSAVE
        dw B_BLK,PLUS
        dw lit,1,BLK,PLUSSTORE
        dw xloop,BSAVE2
BSAVE1:
        dw RFROM,XBSAVE
        dw FLUSH
        dw EXIT

; (BLOAD) ( c-addr u -- )  save block to file
;    BLK @ BLOCK                            ( c-addr u buffer )
;    ROT ROT MOVE    ;
    head(XBLOAD,(BLOAD),docolon)
        dw BLK,FETCH,BLOCK
        dw ROT,ROT,MOVE
        dw EXIT

;: BLOAD   ( blk c-addr u -- )
;    ROT BLOCK DROP    ( c-addr u -- ; blk in BLK)
;
;    B/BLK /MOD    ( c-addr rem #blks )
;    SWAP   >R      (c-addr #blks ; rem )
;    ?DUP IF
;      0 DO                  ( c-addr ; rem )
;        DUP B/BLK (BLOAD)   ( c-addr ; rem )
;        B/BLK +             ( c-addr' ; rem )
;        1 BLK +!            ( c-addr' ; rem )
;      LOOP
;    THEN           ( c-addr ; rem )
;
;    R> (BLOAD) ( c-addr rem )     ;
    head(BLOAD,BLOAD,docolon)
        dw ROT,BLOCK,DROP
        dw B_BLK,SLASHMOD
        dw SWOP,TOR
        dw QDUP,qbranch,BLOAD1
        dw lit,0,xdo
BLOAD2:
        dw DUP,B_BLK,XBLOAD
        dw B_BLK,PLUS
        dw lit,1,BLK,PLUSSTORE
        dw xloop,BLOAD2
BLOAD1:
        dw RFROM,XBLOAD
        dw EXIT


defc BLK_HEADER_SIZE = 64
defc BLK_DATA_SIZE   = 960

; (SAVEHDR)            ( c-addr u buffer -- c-addr' u' )
; --------------------------------------------------------------
; Header
;    size of header
;    size of data
;    LATEST 2 bytes. last word in dict
;    DP 2 bytes. dictionary pointer
;    VOCLNK 2 bytes.
;    CONTEXT 2 bytes (for number of items on wordlist stack)
;         followed by n items of stack.
; --------------------------------------------------------------
;    Setup header
;    BUFFER     ( c-addr u buffer -- ; blk in BLK)
;      DUP BLK_HEADER_SIZE ERASE
;      BLK_HEADER_SIZE OVER !    ( store header size )
;      CELL+ 2DUP !              ( store data size )
;      CELL+ LATEST @ OVER !     ( store LATEST )
;      CELL+ DP @ OVER !         ( store DP )
;      DROP                      ( c-addr u )
;
;    BUFFER BLK_HDR_SIZE +    ( c-addr u buffer' )
;    OVER BLK_DATA_SIZE > IF
;      >R OVER R> BLK_DATA_SIZE MOVE UPDATE  ( c-addr u )
;      SWAP BLK_DATA_SIZE +                  ( u c-addr' )
;      SWAP BLK_DATA_SIZE -                  ( c-addr' u' )
;    ELSE                     ( c-addr u buffer' )
;      >R 2DUP R>             ( c-addr u c-addr u buffer' )
;       SWAP MOVE UPDATE      ( c-addr u )
;       + 0                   ( c-addr' 0 )
;    THEN   FLUSH  ;
    head(XSAVEHDR,(SAVEHDR),docolon)
        dw DUP,lit,BLK_HEADER_SIZE,ERASE
        dw lit,BLK_HEADER_SIZE,OVER,STORE
        dw CELLPLUS,TWODUP,STORE
        dw CELLPLUS,LATEST,FETCH,OVER,STORE
        dw CELLPLUS,DP,FETCH,OVER,STORE
        dw DROP

        dw BLK,FETCH,BUFFER,lit,BLK_HEADER_SIZE,PLUS
        dw OVER,lit,BLK_HEADER_SIZE,GREATER,qbranch,XSAVEHDR1
        dw TOR,OVER,RFROM,lit,BLK_DATA_SIZE,MOVE,UPDATE
        dw SWOP,lit,BLK_DATA_SIZE,PLUS
        dw SWOP,lit,BLK_DATA_SIZE,MINUS
        dw branch,XSAVEHDR2
XSAVEHDR1:
        dw TOR,TWODUP,RFROM
        dw SWOP,MOVE,UPDATE
        dw PLUS,lit,0
XSAVEHDR2:
        dw FLUSH
        dw EXIT

;: SAVE   ( blk -- )
;    enddict   DP @ enddict -    ( blk c-addr u )
;    ROT BUFFER                  ( c-addr u buffer -- ; blk in BLK)
;    (BSAVEHDR)                  ( c-addr' u' )
;
;    B/BLK /MOD    ( c-addr rem #blks )
;    SWAP   >R      (c-addr #blks ; rem )

;    BLK @ 1+ BUFFER DROP
;
;    ?DUP IF
;      0 DO                  ( c-addr ; rem )
;        DUP B/BLK (BSAVE)   ( c-addr ; rem )
;        B/BLK +             ( c-addr' ; rem )
;        BLK @ 1+ BUFFER DROP            ( c-addr' ; rem )
;      LOOP
;    THEN           ( c-addr ; rem )
;
;    R>
;    QDUP  IF (BSAVE) ELSE DROP THEN    (  )
;    FLUSH   ;
    head(SAVE,SAVE,docolon)
        dw lit,enddict,DP,FETCH,lit,enddict,MINUS  ;  ( block c-addr u )
        dw ROT,BUFFER                              ;  ( c-addr u buffer )
        dw XSAVEHDR
        dw BLK,FETCH,ONEPLUS,BUFFER,DROP

        dw B_BLK,SLASHMOD
        dw SWOP,TOR

        dw QDUP,qbranch,SAVE2
        dw lit,0,xdo
SAVE1:
        dw DUP,B_BLK,XBSAVE
        dw B_BLK,PLUS
        dw BLK,FETCH,ONEPLUS,BUFFER,DROP
        dw xloop,SAVE1
SAVE2:
        dw RFROM
        dw QDUP,qbranch,SAVE3
        dw XBSAVE
        dw branch,SAVE4
SAVE3:
        dw DROP
SAVE4:
        dw FLUSH
        dw EXIT

;: RESTORE   ( blk -- )
;    BLOCK                       ( buffer -- ; blk in BLK)
;    DUP @                       ( buffer hdr_size )
;    OVER +                      ( buffer buffer' )
;    SWAP CELL+ DUP @            ( buffer' buffer data_size )
;    SWAP CELL+ DUP @ LATEST !   ( buffer' data_size buffer )
;    CELL+ DUP @ DP !            ( buffer' data_size buffer )
;
;    DROP SWAP enddict ROT       ( buffer' enddict u)
;
;    DUP BLK_DATA_SIZE > IF   ( buffer' c-addr u )
;      >R 2DUP BLK_DATA_SIZE MOVE NIP R>     ( c-addr u )
;      SWAP BLK_DATA_SIZE +                  ( u c-addr' )
;      SWAP BLK_DATA_SIZE -                  ( c-addr' u' )
;    ELSE                     ( buffer' c-addr u )
;      >R 2DUP R@             ( buffer' c-addr buffer' c-addr u )
;       MOVE NIP R>           ( c-addr u )
;       + 0                   ( c-addr' 0 )
;    THEN
;
;    lastword enddict !          ( patch link address )
;
;    BLK @ 1+ BLOCK DROP
;
;    B/BLK /MOD    ( c-addr rem #blks )
;    SWAP   >R      (c-addr #blks ; rem )
;    ?DUP IF
;      0 DO                  ( c-addr ; rem )
;        DUP B/BLK (BLOAD)   ( c-addr ; rem )
;        B/BLK +             ( c-addr' ; rem )
;        BLK @ 1+ BLOCK DROP ( c-addr' ; rem )
;      LOOP
;    THEN           ( c-addr ; rem )
;    R>       ( c-addr rem )
;
;    QDUP  IF (BLOAD) ELSE DROP THEN  ;  (  )
    head(RESTORE,RESTORE,docolon)
        dw BLOCK
        dw DUP,FETCH               ; header size
        dw OVER,PLUS
        dw SWOP,CELLPLUS,DUP,FETCH
        dw SWOP,CELLPLUS,DUP,FETCH,LATEST,STORE
        dw CELLPLUS,DUP,FETCH,DP,STORE

        dw DROP,SWOP,lit,enddict,ROT

        dw DUP,lit,BLK_DATA_SIZE,GREATER,qbranch,RESTORE1
        dw TOR,TWODUP,lit,BLK_DATA_SIZE,MOVE,NIP,RFROM
        dw SWOP,lit,BLK_DATA_SIZE,PLUS
        dw SWOP,lit,BLK_DATA_SIZE,MINUS
        dw branch,RESTORE2
RESTORE1:
        dw TOR,TWODUP,RFETCH
        dw MOVE,NIP,RFROM
        dw PLUS,lit,0

RESTORE2:
        dw lit,lastword,lit,enddict,STORE
        dw BLK,FETCH,ONEPLUS,BLOCK,DROP

        dw B_BLK,SLASHMOD
        dw SWOP,TOR

        dw QDUP,qbranch,RESTORE4
        dw lit,0,xdo
RESTORE3:
        dw DUP,B_BLK,XBLOAD
        dw B_BLK,PLUS
        dw BLK,FETCH,ONEPLUS,BLOCK,DROP
        dw xloop,RESTORE3
RESTORE4:
        dw RFROM
        dw QDUP,qbranch,RESTORE5
        dw XBLOAD
        dw branch,RESTORE6
RESTORE5:
        dw DROP

RESTORE6:
        dw EXIT




; an alternative implementation of recognizers. It
; uses a separate stack module, that can be used
; to implement the search order words independently
;
; Based on the RECOGNIZER by Author: Matthias Trute
; License: Public Domain

; 4 STACK VALUE FORTH-RECOGNIZER

; define a recognizer with three actions. Suggesting RECTYPE-* names
;: RECTYPE: ( XT-INTERPRET XT-COMPILE XT-POSTPONE "<spaces>name" -- )
;  CREATE SWAP ROT , , ,
;    ;
    head(RECTYPECOLON,RECTYPE:,docolon)
        DW CREATE,SWOP,ROT,COMMA,COMMA,COMMA
        DW EXIT

;: RECTYPE>POST ( RECTYPE-TOKEN -- XT-POSTPONE ) CELL+ CELL+ @ ;
    head(RECTYPETOPOST,RECTYPE>POST,docolon)
        DW CELLPLUS,CELLPLUS,FETCH
        DW EXIT

;: RECTYPE>COMP ( RECTYPE-TOKEN -- XT-COMPILE  )       CELL+ @ ;
    head(RECTYPETOCOMP,RECTYPE>COMP,docolon)
        DW CELLPLUS,FETCH
        DW EXIT

;: RECTYPE>INT  ( RECTYPE-TOKEN -- XT-INTERPRET)             @ ;
    head(RECTYPETOINT,RECTYPE>INT,docolon)
        DW FETCH
        DW EXIT

; 0 CONSTANT RECTYPE-NULL
    head(RECTYPE_NULL,RECTYPE-NULL,docon)
        dw 0

;: (recognize) ( addr len XT -- addr len 0 | i*x RECTYPE-TOKEN -1 )
;   ROT ROT 2DUP 2>R ROT EXECUTE 2R> ROT
;   DUP RECTYPE-NULL =          ( -- i*x addr len RECTYPE-TOKEN f )
;   IF DROP 0 ELSE NIP NIP -1 THEN   ;
    head(XRECOGNIZE,(RECOGNIZE),docolon)
        dw ROT,ROT,TWODUP,TOR,TOR,ROT,EXECUTE,RFROM,RFROM,ROT
        dw DUP,RECTYPE_NULL
        dw EQUAL,qbranch,XRECOGNIZE1
        dw DROP,lit,0,branch,XRECOGNIZE2
XRECOGNIZE1:
        dw NIP,NIP,lit,-1
XRECOGNIZE2:
        dw EXIT

; Example:
; ' NOOP :NONAME POSTPONE LITERAL ; DUP   RECTYPE: RECTYPE-NUM
;
; : REC-CHAR ( addr len -- n RECTYPE-NUM | RECTYPE-NULL )
;    3 = IF
;       DUP C@ [CHAR] ' = IF
;          DUP 2 + C@ [CHAR] ' = IF
;             1+ C@ RECTYPE-NUM EXIT
;          THEN
;       THEN
;    THEN
;    DROP RECTYPE-NULL   ;
;
; 4 STACK recognizor
;
; ' REC-CHAR recognizor >STACK
;
; : test S" ' '" recognizor RECOGNIZE  ;

;: RECOGNIZE ( addr len stack-id -- i*x rectype-token | rectype-null )
;    ['] (recognize) SWAP STACK-UNTIL ( -- i*x rectype-token -1 | addr len 0 )
;    0= IF                           \ no recognizer did the job, remove addr/len
;     2DROP RECTYPE-NULL
;    THEN    ;
    head(RECOGNIZE,RECOGNIZE,docolon)
        DW lit,XRECOGNIZE,SWOP,STACKUNTIL
        DW ZEROEQUAL,qbranch,RECOGNIZE1
        DW TWODROP,RECTYPE_NULL
RECOGNIZE1:
        DW EXIT

