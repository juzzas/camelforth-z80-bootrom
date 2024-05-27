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

;Z /16KROM    init enhanced features
    head(SLASH16KROM,``/16KROM'',docolon)
        DW lit,VOCAB_WORDLIST_WID,lit,FORTH_WORDLIST_WID,lit,2,SET_ORDER
        DW lit,FORTH_WORDLIST_WID,CURRENT,STORE
        DW lit,VOCAB_WORDLIST_WID,VOCLINK,STORE
        DW lit,CF_BLOCK_READWRITE,BLKRWVEC,STORE
        DW SLASHBLKCTX
        dw EXIT

    ;Z BLK      -- a-addr     block number storage
    ;  20 USER BLK
        head(BLK,BLK,douser)
            dw 20

    ;Z DSK      -- a-addr     disk number storage
    ;  22 USER DSK
        head(DSK,DSK,douser)
            dw 22

    ;Z BLKOFFSET    -- a-addr  1024byte block buffer
    ;  24 USER BLKOFFSET
        head(BLKOFFSET,BLKOFFSET,douser)
            dw 24

    ;Z BLKLIMIT    -- a-addr  block update flag storage
    ;  26 USER BLKLIMIT
        head(BLKLIMIT,BLKLIMIT,douser)
            dw 26

    ;Z BLKRWVEC   -- a-addr  if set, use XT to read block
    ;  28 USER BLKRWVEC
        head(BLKRWVEC,BLKRWVEC,douser)
            dw 28

    ;Z BLKWRITEVEC   -- a-addr  if set, use XT to write block
    ;  30 USER BLKWRITEVEC
        head(BLKWRITEVEC,BLKWRITEVEC,douser)
            dw 30

    ;Z SCR          -- a-addr  last edited screen number
    ;  32 USER SCR
        head(SCR,SCR,douser)
            dw 32

    ;Z REC-USERVEC      -- xt    if set, use XT as user vector for RECOGNIZER
    ;  34 USER REC-USERVEC
        head(REC_USERVEC,REC-USERVEC,douser)
            dw 34

    ;Z VOCLINK      -- a-addr   address for VOCLINK wid
    ;  38 USER CURRENT
        head(VOCLINK,VOCLINK,douser)
            dw 38

    ;Z WORDLISTS      -- addr    address of WORDLIST list
    ; one cell for count, then 8 cells for LFA's of wordlists
    ;  48 USER WORDLISTS
        head(WORDLISTS,WORDLISTS,douser)
            dw 48


;Z INTVEC      -- a-addr   pointer to address holding interrupt vector
;  intvec_ptr CONSTANT INTVEC
    head(INTVEC,INTVEC,docon)
        dw intvec_ptr

;Z RST30VEC     -- a-addr   pointer to address holding RST30 vector
;  intvec_ptr CONSTANT RST30VEC
    head(RST30VEC,RST30VEC,docon)
        dw rst30vec_ptr

SECTION data_user
intvec_ptr:
        DEFW 0

rst30vec_ptr:
        DEFW 0

SECTION code_user_16k

dnl ;Z :NONAME       ( -- xt      define anonymous xt )
dnl ;    CURRENT @ @ , 0 C,    ( last link + immed flag )
dnl ;    HERE CURRENT @ !      ( new "latest" )
dnl ;    0 C,          ( empty NFA )
dnl ;    HERE               ( push xt to stack             )
dnl ;    HIDE ] !COLON  ;   ( start compiling as a docolon )
    head(NONAME,:NONAME,docolon)
        dw CURRENT,FETCH,FETCH,COMMA,lit,0,CCOMMA
        dw HERE,CURRENT,FETCH,STORE
        dw lit,0,CCOMMA
        dw HERE
        dw HIDE,RIGHTBRACKET,lit,docolon,COMMACF
        dw EXIT

;Z   MS ( n -- )  delay n milliseconds
    head(MS,MS,docode)
        push hl
        push de
        call asm_z80_delay_ms
        pop de
        pop hl
        pop bc
        next

;Z   TDELAY ( n -- )  delay in t-states
    head(TDELAY,TDELAY,docode)
        push hl
        push de
        ld h, b
        ld l, c
        call asm_z80_delay_tstate
        pop de
        pop hl
        pop bc
        next

; RC2014 EXTENSION output ====================


;Z VT-ESC  ( --  emit escape character )
;    27 EMIT [CHAR] [ EMIT ;
    head(VT_ESC,VT-ESC,docolon)
        dw lit,0x1b,EMIT
        dw lit,'[',EMIT
        dw EXIT

;Z CLS  ( --  clear screen )
;    VT-ESC ." 2J"
    head(CLS,CLS,docolon)
        dw VT_ESC, XSQUOTE
        db 2,"2J"
        dw TYPE
        dw EXIT

;Z AT-XY  ( x y -- move cursor to x,y )
;    VT-ESC 1+ (.) TYPE ." ;" 1+ (.) TYPE ." H" ;
    head(AT_XY,AT-XY,docolon)
        dw VT_ESC
        dw ONEPLUS,XDOT,TYPE
        dw lit,';',EMIT
        dw ONEPLUS,XDOT,TYPE
        dw lit,'H',EMIT
        dw EXIT

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
; : STACK-CLEAR ( lifo -- )
;      DUP    ( lifo lifo )
;      CELL+  ( lifo lifo+2 )
;      SWAP   ( lifo+2 lifo )
;      !     ;
    head(STACKCLEAR,STACK-CLEAR,docolon)
        DW DUP,CELLPLUS,SWOP,STORE
        DW EXIT

; : STACK-DEPTH ( lifo -- n )
;      STACK.BOUNDS - CELL /  ;
    head(STACKDEPTH,STACK-DEPTH,docolon)
        DW STACKBOUNDS,MINUS
        DW TWOSLASH      ;  optimize "DW CELL,SLASH" for 16bit
        DW EXIT

; : STACK-EMPTY? ( lifo -- flag )
;      STACK.BOUNDS = ;
    head(STACKEMPTYQ,STACK-EMPTY?,docolon)
        DW STACKBOUNDS,EQUAL
        DW EXIT

; Create parameters for a ?DO loop that will scan every item currently in STACK. The intended use is:
;      ( lifo )   STACK-BOUNDS ?DO -- CELL +LOOP
; : STACK-BOUNDS ( lifo -- addr1 addr2 )
;     DUP @ SWAP CELL+ ;
    head(STACKBOUNDS,STACK-BOUNDS,docolon)
        DW DUP,FETCH,SWOP,CELLPLUS
        DW EXIT

; Set the stack from the data stack
; : STACK-SET     ( rec-n .. rec-1 n lifo )
;    OVER IF
;      2DUP SWAP CELLS + CELL+ ( n lifo tos+2 )
;      DUP ROT !           ( ... n tos+2 )
;      SWAP 0 DO           ( rec-n rec-1 tos+2)
;         CELL- DUP >R ! R>   ( rec-n ... rec-i addr )
;      LOOP   DROP
;    ELSE
;      NIP STACK.CLEAR
;    THEN    ;
    head(STACKSET,STACK-SET,docolon)
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
; : STACK-GET  ( lifo -- rec-n .. rec-1 n )
;     DUP STACK.DEPTH    ( lifo n )
;     DUP IF             ( lifo n )
;         >R             ( lifo )
;         STACK-BOUNDS   ( addr1 addr2 )
;         DO              (  )
;             I @         ( rec-i )
;             CELL
;         +LOOP           ( rec-i )
;         R>              ( rec-i n )
;     ELSE                ( lifo n )
;         NIP             ( n )
;     THEN                ( )
;     ;
    head(STACKGET,STACK-GET,docolon)
        DW DUP,STACKDEPTH,DUP,qbranch,STACKGET2
        DW TOR,STACKBOUNDS,xdo
STACKGET1:
        DW II,FETCH,CELL,xplusloop,STACKGET1
        DW RFROM
        DW EXIT

STACKGET2:
        DW NIP
        DW EXIT

; : STACK-MAP  ( xt lifo -- )  ( execute xt for every item in stack )
; \ XT should not return anything on stack
;      DUP @ SWAP STACK-DEPTH   ( xt tos+2 n )
;      ?DUP IF   0 DO           ( xt tos+2 )
;         CELL-                 ( xt tos )
;         2DUP @ SWAP EXECUTE
;      LOOP   THEN
;      DROP DROP  ;
    head(STACKMAP,STACK-MAP,docolon)
        DW DUP,FETCH,SWOP,STACKDEPTH
        DW QDUP,qbranch,STACKMAP2
        DW lit,0,xdo
STACKMAP1:
        DW CELLMINUS,TWODUP,FETCH,SWOP,EXECUTE
        DW xloop,STACKMAP1
STACKMAP2:
        DW DROP,DROP
        DW EXIT

; : STACK-UNTIL  ( xt lifo -- x*i flag )  ( execute xt for every item in stack, until xt returns true )
; \ XT should return flag on stack
;      DUP @ SWAP STACK-DEPTH   ( xt tos+2 n )
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
    head(STACKUNTIL,STACK-UNTIL,docolon)
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

;Z U.R         ( u width --   right align )
;   0 SWAP D.R ;     ( quick convert to double )
    head(UDOTR,U.R,docolon)
        dw lit,0,SWOP,DDOTR
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

;Z PRINTABLE?         ( n - flag  is character printable? )
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

SECTION code_user_16k

;: WORDLIST ( -- wid )
; Create a new empty word list, returning its word list identifier wid.
; The new word list may be returned from a pool of preallocated word lists
; or may be dynamically allocated in data space. A system shall allow the
; creation of at least 8 new word lists in addition to any provided as part
; of the system.
;       WORDLISTS @ 1+ DUP 8 > IF
;           ABORT" ERROR:WID"
;       ELSE
;           DUP WORDLISTS !
;           DUP 0 SWAP WID>NFA!
;       THEN ;
        head(WORDLIST,WORDLIST,docolon)
            dw WORDLISTS,FETCH,ONEPLUS,DUP,lit,8,GREATER,qbranch,WORDLIST1
            dw XSQUOTE
            db 9,"ERROR:WID"
            dw COUNT,TYPE,ABORT
WORDLIST1:
            dw DUP,WORDLISTS,STORE
            dw DUP,lit,0,SWOP,WIDTONFASTORE
            dw EXIT

;: SEARCH-WORDLIST ( c-addr u wid -- 0 | xt 1 | xt -1 )
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

;: SAVE-ORDER       (addr -- )
;     >R GET-ORDER R>
;     2DUP !
;     SWAP DUP IF  ( n1..nx addr )
;         0 DO
;             CELL+ DUP >R ! R>
;         LOOP
;     ELSE  !·
;     THEN  DROP ;
        head(SAVE_ORDER,SAVE-ORDER,docolon)
            dw TOR,GET_ORDER,RFROM
            dw TWODUP,STORE
            dw SWOP,DUP,qbranch,SAVEORDER1
            dw lit,0,xdo
SAVEORDER2:
            dw CELLPLUS,DUP,TOR,STORE,RFROM
            dw xloop,SAVEORDER2
            dw branch,SAVEORDER3
SAVEORDER1:
            dw STORE
SAVEORDER3:
            dw DROP
            dw EXIT

;: RESTORE-ORDER    ( addr --  )
;     DUP @ DUP >R                              ( addr #n ; r: #n )
;     SWAP OVER CELLS + SWAP                    ( addr' #n ; r: #n )
;     DUP       IF
;         0 DO                                  ( addr' ; r: #n )
;             DUP >R @ R> 1 CELLS -
;         LOOP
;     THEN  DROP R>  SET-ORDER  ;
        head(RESTORE_ORDER,RESTORE-ORDER,docolon)
            dw DUP,FETCH,DUP,TOR
            dw SWOP,OVER,CELLS,PLUS,SWOP
            dw DUP,qbranch,RESTOREORDER1
            dw lit,0,xdo
RESTOREORDER2:
            dw DUP,TOR,FETCH,RFROM,lit,1,CELLS,MINUS
            dw xloop,RESTOREORDER2
RESTOREORDER1:
            dw DROP,RFROM,SET_ORDER
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

;C FIND-NAME-IN   c-addr len wid   --  c-addr len 0       if not found
;                                      c-addr len nfa     if found
;   WID>NFA DUP 0= IF DROP 0 EXIT THEN
;   BEGIN                      -- a len nfa
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
        DW WIDTONFA
        DW DUP,ZEROEQUAL,qbranch,FINDIN1
        DW DROP,lit,0,EXIT
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
FIND_16K:
        call docolon
        DW DUP,COUNT,FIND_NAME
        DW DUP,qbranch,FINDNG1
        DW NIP,DUP,NFATOCFA
        DW SWOP,IMMEDQ
        DW ZEROEQUAL,lit,1,OR
FINDNG1:
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
;   VOC-LINK @ WID>NFA , 0 C,         link & `immed' field
;   HERE VOC-LINK @ WID>NFA!           new "latest" link
;   BL WORD C@ 1+ ALLOT         name field
;   docreate ,CF                code field
;   WORDLIST ,
;
;    DOES>  @ STACK_WORDLISTS TOSTACK
;  ;
    head(VOCABULARY,VOCABULARY,docolon)
        dw VOCLINK,FETCH,WIDTONFA,COMMA,lit,0,CCOMMA
        dw HERE,VOCLINK,FETCH,WIDTONFASTORE
        dw BL,WORD,CFETCH,ONEPLUS,ALLOT
        dw lit,docreate,COMMACF
        dw WORDLIST,COMMA

        dw XVOCDOES
        call dodoes
        DW FETCH,lit,STACK_WORDLISTS,STACKSTORE
        dw EXIT

; patch latest entry on VOCLINK with code from VOCDOES
    head(XVOCDOES,(VOCDOES>),docolon)
        DW RFROM,VOCLINK,FETCH,WIDTONFA,NFATOCFA,STORECF
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
;   (WORDS) STACK_WORDLISTS STACK.MAP ;
    head(VLIST,VLIST,docolon)
        DW lit,XWORDS,lit,STACK_WORDLISTS,STACKMAP
        DW EXIT
; ================================================

; PICK  ( xu...x1 x0 u -- xu...x1 x0 xu )
; Remove u. Copy the xu to the top of the stack. An ambiguous
; condition exists if there are less than u+2 items on the stack
; before PICK is executed.
;   1+ CELLS SP@ + @ ;
    head(PICK,PICK,docolon)
        DW ONEPLUS,CELLS,SPFETCH,PLUS,FETCH
        DW EXIT

; 2=  ( d1 d2 | n1 n2 n3 n4 -- flag )  true if d1=d2
;                                      or (n1=n3 AND n2=n4)
;   ROT =   ( n1 n2 f )
;   SWAP ROT =   ( n1 n2 f )
;   AND ;
    head(TWOEQUAL,2=,docolon)
        DW ROT,EQUAL
        DW SWOP,ROT,EQUAL
        DW AND
        DW EXIT

; BLOCK implementation ==========================

; BLOCKCTX structure
;   Each context struct is indexed to a 1024byte block buffer
;    DISK number (1 cell)
;    BLOCK number (1 cell)
;    BUFFER address (1 cell)
;    BLOCK update flag (1 cell)

DEFC BLOCKCTX_SIZE = 8
DEFC BLOCKCTX_NUM = 4
DEFC BLOCK_FIRST = 0xE000

;Z /BLKCTX   ( -- ) initialise the block contexts
;    BLKCTX_PTR  BLKCTX% BLKCTX# *  0 FILL
;    0 BLKCTX_IDX !    ;
    head(SLASHBLKCTX,/BLKCTX,docolon)
        dw lit,BLKCTX_PTR
        dw BLKCTXSIZE,BLKCTXNUM,STAR,lit,0,FILL
        dw lit,0,lit,BLKCTX_IDX,STORE
        dw EXIT

;Z BLKCTX>DISK  ( ctx -- a-addr' )  get address of DISK number
;    ;
    head(BLKCTXTODISK,BLKCTX>DISK,docolon)
        dw EXIT

;Z BLKCTX>BLOCK  ( ctx -- a-addr' )  get address of BLOCK number
;    ;
    head(BLKCTXTOBLOCK,BLKCTX>BLOCK,docolon)
        dw lit,2,PLUS
        dw EXIT

;Z BLKCTX>BUFFER  ( ctx -- a-addr' )  get address of BUFFER
;    ;
    head(BLKCTXTOBUFFER,BLKCTX>BUFFER,docolon)
        dw lit,4,PLUS
        dw EXIT

;Z BLKCTX>FLAGS  ( ctx -- a-addr' )  get address of FLAGS
;    ;
    head(BLKCTXTOFLAGS,BLKCTX>FLAGS,docolon)
        dw lit,6,PLUS
        dw EXIT

;Z BLKCTX%  ( a-addr -- a-addr' )  size of stucture
    head(BLKCTXSIZE,``BLKCTX%'',docon)
        dw BLOCKCTX_SIZE

;Z BLKCTX#  ( a-addr -- a-addr' )  number of buffer structures
    head(BLKCTXNUM,``BLKCTX#'',docon)
        dw BLOCKCTX_NUM


;Z BLKFIRST      -- a-adrs      address of first block buffer
;   RAMTOP @ 0xFC00 AND 0x1000 - ;
    head(BLKFIRST,BLKFIRST,docolon)
        dw RAMTOP,FETCH,lit,0xFC00,AND,lit,0x1000,MINUS
        dw EXIT

;Z BLKCTX-NEXT  ( -- ctx )  increment buffer structure
;   BLKCTX_PTR   BLKCTX_IDX @
;   BLKCTX% * +  ( new-ctx )
;   BLKCTX_IDX @  B/BLK *  BLKFIRST PLUS  ( new-ctx buffer )
;   OVER BLKCTX>BUFFER !  ( new-ctx )
;   BLKCTX_IDX @ 1+ BLKCTXNUM MOD  BLKCTX_IDX !
;   ;
    head(BLKCTX_NEXT,BLKCTX-NEXT,docolon)
        dw lit,BLKCTX_PTR
        dw lit,BLKCTX_IDX,FETCH
        dw BLKCTXSIZE,STAR,PLUS  ; TODO: (FLUSH) this ctx
        dw lit,BLKCTX_IDX,FETCH,B_BLK,STAR,BLKFIRST,PLUS
        dw OVER,BLKCTXTOBUFFER,STORE
        dw lit,BLKCTX_IDX,FETCH,ONEPLUS,BLKCTXNUM,MOD,lit,BLKCTX_IDX,STORE
        dw EXIT

;Z BLKCTX-FIND   blk disk -- ctx    address of matching buffer, if exists, else 0
;    BLKCTX_PTR BLKCTX# 0 DO   ( blk dsk ctx[i] )
;       >R                ( blk dsk ; r: ctx[i] )
;       2DUP              ( blk dsk blk dsk ; r: ctx[i] )
;       R@ BLKCTX>BLOCK @  ( blk dsk blk dsk blk[i] ; r: ctx[i] )
;       R@ BLKCTX>DISK  @  ( blk dsk blk dsk blk[i] dsk[i] ; r: ctx[i] )
;       2=  IF            ( blk dsk ; r: ctx[i] )
;           2DROP R> UNLOOP EXIT
;       THEN
;       R> BLKCTX% + ( blk dsk ctx[i+1] )
;    LOOP
;    2DROP DROP 0   ;
    head(BLKCTX_FIND,BLKCTX-FIND,docolon)
        dw lit,BLKCTX_PTR,BLKCTXNUM,lit,0,xdo
BLKCTXF1:
        dw TOR
        dw TWODUP
        dw RFETCH,BLKCTXTOBLOCK,FETCH
        dw RFETCH,BLKCTXTODISK,FETCH
        dw TWOEQUAL,qbranch,BLKCTXF2
        dw TWODROP,RFROM,UNLOOP
        dw EXIT
BLKCTXF2:
        dw RFROM,BLKCTXSIZE,PLUS
        dw xloop,BLKCTXF1
        dw TWODROP,DROP,lit,0
        dw EXIT

;Z BLKCTX-GET  ( blk dsk -- ctx )  increment buffer structure
;     2DUP BLKCTX-FIND ?DUP IF   ( blk dsk ctx )
;         NIP NIP
;     ELSE                       ( blk dsk )
;         BLKCTX-NEXT    ( blk dsk ctx )
;         >R
;         R@ BLKCTX>DISK !
;         R@ BLKCTX>BLOCK !
;         R@ BLKCTX>FLAGS 1 SWAP !
;         R>  ;
    head(BLKCTX_GET,BLKCTX-GET,docolon)
        dw TWODUP,BLKCTX_FIND,QDUP,qbranch,BLKCTXG1
        dw NIP,NIP
        dw EXIT
BLKCTXG1:
        dw BLKCTX_NEXT
        dw TOR
        dw RFETCH,BLKCTXTODISK,STORE
        dw RFETCH,BLKCTXTOBLOCK,STORE
        dw RFETCH,BLKCTXTOFLAGS,lit,1,SWOP,STORE
        dw RFROM
        dw EXIT

;Z BLKCTX-MAP   xt --     execute xt for each blkctx
;    BLKCTX_PTR BLKCTX# 0 DO   ( xt ctx[i] )
;       >R                ( xt ; r: ctx[i] )
;       DUP R@ SWAP EXECUTE
;       R> BLKCTX% +      ( xt ctx[i+1] )
;    LOOP   2DROP  ;
    head(BLKCTX_MAP,BLKCTX-MAP,docolon)
        dw lit,BLKCTX_PTR,BLKCTXNUM,lit,0,xdo
BLKCTXMAP1:
        dw TOR
        dw DUP
        dw RFETCH,SWOP,EXECUTE
        dw RFROM,BLKCTXSIZE,PLUS
        dw xloop,BLKCTXMAP1
        dw TWODROP
        dw EXIT


SECTION bss_user

BLKCTX_PTR:
        DEFW 0,0,0,0
        DEFW 0,0,0,0
        DEFW 0,0,0,0
        DEFW 0,0,0,0

BLKCTX_IDX:
        DEFW 0

SECTION code_user_16k

;Z CF-BLOCK-READ  ( dsk blk adrs -- )   Compact Flash read block  BLK and DSK
; Reads the block from the Compact Flash card into memory
; address found at 'adrs'. 'dsk' and 'blk' are the disk
; and block numbers respectively
    head(CF_BLOCK_READ,CF-BLOCK-READ,docolon)
BLOCK_READ1:
        dw lit,cflash_read_block,CALL
        dw branch,BLOCK_READ3

BLOCK_READ2:
        dw INVERT,XSQUOTE
        db 9,"NO DRIVER"
        dw QABORT,EXIT

BLOCK_READ3:
        dw INVERT,XSQUOTE
        db 10,"READ ERROR"
        dw QABORT
        dw EXIT

;Z BLOCK-WRITE  ( dsk blk adrs -- )  Compact Flash write BLK and DSK
; Reads the block from the Compact Flash card into memory
; address found at 'adrs'. 'dsk' and 'blk' are the disk
; and block numbers respectively
    head(CF_BLOCK_WRITE,CF-BLOCK-WRITE,docolon)
BLOCK_WRITE1:
        dw lit,cflash_write_block,CALL
        dw branch,BLOCK_WRITE3

BLOCK_WRITE2:
        dw INVERT,XSQUOTE
        db 9,"NO DRIVER"
        dw QABORT,EXIT

BLOCK_WRITE3:
        dw INVERT,XSQUOTE
        db 11,"WRITE ERROR"
        dw QABORT
        dw EXIT

;Z CF-BLOCK-READWRITE  ( dsk blk adrs f -- )  read or write block
;     IF  CF-BLOCK-WRITE  ELSE  CF-BLOCK-READ  THEN ;
    head(CF_BLOCK_READWRITE,CF-BLOCK-READWRITE,docolon)
        dw qbranch,CFBRW1
        dw CF_BLOCK_WRITE,branch,CFBRW2
CFBRW1:
        dw CF_BLOCK_READ
CFBRW2:
        dw EXIT

;Z BLOCK-READWRITE    ( ctx f -- )  read or write block
;                              f = 0 read, f = -1 write
;     >R >R
;     R@ BLKCTX>DISK @
;     R@ BLKCTX>BLOCK @
;     BLKOFFSET @ +
;     R@ BLKCTX>BUFFER @
;     0 R@ BLKCTX>FLAGS !
;     R> DROP  R>      ( dsk blk adrs f )
;     BLKRWVEC @ EXECUTE ;
    head(BLOCK_READWRITE,BLOCK-READWRITE,docolon)
        dw TOR,TOR
        dw RFETCH,BLKCTXTODISK,FETCH
        dw RFETCH,BLKCTXTOBLOCK,FETCH
        dw BLKOFFSET,FETCH,PLUS
        dw RFETCH,BLKCTXTOBUFFER,FETCH
        dw lit,0,RFETCH,BLKCTXTOFLAGS,STORE
        dw RFROM,DROP,RFROM
        dw BLKRWVEC,FETCH,EXECUTE
        dw EXIT

;Z (BUFFER)      n -- ctx    get buffer context
;     DUP BLKLIMIT @ U< IF
;     DUP BLK !
;     DSK @
;     BLKCTX-GET
;     ELSE  ABORT" BLOCK OUT OF RANGE" THEN ;
    head(XBUFFER,(BUFFER),docolon)
        dw DUP,BLKLIMIT,FETCH,ULESS,qbranch,XBUFFER1
        dw DUP,BLK,STORE
        dw DSK,FETCH
        dw BLKCTX_GET
        dw EXIT
XBUFFER1:
        dw XSQUOTE
        db 18,"BLOCK OUT OF RANGE"
        DW TYPE
        dw ABORT


;C BUFFER        n -- addr         push buffer address
;     (BUFFER)
;     BLKCTX>BUFFER @  ;
    head(BUFFER,BUFFER,docolon)
        dw XBUFFER
        dw BLKCTXTOBUFFER,FETCH
        dw EXIT

;C BLOCK                    n -- addr    load block
;                                        0 if block 0
;        (BUFFER)     ( ctx )
;        DUP BLKCTX>FLAGS @ 1 = IF  ( ctx )
;            DUP 0 BLOCK-READWRITE
;        THEN
;        BLKCTX>BUFFER @
;        ;
    head(BLOCK,BLOCK,docolon)
        dw XBUFFER
        dw DUP,BLKCTXTOFLAGS,FETCH,lit,1,EQUAL,qbranch,BLOCK1
        dw DUP,lit,0,BLOCK_READWRITE
BLOCK1:
        dw BLKCTXTOBUFFER,FETCH
        dw EXIT

;C UPDATE                    --    mark block to update
;     BLK @ DSK @ BLKCTX-FIND ?DUP IF
;        BLKCTX>FLAGS -1 SWAP !
;     THEN ;
    head(UPDATE,UPDATE,docolon)
        dw BLK,FETCH,DSK,FETCH,BLKCTX_FIND,QDUP,qbranch,UPDATE1
        dw BLKCTXTOFLAGS,lit,0xffff,SWOP,STORE
UPDATE1:
        dw EXIT

;C UPDATED?                 blk -- f   is block updated?
;     DSK @ BLKCTX-FIND DUP IF
;         BLKCTX>FLAGS @
;     THEN ;
    head(UPDATEDQ,UPDATED?,docolon)
        dw DSK,FETCH,BLKCTX_FIND,DUP,qbranch,UPDATEDQ1
        dw BLKCTXTOFLAGS,FETCH
UPDATEDQ1:
        dw EXIT

;Z (FLUSH)  ( ctx -- )  flush blkctx to disk
;    DUP BLKCTX>FLAGS @ -1 = IF    ( ctx )
;      DUP -1 BLOCK-READWRITE        ( ctx )
;      BLKCTX>FLAGS 0 SWAP !
;    ELSE DROP
;    THEN ;
    head(XFLUSH,(FLUSH),docolon)
        dw DUP,BLKCTXTOFLAGS,FETCH,lit,-1,EQUAL,qbranch,FLUSH1
        dw DUP,lit,-1,BLOCK_READWRITE
        dw BLKCTXTOFLAGS,lit,0,SWOP,STORE
        dw EXIT
FLUSH1:
        dw DROP,EXIT

;C FLUSH                    --    flush blocks to disk
;     ' (FLUSH) BLKCTX-MAP  ;
    head(FLUSH,FLUSH,docolon)
        dw lit,XFLUSH,BLKCTX_MAP
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

;C +LOAD                  n  --    load block BLK + n
;     BLK @ + LOAD  ;
    head(PLUSLOAD,+LOAD,docolon)
        dw BLK,FETCH,PLUS,LOAD
        dw EXIT

;C THRU            n1 n2  --    load blocks n1 to n2
;     1+ SWAP DO I LOAD LOOP ;
    head(THRU,THRU,docolon)
        dw ONEPLUS,SWOP,xdo
THRU1:
        dw II,DUP,DOT,LOAD,xloop,THRU1
        dw EXIT

;C +THRU            n1 n2  --    load blocks BLK+n1 to BLK+n2
;     1+ SWAP DO I +LOAD LOOP ;
    head(PLUSTHRU,+THRU,docolon)
        dw ONEPLUS,SWOP,xdo
PLUSTHRU1:
        dw II,DUP,DOT,PLUSLOAD,xloop,PLUSTHRU1
        dw EXIT

;C COPY            n1 n2  --    copy block n1 to n2
;   SWAP BLOCK  ( n2 blk1 )
;   SWAP BLOCK  ( blk1 blk2 )
;   B/BLK MOVE UPDATE ;
    head(COPY,COPY,docolon)
        dw SWOP,BLOCK,SWOP,BLOCK,B_BLK,MOVE,UPDATE
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

;C TYPE$    c-addr +n --     type line of printable characters to term'l
;   ?DUP IF
;     OVER + SWAP DO I C@
;       DUP PRINTABLE? INVERT IF DROP [CHAR] . THEN EMIT
;     LOOP
;   ELSE DROP THEN ;
    head(TYPESTRING,TYPE$,docolon)
        DW QDUP,qbranch,TYPS4
        DW OVER,PLUS,SWOP,xdo
TYPS3:  DW II,CFETCH

        DW DUP,PRINTABLEQ,INVERT,qbranch,TYPS2
        dw DROP,lit,46
TYPS2:  DW EMIT

        DW xloop,TYPS3
        DW branch,TYPS5
TYPS4:  DW DROP
TYPS5:  DW EXIT

;Z LL               line# --      List Line
;     (LINE) C/L TYPE CR ;
    head(LL,LL,docolon)
        dw XLINE,C_L,TYPESTRING,CR
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

; snapshots
;    offset | field
;    0000   | copy of USER area (128 bytes)
;    0080   | copy of Wordlist ROM NFA links (16 bytes)
;              these are NFAs of User words that have their
;              LFAs point to ROM. They need to be patched in
;              the likely case that the ROM is updated.
;              Currently, the ROM'd wordlists are
;              FORTH, EDITOR, VOCS.
;              If zero, then the matching wordlist hasn't been
;              extended by the user.
;    0090   | copy of Wordlist order (34 bytes)

;: #SNAPSHOT
;    128 CONSTANT #SNAPSHOT
    head(NUMSNAPSHOT,``#SNAPSHOT'',docon)
        dw 128+16+34

;: SNAPSHOT.USER   ( snapaddr -- addr     ptr to user area in snapshot )
    head(SNAPSHOTDOTUSER,SNAPSHOT.USER,docolon)
        dw EXIT

;: SNAPSHOT.LINKS   ( snapaddr -- addr     ptr to wordlists ROM links in snapshot )
    head(SNAPSHOTDOTLINKS,SNAPSHOT.ORDER,docolon)
        dw lit,0x80,PLUS,EXIT

;: SNAPSHOT.ORDER   ( snapaddr -- addr     ptr to wordlist order in snapshot )
    head(SNAPSHOTDOTORDER,SNAPSHOT.ORDER,docolon)
        dw lit,0x90,PLUS,EXIT

;: >SNAPSHOT  ( snapaddr  -- )
;        DUP #SNAPSHOT ERASE
;        DUP SNAPSHOT.USER U0 SWAP 128 MOVE
;        DUP SNAPSHOT.ORDER SAVE-ORDER
;        DROP   ;
    head(TOSNAPSHOT,>SNAPSHOT,docolon)
        dw DUP,NUMSNAPSHOT,ERASE
        dw DUP,SNAPSHOTDOTUSER,U0,SWOP,lit,128,MOVE
        dw DUP,SNAPSHOTDOTORDER,SAVE_ORDER
        dw DROP
        dw EXIT

; USER space index to restore
defc SNAPSHOT_RST_START = 24
defc SNAPSHOT_RST_LEN = 128-24

;: SNAPSHOT>  ( snapaddr  --   restore snapshot )
;        DUP SNAPSHOT.USER SNAPSHOT_RST_START +
;           U0 SNAPSHOT_RST_START +   SNAPSHOT_RST_LEN MOVE
;        DUP SNAPSHOT.ORDER RESTORE-ORDER
;        DUP SNAPSHOT.USER 8 + @ DP !
;        DROP   ;
    head(SNAPSHOTFROM,SNAPSHOT>,docolon)
        dw DUP,SNAPSHOTDOTUSER,lit,SNAPSHOT_RST_START,PLUS
        dw U0,lit,SNAPSHOT_RST_START,PLUS,lit,SNAPSHOT_RST_LEN,MOVE
        dw DUP,SNAPSHOTDOTORDER,RESTORE_ORDER
        dw DUP,SNAPSHOTDOTUSER,lit,8,PLUS,FETCH,DP,STORE
        dw DROP
        dw EXIT

;: MARKER  ( "name" -- )
;    HERE 32 + DUP R> >SNAPSHOT  ( temp copy ; save snapshot pointer )
;    CREATE
;        R> HERE #SNAPSHOT MOVE
;        #SNAPSHOT CHARS ALLOC
;    DOES>
;       SNAPSHOT> ;
    head(MARKER,MARKER,docolon)
        DW HERE,lit,32,PLUS,DUP,TOR,TOSNAPSHOT
        DW CREATE
        DW RFROM,HERE,NUMSNAPSHOT,MOVE
        DW NUMSNAPSHOT,CHARS,ALLOT

        DW XDOES
        call dodoes
        DW SNAPSHOTFROM
        DW XSQUOTE
        db 3," OK"
        dw TYPE,CR,QUIT
        dw EXIT

; (BSAVE) ( c-addr u blk -- )  save block to file
;    BUFFER                               ( c-addr u buffer )
;    SWAP MOVE
;    UPDATE  ;
    head(XBSAVE,(BSAVE),docolon)
        dw BUFFER
        dw SWOP,MOVE
        dw UPDATE
        dw EXIT

;: BSAVE   ( c-addr u blk -- )
;    SWAP B/BLK /MOD    ( c-addr blk rem #blks )
;    SWAP   >R      (c-addr blk #blks ; rem )
;    ?DUP IF
;      0 DO                  ( c-addr blk ; rem )
;        2DUP B/BLK SWAP (BSAVE)  ( c-addr blk ; rem )
;        SWAP B/BLK +             ( blk c-addr' ; rem )
;        SWAP 1+                  ( c-addr' blk' ; rem )
;      LOOP
;    THEN           ( c-addr blk ; rem )
;
;    R> SWAP        ( c-addr rem blk )
;    OVER IF (BSAVE) ELSE 2DROP DROP THEN
;    FLUSH  ;
    head(BSAVE,BSAVE,docolon)
        dw SWOP,B_BLK,SLASHMOD
        dw SWOP,TOR
        dw QDUP,qbranch,BSAVE1
        dw lit,0,xdo
BSAVE2:
        dw TWODUP,B_BLK,SWOP,XBSAVE
        dw SWOP,B_BLK,PLUS
        dw SWOP,ONEPLUS
        dw xloop,BSAVE2
BSAVE1:
        dw RFROM,SWOP
        dw OVER,qbranch,BSAVE3
        dw XBSAVE
        dw branch,BSAVE4
BSAVE3:
        dw TWODROP,DROP
BSAVE4:
        dw FLUSH
        dw EXIT

; (BLOAD) ( c-addr u blk -- )  load block from disks
;    BLOCK                            ( c-addr u buffer )
;    ROT ROT MOVE    ;
    head(XBLOAD,(BLOAD),docolon)
        dw BLOCK
        dw ROT,ROT,MOVE
        dw EXIT

;: BLOAD   ( c-addr u blk -- )
;    SWAP B/BLK /MOD    ( c-addr blk rem #blks )
;    SWAP   >R      (c-addr blk #blks ; rem )
;    ?DUP IF
;      0 DO                  ( c-addr blk ; rem )
;        2DUP B/BLK SWAP (BLOAD)   ( c-addr blk ; rem )
;        SWAP B/BLK +              ( blk c-addr' ; rem )
;        SWAP 1+                   ( c-addr' blk' ; rem )
;      LOOP
;    THEN           ( c-addr blk ; rem )
;
;    R> SWAP        ( c-addr rem blk )
;    OVER IF (BLOAD) ELSE 2DROP DROP THEN
;     ;
    head(BLOAD,BLOAD,docolon)
        dw SWOP,B_BLK,SLASHMOD
        dw SWOP,TOR
        dw QDUP,qbranch,BLOAD1
        dw lit,0,xdo
BLOAD2:
        dw TWODUP,B_BLK,SWOP,XBLOAD
        dw SWOP,B_BLK,PLUS
        dw SWOP,ONEPLUS
        dw xloop,BLOAD2
BLOAD1:
        dw RFROM,SWOP
        dw OVER,qbranch,BLOAD3
        dw XBLOAD
        dw branch,BLOAD4
BLOAD3:
        dw TWODROP,DROP
BLOAD4:
        dw EXIT


defc BLK_HEADER_SIZE = 256
defc BLK_DATA_SIZE   = 1024-BLK_HEADER_SIZE

; (SAVEHDR)            ( c-addr u buffer -- c-addr' u' )
; --------------------------------------------------------------
; Header
;    size of header
;    size of data
;    snapshot
; --------------------------------------------------------------
;    Setup header
;    BUFFER     ( c-addr u buffer -- ; blk in BLK)
;      DUP BLK_HEADER_SIZE ERASE ( c-addr u buffer ; erase header at buffer )
;      BLK_HEADER_SIZE OVER !    ( c-addr u buffer ; store header size at buffer+0 )
;      CELL+ 2DUP !              ( c-addr u buffer+2 ; store data size at buffer+2 )
;      CELL+ >SNAPSHOT           ( c-addr u ; store SNAPSHOT )
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
        dw CELLPLUS,TOSNAPSHOT

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
;    DUP WIPE
;    enddict   DP @ enddict -    ( blk c-addr u )
;    ROT BUFFER                  ( c-addr u buffer -- ; blk in BLK)
;    (BSAVEHDR)                  ( c-addr' u' )
;
;    DUP IF
;      BLK @ 1+ BSAVE
;    ELSE
;      2DROP
;    THEN   ;
    head(SAVE,SAVE,docolon)
        dw DUP,WIPE
        dw lit,enddict,DP,FETCH,lit,enddict,MINUS  ;  ( block c-addr u )
        dw ROT,BUFFER                              ;  ( c-addr u buffer )
        dw XSAVEHDR

        dw DUP,qbranch,SAVE1
        dw BLK,FETCH,ONEPLUS,BSAVE
        dw branch,SAVE2
SAVE1:
        dw TWODROP
SAVE2:
        dw EXIT

;: RESTORE   ( blk -- )
;    BLOCK                       ( buffer -- ; blk in BLK)
;    DUP @                       ( buffer hdr_size )
;    OVER +                      ( buffer buffer' )
;    SWAP CELL+ DUP @            ( buffer' buffer data_size )
;    SWAP CELL+ SNAPSHOT>    ( buffer' data_size )
;
;    SWAP enddict ROT       ( buffer' enddict u)
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
;    DUP IF    ( c-addr' u' )
;      BLK @ 1+ BLOAD
;    ELSE
;      2DROP
;    THEN    ;
    head(RESTORE,RESTORE,docolon)
        dw BLOCK
        dw DUP,FETCH               ; header size
        dw OVER,PLUS
        dw SWOP,CELLPLUS,DUP,FETCH
        dw SWOP,CELLPLUS
        dw SNAPSHOTFROM

        dw SWOP,lit,enddict,ROT

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
        dw DUP,qbranch,RESTORE3
        dw BLK,FETCH,ONEPLUS,BLOAD
        dw branch,RESTORE4
RESTORE3:
        dw TWODROP
RESTORE4:
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

; :NONAME  ABORT" ?"  ;
REC_NULL_XT:
        call docolon
        DW XSQUOTE
        DB 1,"?"
        DW QABORT

; ' NOOP ' NOOP ' NOOP  RECTYPE: RECTYPE-NULL
    head(RECTYPE_NULL,RECTYPE-NULL,docreate)
        dw REC_NULL_XT
        dw REC_NULL_XT
        dw REC_NULL_XT

; ' NOOP ' NOOP ' NOOP  RECTYPE: RECTYPE-NOOP
    head(RECTYPE_NOOP,RECTYPE-NOOP,docreate)
        dw NOOP
        dw NOOP
        dw NOOP

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

    head(STACK_RECOGNIZER,STACK-RECOGNIZER,docreate)
        DW STACK_RECOGNIZER_END
        ;DW REC_IHEX
        DW REC_NUMBER
        DW REC_USER
        DW REC_FIND
STACK_RECOGNIZER_END:

;: REC-FIND ( addr len -- XT flags RECTYPE_XT  |  RECTYPE_NULL )
;    FIND-NAME            ( 0       if not found )
;                         ( nfa     if found )
;    ?DUP IF
;       DUP NFA>CFA            -- nfa xt
;       SWAP IMMED?            -- xt iflag
;       0= 1 OR                -- xt 1/-1
;       RECTYPE-XT
;    ELSE
;       RECTYPE-NULL
;    THEN   ;
    head(REC_FIND,REC-FIND,docolon)
        DW FIND_NAME
        DW QDUP,qbranch,REC_FIND1
        DW DUP,NFATOCFA
        DW SWOP,IMMEDQ
        DW ZEROEQUAL,lit,1,OR
        DW RECTYPE_XT
        DW EXIT
REC_FIND1:
        DW RECTYPE_NULL
        DW EXIT

; :NONAME ( i*x XT flags -- j*y )  \ INTERPRET
;   DROP EXECUTE ;
REC_FIND_XT:
        call docolon
        DW DROP,EXECUTE
        DW EXIT

; :NONAME ( XT flags -- )          \ COMPILE
;   0< IF COMPILE, ELSE EXECUTE THEN ;
REC_FIND_COMP:
        call docolon
        DW ZEROLESS,qbranch,REC_FIND_COMP1
        DW COMMAXT
        DW branch,REC_FIND_COMP2
REC_FIND_COMP1:
        DW EXECUTE
REC_FIND_COMP2:
        DW EXIT

; :NONAME POSTPONE 2LITERAL  ; ( XT flag -- )
;    ( stores the XT and flag into definition )
REC_FIND_POST:
        call docolon
        DW TWOLITERAL
        DW EXIT

;    RECTYPE: RECTYPE-XT
    head(RECTYPE_XT,RECTYPE-XT,docreate)
        DW REC_FIND_XT
        DW REC_FIND_COMP
        DW REC_FIND_POST

;: REC-NUMBER ( addr len -- n RECTYPE_NUM  |  RECTYPE_NULL )
;   0 0 2SWAP               -- ud adr n
;   ?SIGN >R  >NUMBER       -- ud adr' n'
;   IF   R> 2DROP 2DROP 0   -- 0   (error)
;   ELSE 2DROP R>
;       IF NEGATE THEN  -1  -- n -1   (ok)
;   THEN
;    IF
;       RECTYPE-NUM
;    ELSE
;       RECTYPE-NULL
;    THEN   ;
    head(REC_NUMBER,REC-NUMBER,docolon)
        DW lit,0,DUP,TWOSWAP
        DW QSIGN,TOR,TONUMBER,qbranch,RECNUM1
        DW RFROM,TWODROP,TWODROP,lit,0
        DW branch,RECNUM3
RECNUM1:  DW TWODROP,RFROM,qbranch,RECNUM2,NEGATE
RECNUM2:  DW lit,-1

RECNUM3:
        DW qbranch,RECNUM4
        DW RECTYPE_NUM
        DW EXIT
RECNUM4:
        DW RECTYPE_NULL
        DW EXIT


;    RECTYPE: RECTYPE-NUM
    head(RECTYPE_NUM,RECTYPE-NUM,docreate)
        DW NOOP
        DW LITERAL
        DW LITERAL

; : POSTPONE ( "name" -- )  \ COMPILE
; This is the 16K ROM Next Generation version
;   BL WORD  COUNT
;     STACK_RECOGNIZER RECOGNIZE   ( xt flags RECTYPE_XT | RECTYPE_NULL )
;     DUP
;     >R                 ( call POST action )
;     RECTYPE>POST EXECUTE
;     R>
;     RECTYPE>COMP COMMA   ( add compile action to definition )     ;
POSTPONE_16K:
        call docolon
        DW BL,WORD,COUNT
        DW STACK_RECOGNIZER,RECOGNIZE
        DW DUP,TOR
        DW RECTYPETOPOST,EXECUTE
        DW RFROM,RECTYPETOCOMP,COMMA
        DW EXIT


;Z INTERPRET    i*x c- -- j*x
;Z                      interpret given buffer
; This is a common factor of EVALUATE and QUIT.
; ref. dpANS-6, 3.4 The Forth Text Interpreter
; This is the 16K ROM Next Generation version
;   BEGIN
;   BL WORD DUP C@ WHILE        -- textadr
;       DUP >R COUNT            -- c-addr n  ; textadr
;       STACK_RECOGNIZER RECOGNIZE   ( i*x RECTYPE_XXX | RECTYPE_NULL )
;       DUP RECTYPE_NULL <> IF  -- i*x RECTYPE_XXX
;           STATE @ IF
;             RECTYPE>COMP EXECUTE
;           ELSE
;             RECTYPE>INT EXECUTE
;           THEN
;           R> DROP
;       ELSE
;           DROP R> COUNT TYPE 3F EMIT CR ABORT  err
;       THEN
;   REPEAT DROP ;
INTERPRET_16K:
        call docolon
INTRP_NG1: DW BL,WORD,DUP,CFETCH,qbranch,INTRP_NG9
           DW DUP,TOR,COUNT
           DW STACK_RECOGNIZER,RECOGNIZE
           DW DUP,RECTYPE_NULL,NOTEQUAL,qbranch,INTRP_NG2
           DW STATE,FETCH,qbranch,INTRP_NG3
           DW RECTYPETOCOMP,EXECUTE
           DW branch,INTRP_NG4
INTRP_NG3: DW RECTYPETOINT,EXECUTE
INTRP_NG4: DW RFROM,DROP,branch,INTRP_NG5
INTRP_NG2: DW DROP,RFROM,COUNT,TYPE,lit,63,EMIT,CR,ABORT
INTRP_NG5: DW branch,INTRP_NG1
INTRP_NG9: DW DROP
        DW EXIT






;  NONAME:    ( src dest len --     xt for rectype-ihex )
REC_IHEX_XT:
        call docolon
        DW XIHEX
        DW EXIT

;  NONAME:    ( src dest len --     compile action for rectype-ihex )
;     SWAP LITERAL SLITERAL
;     ['] (IHEX) ,  ;
REC_IHEX_COMP:
        call docolon
        DW IHEXCOMMA
        DW EXIT

; RECTYPE: RECTYPE-IHEX ;
    head(RECTYPE_IHEX,RECTYPE-IHEX,docreate)
        DW REC_IHEX_XT
        DW REC_IHEX_COMP
        DW NOOP


;: REC-IHEX ( addr len -- src dest n RECTYPE_IHEX   if ok, RECTYPE_NULL if not recognised )
;    IHEX? DUP 1 = IF
;       DROP NIP NIP
;       RECTYPE_NOOP EXIT
;    THEN
;    DUP 0 = IF
;       DROP NIP NIP
;       RECTYPE_NULL EXIT
;    THEN
;    DROP RECTYPE_IHEX  ;
;
;  old:
;    DROP
;    0 IHXCRC !
;    DUP C@ [CHAR] : <> IF DROP RECTYPE_NULL  ( no colon ) EXIT  THEN
;
;    CHAR+
;    IHXBYTE   ( count tib-ptr )
;    OVER PAD CELL+ !
;    IHXWORD   ( count hex-addr tib-ptr )
;    SWAP PAD !  ( count tib-ptr )
;    PAD CELL+ CELL+ SWAP     ( count addr tib-ptr )
;    IHXBYTE   ( count hex-addr record-type tib-ptr )
;
;    OVER 0=  IF NIP           ( count addr tib-ptr )
;        IHXREC!               ( addr tib-ptr )
;        (IHXBYTE)             ( addr crc tib-ptr )
;        DROP NIP              ( crc )
;    ELSE      ( count hex-addr record-type tib-ptr )
;        DROP NIP NIP
;        1 = IF      ( end of hex record? )
;           RECTYPE_NOOP EXIT
;        ELSE
;           RECTYPE_NULL EXIT
;        THEN
;    THEN
;
;    ?IHXCRC IF
;        PAD CELL+ CELL+        ( src )
;        PAD @                  ( src dest )
;        PAD CELL+ @ RECTYPE_IHEX         ( src dest n -1 )
;    ELSE
;        RECTYPE_NULL
;    THEN  ;
    head(REC_IHEX,REC-IHEX,docolon)
        DW DROP
        DW lit,0,IHXCRC,STORE
        DW DUP,CFETCH,lit,58,NOTEQUAL,qbranch,RECIHEX1
        DW DROP,RECTYPE_NULL,EXIT

RECIHEX1:
        DW CHARPLUS
        DW IHXBYTE,OVER,PAD,CELLPLUS,STORE
        DW IHXWORD,SWOP,PAD,STORE
        DW PAD,CELLPLUS,CELLPLUS,SWOP
        DW IHXBYTE

        DW OVER,ZEROEQUAL,qbranch,RECIHEX2
        DW NIP,IHXRECSTORE,XIHXBYTE,DROP,NIP
        DW branch,RECIHEX3
RECIHEX2:
        DW DROP,NIP,NIP
        DW lit,1,EQUAL,qbranch,RECIHEX2a
        DW RECTYPE_NOOP,EXIT
RECIHEX2a:
        DW RECTYPE_NULL,EXIT

RECIHEX3:
        DW QIHXCRC,qbranch,RECIHEX4
        DW PAD,CELLPLUS,CELLPLUS
        DW PAD,FETCH
        DW PAD,CELLPLUS,FETCH,RECTYPE_IHEX
        DW EXIT
RECIHEX4:
        DW RECTYPE_NULL,EXIT

;: REC-USER ( addr len -- j*x RECTYPE_xxx   if ok, RECTYPE_NULL if not recognised )
;     REC-USERVEC @ ?DUP IF
;         EXECUTE
;     ELSE
;         2DROP RECTYPE_NULL
;     THEN ;
    head(REC_USER,REC-USER,docolon)
        DW REC_USERVEC,FETCH,QDUP,qbranch,RECUSER1
        DW EXECUTE
        DW EXIT
RECUSER1:
        DW TWODROP,RECTYPE_NULL
        DW EXIT

;Z SAVE-INPUT
;   SOURCE-ID @ 'SOURCE 2@  >IN @  4 N>R  ;
    head(SAVE_INPUT,SAVE-INPUT,docolon)
        DW SOURCE_ID,FETCH
        DW TICKSOURCE,TWOFETCH
        DW TOIN,FETCH

        DW lit,4

        DW DUP
NTOR1:  DW DUP
        DW qbranch,NTOR2
        DW ROT,RFROM,SWOP,TOR,TOR
        DW ONEMINUS
        DW branch,NTOR1
NTOR2:
        DW DROP,RFROM,SWOP,TOR,TOR
        DW EXIT

;Z RESTORE-INPUT
;   NR> DROP  >IN !  'SOURCE 2! SOURCE-ID !  ;
    head(RESTORE_INPUT,RESTORE-INPUT,docolon)
        DW RFROM,RFROM,SWOP,TOR,DUP
NR1:    DW DUP
        DW qbranch,NR2
        DW RFROM,RFROM,SWOP,TOR,ROT,ROT
        DW ONEMINUS
        DW branch,NR1
NR2:    DW DROP

        DW DROP
        DW TOIN,STORE
        DW TICKSOURCE,TWOSTORE
        DW SOURCE_ID,STORE
        DW EXIT
