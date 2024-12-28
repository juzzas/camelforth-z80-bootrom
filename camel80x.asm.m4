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

EXTERN asm_z80_delay_ms
EXTERN asm_z80_delay_tstate

SECTION code_16k


    ;Z BLK      -- a-addr     block number storage
    ;  20 USER BLK
        head(BLK,BLK,douser)
            dw 20

    ;Z DSK      -- a-addr     disk number storage
    ;  22 USER DSK
        head(DSK,DSK,douser)
            dw 22


    ;Z SCR          -- a-addr  last edited screen number
    ;  24 USER SCR
        head(SCR,SCR,douser)
            dw 24


    ;Z 'KEY      -- xt     if set, use XT as KEY destination
    ;  36 USER 'KEY
        head(TICKKEY,'KEY,docode)
            jp KEYVEC

    ;Z 'KEY?      -- xt     if set, use XT as KEY? destination
    ;  38 USER 'KEY?
        head(TICKKEYQ,'KEY?,docode)
            jp KEYQVEC

    ;Z 'EMIT      -- xt     if set, use XT as EMIT destination
    ;  40 USER 'EMIT
        head(TICKEMIT,'EMIT,docode)
            jp EMITVEC

    ;Z 'REFILL      -- xt    if set, use XT as REFILL source
    ;  42 USER 'REFILL
        head(TICKREFILL,'REFILL,docode)
            jp REFILLVEC

    ;Z LINK        -- a-addr  link to following round-robin task
    ;  44 USER LINK
        head(LINK,LINK,douser)
            dw 44

    ;Z STACKTOP      -- a-addr  address of stored stack top for task
    ;  2 USER STACKTOP
        head(STACKTOP,STACKTOP,douser)
            dw 2

    ;Z ENTRY      -- a-addr  address of xt for entry point of task
    ;  8 USER ENTRY
        head(ENTRY,ENTRY,douser)
            dw 8

EXTERN intvec_ptr
;Z 'INT      -- a-addr   pointer to address holding interrupt vector
;  intvec_ptr CONSTANT INTVEC
    head(INTVEC,'INT,docon)
        dw intvec_ptr

EXTERN nmivec_ptr
;Z 'NMI      -- a-addr   pointer to address holding interrupt vector
;  intvec_ptr CONSTANT NMIVEC
    head(NMIVEC,'NMI,docon)
        dw nmivec_ptr

EXTERN pausevec_ptr
;Z 'PAUSE      -- a-addr   pointer to address holding pause vector
;  pausevec_ptr CONSTANT 'INTVEC
    head(TICKPAUSE,'PAUSE,docode)
        jp PAUSEVEC


;: FORTH-WORDLIST ( -- wid )
; Return wid, the identifier of the word list that includes all standard
; words provided by the implementation. This word list is initially the
; compilation word list and is part of the initial search order.
        head(FORTH_WORDLIST,FORTH-WORDLIST,docolon)
            dw LATEST,EXIT

;: EDITOR-WORDLIST ( -- wid )
        head(EDITOR_WORDLIST,EDITOR-WORDLIST,docon)
            dw editor_wordlist_head

;: VOCAB-WORDLIST ( -- wid )
        head(VOCAB_WORDLIST,VOCAB-WORDLIST,docon)
            dw vocab_wordlist_head

;: UTILS-WORDLIST ( -- wid )
        head(UTILS_WORDLIST,UTILS-WORDLIST,docon)
            dw utils_wordlist_head

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

;C   PAUSE ( -- )   call idle routine
    head(PAUSE,PAUSE,docolon)
        DW PAUSEVEC,FETCH,EXECUTE
        DW EXIT

;C TRUE
    head(TRUE,TRUE,docode)
        push bc
        jp tostrue

;C FALSE
    head(FALSE,FALSE,docode)
        push bc
        jp tosfalse

; TEMPBUFF reserves chucks of memory in a stack-like algorithm
;Z   /TBUFFER  ( -- )
SLASHTEMPBUFF:
        call docolon
        DW lit,STACK_TEMPBUFF,lit,STACK_TEMPBUFF_SIZE,SLASHSTACK
        DW EXIT

;Z   TBUFFER-BASE  ( -- addr )  addr of base of tempbuffers
TEMPBUFF_BASE:
        call docolon
        DW BLKFIRST
        DW lit,STACK_TEMPBUFF,STACKEMPTYQ,INVERT,qbranch,TEMPBUFF_BASE1
        DW lit,STACK_TEMPBUFF,STACKFETCH,MINUS
TEMPBUFF_BASE1:
        DW EXIT

;Z   TBUFFER-ALLOC  ( u -- addr ) allocates u byte buffer
    head(TEMPBUFF_ALLOC,TBUFFER-ALLOC,docolon)
        DW lit,STACK_TEMPBUFF,STACKEMPTYQ,INVERT,qbranch,TEMPBUFF_ALLOC1
        DW lit,STACK_TEMPBUFF,STACKFETCH,PLUS
TEMPBUFF_ALLOC1:
        DW lit,STACK_TEMPBUFF,TOSTACK
        DW TEMPBUFF_BASE
        dw EXIT

;Z   TBUFFER-FREE  ( --  ) frees last 256byte buffer
    head(TEMPBUFF_FREE,TBUFFER-FREE,docolon)
        DW lit,STACK_TEMPBUFF,STACKEMPTYQ,INVERT,qbranch,TEMPBUFF_FREE1
        DW lit,STACK_TEMPBUFF,STACKDROP
TEMPBUFF_FREE1:
        dw EXIT

SECTION data
defc STACK_TEMPBUFF_SIZE = 36
STACK_TEMPBUFF:
        ds STACK_TEMPBUFF_SIZE    ; 16 cells + stack top pointer

SECTION code_16k


;C   UNUSED  ( -- u )  return unused space in data area
    head(UNUSED,UNUSED,docolon)
        dw TEMPBUFF_BASE,HERE,MINUS
        dw EXIT



; RC2014 EXTENSION output ====================

;C ?DO       -- if-adrs -1 adrs   L: -- 0
;   -1           flag to distiguish this from DO
;   ['] 2DUP ,XT  ['] <> ,XT  POSTPONE IF
;                SWAP     swap IF target and flag
;   ['] xdo ,XT   HERE     target for bwd branch
;   0 >L ; IMMEDIATE           marker for LEAVEs
    immed(QDO,?DO,docolon)
        DW TRUE
        DW lit,TWODUP,COMMAXT
        DW lit,NOTEQUAL,COMMAXT
        DW IF,SWOP
        DW DO_COMMON,EXIT


dnl ;Z VT-ESC  ( --  emit escape character )
dnl ;    27 EMIT [CHAR] [ EMIT ;
dnl     head(VT_ESC,VT-ESC,docolon)
dnl         dw lit,0x1b,EMIT
dnl         dw lit,'[',EMIT
dnl         dw EXIT

;C 2LITERAL  x1 x2 --    append double numeric literal
;   STATE @ IF ['] DLIT ,XT , , THEN ; IMMEDIATE
; This tests STATE so that it can also be used
; interpretively.  (ANSI doesn't require this.)
    immed(TWOLITERAL,2LITERAL,docolon)
        DW STATE,FETCH,qbranch,DLITER1
        DW lit,dlit,COMMAXT,COMMA,COMMA
DLITER1: DW EXIT



;C 0<>     x1 -- flag    test not eq to 0
    head(ZERONOTEQUAL,0<>,docolon)
        DW ZEROEQUAL,INVERT,EXIT

;C 0>     n -- flag     test greater than 0
    head(ZEROGREATER,0>,docolon)
        DW lit,0x7fff,AND,EXIT


;C D-
;   dnegate d+ ;
    head(DMINUS,D-,docolon)
        dw DNEGATE,DPLUS,EXIT

;: D=
;   rot = -rot = and ;
    head(DEQUAL,D=,docolon)
        dw ROT,EQUAL,ROT,ROT,EQUAL,AND,EXIT

;: d< rot 2dup >
;    if = nip nip if 0 exit then -1 exit then
;    2drop u< ;
    head(DLESS,D<,docolon)
        DW ROT,TWODUP,GREATER
        DW qbranch,DLESS2

        DW EQUAL,NIP,NIP
        DW qbranch,DLESS1

        DW FALSE,EXIT

DLESS1:
        DW TRUE,EXIT


DLESS2:
        DW TWODROP,ULESS
        DW EXIT


;: D0=   OR 0=  ;
    head(DZEROEQUAL,D0=,docolon)
        DW OR,ZEROEQUAL,EXIT

;: d2/     ( d -- d     shift double number right, perserving most-significant bit )
    head(DTWOSLASH,D2/,docode)
        pop hl
        sra b
        rr c
        rr h
        rr l
        push hl
        next

;: d2*     ( d -- d     shift double left )
    head(DTWOSTAR,D2*,docode)
       pop hl 
       sla l
       rl h
       rl c
       rl b
       push hl
       next


;: dmax 2over 2over d< if 2swap then 2drop ; ( d1 d2 -- d )
    head(DMAX,DMAX,docolon)
        DW TWOOVER,TWOOVER,DLESS
        DW qbranch,DMAX1
        DW TWOSWAP
DMAX1:
        DW TWODROP,EXIT

;: dmin 2over 2over d> if 2swap then 2drop ; ( d1 d2 -- d )
    head(DMIN,DMIN,docolon)
        DW TWOOVER,TWOOVER,TWOSWAP,DLESS
        DW qbranch,DMIN1
        DW TWOSWAP
DMIN1:
        DW TWODROP,EXIT




; RC2014 EXTENDED STRINGS =======================

; RC2014 EXTENDED STRUCTURES ====================

;C CELL-    a-addr1 -- a-addr2    subtract cell size
;   2 - ;
    head(CELLMINUS,CELL-,docode)
        dec bc
        dec bc
        next

;C CODE:   --      create an empty code definition
;   CURRENT @ WID>NFA , 0 C,         link & `immed' field
;   HERE CURRENT @ WID>NFA!           new "latest" link
;   BL WORD C@ 1+ ALLOT         name field
    head(CODECOLON,CODE:,docolon)
        DW CURRENT,FETCH,WIDTONFA,COMMA,lit,0,CCOMMA
        DW HERE,CURRENT,FETCH,WIDTONFASTORE
        DW BL,WORD,CFETCH,ONEPLUS,ALLOT
        DW EXIT


;C ;CODE   --      end a code definition
    head(SEMICODE,;CODE,docolon)
        DW EXIT

;C NEXT,   --      compile forth NEXT word to code definition
    head(NEXTCOMMA,``NEXT,'',docolon)
        DW lit,nextcomma_block,HERE
        DW lit,nextcomma_block_len,MOVE
        DW lit,nextcomma_block_len,ALLOT
        DW EXIT


dnl ; http://www.forth.org/svfig/Len/softstak.htm

dnl ;     lifo+0 -> ptr to top of stack - 2
dnl ;     lifo+2 -> S0: bottom of stack
dnl ;     lifo+2+n -> bottom of stack

dnl ; note: on empty stack, S0 and SP point to beyond end of area.
dnl ;       SP decremented before storing.

; initialise a stack at addr with n bytes
; : /STACK   ( addr n -- )
;      OVER +         ( addr S0 )
;      SWAP 2DUP CELL+     ( SP sp-addr S0 s0-addr )
;      !  !   ;
    head_utils(SLASHSTACK,``/STACK'',docolon)
        DW TWODUP,lit,0,FILL
        DW OVER,PLUS
        DW SWOP,TWODUP,CELLPLUS
        DW STORE,STORE
        DW EXIT

dnl ; During compilation, create a STACK (software stack) with n cells.
dnl ; On execution, return the address of the stack.
dnl ; : STACK ( n -- ) ( -- adr)
dnl ;      CREATE              ( n )
dnl ;      CELLS CELL+ CELL+   ( bytes )
dnl ;      HERE OVER ALLOT      ( bytes addr )
dnl ;      SWAP /STACK
dnl ;   DOES>  ;
    head_utils(STACK,STACK:,docolon)
        DW CREATE
        DW CELLS,CELLPLUS,CELLPLUS
        DW HERE,OVER,ALLOT
        DW SWOP,SLASHSTACK
        DW XDOES
        call dodoes
        dw EXIT

; Push number onto STACK
; : >S ( n lifo -- )
;      SWAP OVER @ ( lifo n tos )
;      CELL- !     ( lifo )
;      CELL NEGATE SWAP +! ;
    head_utils(TOSTACK,>S,docolon)
        DW SWOP,OVER,FETCH
        DW CELLMINUS,STORE
        DW CELL,NEGATE,SWOP,PLUSSTORE
        DW EXIT

; Pop number from STACK
; : S> ( lifo -- x )
;      DUP @ @        ( lifo x )
;      SWAP           ( x lifo  )
;      CELL SWAP +!  ;  ( x )
    head_utils(STACKFROM,S>,docolon)
        DW DUP,FETCH,FETCH
        DW SWOP
        DW CELL,SWOP,PLUSSTORE
        DW EXIT

; Fetch the value at the top of the STACK
; : S@ ( lifo -- x )
;      @  @ ;
    head_utils(STACKFETCH,S@,docolon)
        DW FETCH,FETCH
        DW EXIT

; Replace the value at the top of the STACK
; : S! ( x lifo -- )
;      @ ! ;
    head_utils(STACKSTORE,S!,docolon)
        DW FETCH,STORE
        DW EXIT

; Drop the value at the top of the STACK
; : SDROP ( lifo -- )
;      S> DROP ;
    head_utils(STACKDROP,SDROP,docolon)
        DW STACKFROM,DROP
        DW EXIT

; Duplicate the value at the top of the STACK
; : SDUP ( lifo -- )
;      DUP S@ SWAP >S ;
    head_utils(STACKDUP,SDUP,docolon)
        DW DUP,STACKFETCH,SWOP,TOSTACK
        DW EXIT

; PICK for stack
; : SPICK ( n lifo -- x )
;     @ SWAP CELLS + @ ;
    head_utils(SPICK,SPICK,docolon)
        DW FETCH,SWOP,CELLS,PLUS,FETCH
        DW EXIT

; : STACK-DEPTH ( lifo -- n )
;      STACK.BOUNDS - CELL /  ;
    head_utils(STACKDEPTH,SDEPTH,docolon)
        DW STACKBOUNDS,MINUS
        DW TWOSLASH      ;  optimize "DW CELL,SLASH" for 16bit
        DW EXIT

; : STACK-EMPTY? ( lifo -- flag )
;      STACK.BOUNDS = ;
    head_utils(STACKEMPTYQ,SEMPTY?,docolon)
        DW STACKBOUNDS,EQUAL
        DW EXIT

; Create parameters for a ?DO loop that will scan every item currently in STACK. The intended use is:
;      ( lifo )   STACK-BOUNDS ?DO -- CELL +LOOP
; : STACK-BOUNDS ( lifo -- addr1 addr2 )
;     DUP CELL+ @ SWAP @ ;
    head_utils(STACKBOUNDS,STACK-BOUNDS,docolon)
        DW DUP,CELLPLUS,FETCH,SWOP,FETCH
        DW EXIT

; Set the stack from the data stack
; : STACK-SET     ( rec-n .. rec-1 n lifo )
;    OVER 0< IF -4 THROW THEN
;    OVER IF
;      DUP >R CELL+ @      ( item-n .. item-1 n S0 ; r: stk )
;      OVER CELLS -        ( item-n .. item-1 n sp ; r: stk )
;      DUP >R              ( item-n .. item-1 n sp ; r: stk sp )
;      SWAP 0 DO           ( item-n .. item-1 sp'  ; r: stk sp )
;        2DUP ! CELL+  NIP
;      LOOP               ( sp''  ;  r: stk sp )
;      DROP R> R> !
;    ELSE
;      NIP DUP CELL+ @ SWAP !   \ clear stack
;    THEN    ;
    head_utils(STACKSET,STACK-SET,docolon)
        DW OVER,ZEROLESS,qbranch,STACKSET0
        DW lit,-4,THROW
STACKSET0:
        DW OVER,qbranch,STACKSET2
        DW DUP,TOR,CELLPLUS,FETCH
        DW OVER,CELLS,MINUS
        DW DUP,TOR
        DW SWOP,lit,0,xdo
STACKSET1:
        DW TWODUP,STORE,CELLPLUS,NIP
        DW xloop,STACKSET1
        DW DROP,RFROM,RFROM,STORE
        DW EXIT
STACKSET2:
        DW NIP
        DW DUP,CELLPLUS,FETCH,SWOP,STORE
        DW EXIT

; Get the STACK onto the data stack
; : STACK-GET  ( lifo -- rec-n .. rec-1 n )
;     DUP STACK.DEPTH    ( lifo n )
;     DUP IF             ( lifo n )
;         >R             ( lifo ; n )
;         CELL+ @ CELL- R@   ( S0 n ; n )
;         0 DO           ( S0 ; n )
;             DUP I CELLS - @  ( S0 rec-i )
;             SWAP
;         LOOP           ( rec-i S0 )
;         DROP R>        ( rec-i n )
;     ELSE               ( lifo n )
;         NIP            ( n )
;     THEN               ( )
;     ;
    head_utils(STACKGET,STACK-GET,docolon)
        DW DUP,STACKDEPTH
        DW DUP,qbranch,STACKGET2
        DW TOR,CELLPLUS,FETCH,CELLMINUS,RFETCH
        DW lit,0,xdo
STACKGET1:
        DW DUP,II,CELLS,MINUS,FETCH
        DW SWOP
        DW xloop,STACKGET1
        DW DROP,RFROM
        DW EXIT

STACKGET2:
        DW NIP
        DW EXIT

; : STACK-MAP  ( xt lifo -- )  ( execute xt for every item in stack )
; \ XT should not return anything on stack
;      STACK-BOUNDS ?DO     ( xt )
;          I @ SWAP DUP >R EXECUTE R>
;      CELL +LOOP
;      DROP ;
    head_utils(STACKMAP,STACK-MAP,docolon)
        DW STACKBOUNDS
        DW TWODUP,EQUAL,qbranch,STACKMAP0
        DW TWODROP,DROP,EXIT
STACKMAP0:
        DW xdo
STACKMAP1:
        DW II,FETCH,SWOP,DUP,TOR,EXECUTE,RFROM
        DW CELL,xplusloop,STACKMAP1
STACKMAP2:
        DW DROP
        DW EXIT

; : STACK-UNTIL  ( x*i xt lifo -- x*j flag )  ( execute xt for every item in stack, until xt returns true )
; \ XT should return flag on stack
;    STACK-BOUNDS ?DO          ( x*i xt )
;      I @ SWAP           ( x*i item xt )
;      DUP >R        ( x*i item xt ; xt )
;      EXECUTE             ( x*j f ; xt )
;      R>                    ( x*j f xt )
;      OVER IF DROP UNLOOP EXIT
;           ELSE NIP
;      THEN
;    CELL +LOOP                  ( xt )
;    DROP FALSE
;     ;
    head_utils(STACKUNTIL,STACK-UNTIL,docolon)
        DW STACKBOUNDS
        DW TWODUP,EQUAL,qbranch,STACKUNTIL0
        DW TWODROP,DROP,FALSE,EXIT
STACKUNTIL0:
        DW xdo
STACKUNTIL1:
        DW II,FETCH,SWOP
        DW DUP,TOR
        DW EXECUTE
        DW RFROM
        DW OVER,qbranch,STACKUNTIL2
        DW DROP,UNLOOP,EXIT
STACKUNTIL2:
        DW NIP
        DW CELL,xplusloop,STACKUNTIL1
STACKUNTIL3:
        DW DROP,FALSE,EXIT


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
    head(NOOP,NOOP,docode)
        next


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

dnl ;Z (D.W)         ( d width --   width with leading 0's )
dnl ;    1- DUP 1 < IF DROP 1 THEN <# 0 DO # LOOP #S #> ;
dnl XDDOTW:
dnl         call docolon
dnl         dw ONEMINUS,DUP,lit,1,LESS,qbranch,XDDOTW1
dnl         dw DROP,lit,1
dnl XDDOTW1:
dnl         dw LESSNUM,lit,0,xdo
dnl XDDOTW2:
dnl         dw NUM,xloop,XDDOTW2
dnl         dw NUMS,NUMGREATER
dnl         dw EXIT
dnl 
dnl ;Z (.W)         ( n width --   width with leading 0's )
dnl ;    >R S>D R> (D.W) ;
dnl XDOTW:
dnl         call docolon
dnl         dw TOR,STOD,RFROM,XDDOTW
dnl         dw EXIT
dnl 
dnl ;Z .W           ( n width --   width with leading 0's )
dnl ;    (.W) TYPE SPACE ;
dnl     head(DOTW,.W,docolon)
dnl         dw XDOTW,TYPE,SPACE
dnl         dw EXIT
dnl 
dnl ;Z (U.W)        ( u width --   width with leading 0's )
dnl ;    0 SWAP (D.W) ;
dnl XUDOTW:
dnl         call docolon
dnl         dw lit,0,SWOP,XDDOTW
dnl         dw EXIT
dnl 
dnl ;Z U.W         ( u width --   width with leading 0's )
dnl ;    (U.W) TYPE SPACE ;
dnl     head(UDOTW,U.W,docolon)
dnl         dw XUDOTW,TYPE,SPACE
dnl         dw EXIT
dnl 
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

        head(WORDLISTS,WORDLISTS,docon)
            DW STACK_WORDLISTS

SECTION data

defc STACK_WORDLISTS_SIZE = 36 ; 16 cells + stack top pointer
STACK_WORDLISTS:
        ds STACK_WORDLISTS_SIZE

vocab_wordlist_head:
        ds 2

editor_wordlist_head:
        ds 2

utils_wordlist_head:
        ds 2

SECTION code_16k

;: WORDLIST ( -- wid )
; Create a new empty word list, returning its word list identifier wid.
; The new word list may be returned from a pool of preallocated word lists
; or may be dynamically allocated in data space. A system shall allow the
; creation of at least 8 new word lists in addition to any provided as part
; of the system.
;   HERE  0 , ;
        head(WORDLIST,WORDLIST,docolon)
            dw HERE,lit,0,COMMA
            dw EXIT

;: SEARCH-WORDLIST ( c-addr u wid -- 0 | xt 1 | xt -1 )
; Find the definition identified by the string c-addr u in the word list
; identified by wid. If the definition is not found, return zero. If the
; definition is found, return its execution token xt and one (1) if the
; definition is immediate, minus-one (-1) otherwise.

;: GET-ORDER  ( -- wid1 .. widn n )
        head(GET_ORDER,GET-ORDER,docolon)
            dw WORDLISTS,STACKGET
            dw EXIT

;: SET-ORDER  ( wid1 .. widn n -- )
        head(SET_ORDER,SET-ORDER,docolon)
            dw WORDLISTS,STACKSET
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
            dw WORDLISTS,TOSTACK
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
        DW TRUE
FINDIN3:
        DW DUP,qbranch,FINDIN4
        DW DROP,NFATOLFA,FETCH,DUP
FINDIN4:
        DW ZEROEQUAL,qbranch,FINDIN1
FINDIN5:
        DW EXIT

;C FIND-NAME   c-addr len      -- 0   if not found
;C                                nt  if found
;    ' FIND-NAME-IN WORDLISTS STACK.UNTIL
;                         ( c-addr len 0       if not found )
;                         ( c-addr len nfa     if found )
;    NIP NIP ;
    head(FIND_NAME,FIND-NAME,docolon)
        DW lit,FIND_NAME_IN,WORDLISTS,STACKUNTIL
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
        dw WORDLISTS,STACKSTORE
        dw EXIT

;CREATE EDITOR  EDITOR-WORDLIST , DO-VOCABULARY
    head_vocab(EDITOR,EDITOR,docolon)
        dw EDITOR_WORDLIST
        dw WORDLISTS,STACKSTORE
        dw EXIT

;CREATE UTILS  UTILS-WORDLIST , DO-VOCABULARY
    head_vocab(UTILS,UTILS,docolon)
        dw UTILS_WORDLIST
        dw WORDLISTS,STACKSTORE
        dw EXIT


;: VOCABULARY  ( name -- )  WORDLIST CREATE ,
;   VOC-LINK @ WID>NFA , 0 C,         link & `immed' field
;   HERE VOC-LINK @ WID>NFA!           new "latest" link
;   BL WORD C@ 1+ ALLOT         name field
;   docreate ,CF                code field
;   WORDLIST ,
;
;    DOES>  @ WORDLISTS TOSTACK
;  ;
    head(VOCABULARY,VOCABULARY,docolon)
        dw VOCAB_WORDLIST,WIDTONFA,COMMA,lit,0,CCOMMA
        dw HERE,VOCAB_WORDLIST,WIDTONFASTORE
        dw BL,WORD,CFETCH,ONEPLUS,ALLOT
        dw lit,docreate,COMMACF
        dw WORDLIST,COMMA

        dw XVOCDOES
        call dodoes
        DW WORDLISTS,STACKSTORE
        dw EXIT

; patch latest entry on  with code from VOCDOES
XVOCDOES:
        call docolon
        DW RFROM,VOCAB_WORDLIST,WIDTONFA,NFATOCFA,STORECF
        DW EXIT

;: ALSO  ( -- )  WORDLISTS STACK-DUP ;
    head(ALSO,ALSO,docolon)
        dw WORDLISTS,STACKDUP
        dw EXIT

;: PREVIOUS  ( --  )  WORDLISTS STACK> DROP ;
    head(PREVIOUS,PREVIOUS,docolon)
        dw WORDLISTS,STACKFROM,DROP
        dw EXIT

;: DEFINITIONS  ( -- )  WORDLISTS STACK@ SET-CURRENT ;
    head(DEFINITIONS,DEFINITIONS,docolon)
        dw WORDLISTS,STACKFETCH,SET_CURRENT
        dw EXIT

;: ONLY ( -- )  FORTH-WORDLIST 1 SET-ORDER ;
    head(ONLY,ONLY,docolon)
        dw VOCAB_WORDLIST,FORTH_WORDLIST,lit,2,SET_ORDER
        dw EXIT

;: VOCS  ( -- )      list all vocabularies in dict
;   VOCLINK @ (WORDS) ;
    head(VOCS,VOCS,docolon)
        DW VOCAB_WORDLIST,XWORDS,EXIT

;Z WORDS_16K ( -- )      list all words in search order
;   ['] (WORDS) WORDLISTS STACK.MAP ;
WORDS_16K:
        call docolon
        DW lit,XWORDS,WORDLISTS,STACKMAP
        DW EXIT

;Z VLIST  ( -- )      list all words in current context
;   CONTEXT (WORDS) ;
    head(VLIST,VLIST,docolon)
        DW CONTEXT,XWORDS
        DW EXIT

; ================================================

; : PICK  ( xu...x1 x0 u -- xu...x1 x0 xu )
; Remove u. Copy the xu to the top of the stack. An ambiguous
; condition exists if there are less than u+2 items on the stack
; before PICK is executed.
;   0 PICK is equivalent to DUP and
;   1 PICK is equivalent to OVER.
;   1+ CELLS SP@ + @ ;
    head(PICK,PICK,docode)
        ld hl,bc
        add hl,hl
        add hl,sp
        ld c,(hl)
        inc hl
        ld b,(hl)
        next

; : ROLL   ( xu xu-1 ... x0 u -- xu-1 ... x0 xu )
;  Remove u. Rotate u+1 items on the top of the stack. An
;  ambiguous condition exists if there are less than u+2
;  items on the stack before ROLL is executed.
;   2 ROLL is equivalent to ROT
;   1 ROLL is equivalent to SWAP and
;   0 ROLL is a null operation.
    head(ROLL,ROLL,docode)
        ld a,b
        or c
        jr nz,roll_do
        pop bc
        jr roll_end
roll_do:
        inc bc   ; correct for stack offset
        sla c    ; convert to cells
        rl b
	push bc   ; push count

        ld hl,bc   ; do pick
        add hl,sp
        push hl    ; but, push as shift destination
        ld c,(hl)
        inc hl
        ld b,(hl)

        exx
        pop de    ; shift stack
        pop bc
        ld hl,de
        dec hl
        dec hl
        lddr
        exx

        pop hl  ; do nip and discard
roll_end:
        next


; : CASE ( -- 0 ) 0 >L ; IMMEDIATE
    immed(CASE,CASE,docolon)
        DW lit,0,TOL,EXIT

; : OF ( dest? -- orig )
;     POSTPONE OVER POSTPONE =
;     POSTPONE IF POSTPONE DROP ; IMMEDIATE
    immed(OF,OF,docolon)
        DW lit,OVER,COMMAXT
        DW lit,EQUAL,COMMAXT
        DW IF
        DW lit,DROP,COMMAXT
        DW EXIT

; : ?OF ( dest? -- orig )
;     POSTPONE DUP POSTPONE IF POSTPONE DROP ; IMMEDIATE
    immed(QOF,?OF,docolon)
        DW lit,DUP,COMMAXT
        DW IF
        DW lit,DROP,COMMAXT
        DW EXIT

; : ENDOF ( orig -- ) POSTPONE ELSE ; IMMEDIATE
;   ['] branch ,BRANCH   HERE DUP ,DEST  >L
;   POSTPONE THEN
;   ; IMMEDIATE      unconditional forward branch
    immed(ENDOF,ENDOF,docolon)
        DW lit,branch,COMMABRANCH
        DW HERE,DUP,COMMADEST,TOL
        DW THEN,EXIT

; : ENDCASE ( dest? -- )
;     POSTPONE DROP
;     BEGIN L> ?DUP WHILE POSTPONE THEN REPEAT
;                                 resolve LEAVEs
;     ; IMMEDIATE
    immed(ENDCASE,ENDCASE,docolon)
        DW lit,DROP,COMMAXT
ENDCASE1:
        DW LFROM,QDUP,qbranch,ENDCASE2
        DW THEN,branch,ENDCASE1

ENDCASE2:
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
;    BLKCTX_PTR BLKCTX# 0 DO   ( ctx[i] )
;       0xffff OVER BLKCTX>BLOCK !  ( ctx[i] )
;       0xffff OVER BLKCTX>DISK !  ( ctx[i] )
;       0x0000 OVER BLKCTX>BUFFER !  ( ctx[i] )
;       0x0000 OVER BLKCTX>FLAGS !  ( ctx[i] )
;       BLOCKCTX_SIZE +             ( ctx[i+1 ]
;    LOOP
;    DROP  0 BLKCTX_IDX !  ;
SLASHBLKCTX:
        call docolon
        dw lit,BLKCTX_PTR,BLKCTXNUM,lit,0,xdo
SLASHBLKCTX1:
        dw lit,0xffff,OVER,BLKCTXTOBLOCK,STORE
        dw lit,0xffff,OVER,BLKCTXTODISK,STORE
        dw lit,0x0000,OVER,BLKCTXTOBUFFER,STORE
        dw lit,0x0000,OVER,BLKCTXTOFLAGS,STORE
        dw BLKCTXSIZE,PLUS
        dw xloop,SLASHBLKCTX1
        dw DROP
        dw lit,0,lit,BLKCTX_IDX,STORE
        dw EXIT

;Z BLKCTX>DISK  ( ctx -- a-addr' )  get address of DISK number
;    ;
BLKCTXTODISK:
        jp blkctx_next

;Z BLKCTX>BLOCK  ( ctx -- a-addr' )  get address of BLOCK number
;    ;
BLKCTXTOBLOCK:
        jp blkctx_plus_2

;Z BLKCTX>BUFFER  ( ctx -- a-addr' )  get address of BUFFER
;    ;
BLKCTXTOBUFFER:
        jp blkctx_plus_4

;Z BLKCTX>FLAGS  ( ctx -- a-addr' )  get address of FLAGS
;    ;
BLKCTXTOFLAGS:
        inc bc
        inc bc
blkctx_plus_4:
        inc bc
        inc bc
blkctx_plus_2:
        inc bc
        inc bc
blkctx_next:
        next

;Z BLKCTX%  (  -- u )  size of stucture
BLKCTXSIZE:
        push bc
        ld bc,BLOCKCTX_SIZE
        jp blkctx_next

;Z BLKCTX#  ( -- u )  number of buffer structures
BLKCTXNUM:
        push bc
        ld bc,BLOCKCTX_NUM
        jp blkctx_next


;Z BLKFIRST      -- a-adrs      address of first block buffer
;   RAMTOP @ 0xFC00 AND 0x1000 - ;
BLKFIRST:
        call docolon
        dw RAMTOP,FETCH,lit,0xFC00,AND,lit,0x1000,MINUS
        dw EXIT

;Z BLKCTX-NEXT  ( -- ctx )  increment buffer structure
;   BLKCTX_PTR   BLKCTX_IDX @
;   BLKCTX% * +  ( new-ctx )
;   BLKCTX_IDX @  B/BLK *  BLKFIRST PLUS  ( new-ctx buffer )
;   OVER BLKCTX>BUFFER !  ( new-ctx )
;   BLKCTX_IDX @ 1+ BLKCTXNUM MOD  BLKCTX_IDX !
;   ;
BLKCTX_NEXT:
        call docolon
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
;       D=  IF            ( blk dsk ; r: ctx[i] )
;           2DROP R> UNLOOP EXIT
;       THEN
;       R> BLKCTX% + ( blk dsk ctx[i+1] )
;    LOOP
;    2DROP DROP 0   ;
BLKCTX_FIND:
        call docolon
        dw lit,BLKCTX_PTR,BLKCTXNUM,lit,0,xdo
BLKCTXF1:
        dw TOR
        dw TWODUP
        dw RFETCH,BLKCTXTOBLOCK,FETCH
        dw RFETCH,BLKCTXTODISK,FETCH
        dw DEQUAL,qbranch,BLKCTXF2
        dw TWODROP,RFROM,UNLOOP
        dw EXIT
BLKCTXF2:
        dw RFROM,BLKCTXSIZE,PLUS
        dw xloop,BLKCTXF1
        dw TWODROP,DROP,FALSE
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
BLKCTX_GET:
        call docolon
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
BLKCTX_MAP:
        call docolon
        dw lit,BLKCTX_PTR,BLKCTXNUM,lit,0,xdo
BLKCTXMAP1:
        dw TOR
        dw DUP
        dw RFETCH,SWOP,EXECUTE
        dw RFROM,BLKCTXSIZE,PLUS
        dw xloop,BLKCTXMAP1
        dw TWODROP
        dw EXIT


SECTION data

BLKCTX_PTR:
        DEFS 32  ;  4 * 4 words

BLKCTX_IDX:
        DEFS 2

SECTION code_16k


; DRIVE implementation ==========================

; DRIVECTX structure
;   Each context struct is drive device "driver"
;    XT READ sector (1 cell)
;    XT WRITE sector BLOCK number (1 cell)
;    XT CAPACITY number of sectores (1 cell)

DEFC DRIVECTX_SIZE = 8


;Z DRIVE>READ  ( drive-id -- a-addr' )  get address of  READ xt
    head_utils(DRIVETOREAD,DRIVE>READ,docode)
        jp drvctx_next

;Z DRIVE>WRITE  ( drive-id -- a-addr' )  get address of WRITE xt
    head_utils(DRIVETOWRITE,DRIVE>WRITE,docode)
        jp drvctx_plus_2

;Z DRIVE>CAPACITY  ( drive-id -- a-addr' )  get address of CAPACITY xt
    head_utils(DRIVETOCAPACITY,DRIVE>CAPACITY,docode)
        inc bc
        inc bc
drvctx_plus_2:
        inc bc
        inc bc
drvctx_next:
        next

;Z DRIVECTX  (  -- u )  size of stucture
    head_utils(DRIVECTX,DRIVECTX,docode)
        push bc
        ld bc,DRIVECTX_SIZE
        jp drvctx_next



; DISK implementation ==========================

; DISKCTX structure
;   Each context struct is drive device "driver"
;    DRIVE-ID sector (1 cell)
;    LBA OFFSET  (2 cells)
;    LIMIT number of blocks (1 cell)
;    BLOCK XLATE   xt of Block translation routine (1 cell)

DEFC DISKCTX_SIZE = 10
DEFC DISKCTX_NUM = 8


;Z DISK>DRIVE  ( disk-id -- a-addr' )  get address of drive ID for disk
    head_utils(DISKTODRIVE,DISK>DRIVE,docode)
        jp diskctx_next
        next

;Z DISK>OFFSET  ( disk-id -- a-addr' )  get address of LBA OFFSET for disk
    head_utils(DISKTOOFFSET,DISK>OFFSET,docode)
        jp diskctx_plus_2

;Z DISK>LIMIT  ( disk-id -- a-addr' )  get address of LIMIT for disk
    head_utils(DISKTOLIMIT,DISK>LIMIT,docode)
        jp diskctx_plus_6

;Z DISK>TRANSLATOR  ( disk-id -- a-addr' )  get address of XT to translate virtual to phys b blockss
    head_utils(DISKTOTRANSLATOR,DISK>TRANSLATOR,docode)
        inc bc
        inc bc
diskctx_plus_6:
        inc bc
        inc bc
diskctx_plus_4:
        inc bc
        inc bc
diskctx_plus_2:
        inc bc
        inc bc
diskctx_next:
        next

;Z DISKCTX  (  -- u )  size of stucture
DISKCTX:
        push bc
        ld bc,DISKCTX_SIZE
        jp diskctx_next

;Z #DISK  (  -- u )  number of DISK entries
    head_utils(NUMDISK,``#DISK'',docode)
        push bc
        ld bc,DISKCTX_NUM
        jp diskctx_next

;Z DISK  ( n -- disk-id | 0 )  get disk id from disk number. 0 if out of range
    head_utils(DISK,DISK,docolon)
        dw DUP,lit,0,NUMDISK,WITHIN
        dw qbranch,DISK1
        dw DISKCTX,STAR,lit,DISKCTX_PTR,PLUS
        dw EXIT

DISK1:
        dw DROP,FALSE
        dw EXIT


        DEFC SLICE_SECTORS = 8192*2
;Z SLICE  ( n -- )     set LBA offset to current DSK (n * 8MB)
;  CURRDISKID >R SLICE_SECTORS UM*                ( lba-offset ; disk-id )
;  2DUP R@ DISK>DRIVE @ DRIVE>CAPACITY @ EXECUTE  ( lba-offset lba-offset capacity )
;  2OVER 2OVER  D<  IF                            ( lba-offset lba-offset capacity )
;  2SWAP SLICE_SECTORS M+  DMIN                   ( lba-offset lba-end )
;  2OVER  D-  D2/  D>S                            ( lba-offset limit )
;  R@ DISK>LIMIT !
;  R> DISK>OFFSET 2!
;  ELSE  -257 THROW THEN   ;
    head_utils(SLICE,SLICE,docolon)
        DW FLUSH
        DW CURRDISKID,TOR,lit,SLICE_SECTORS,UMSTAR
        DW TWODUP,RFETCH,DISKTODRIVE,FETCH,DRIVETOCAPACITY,FETCH,EXECUTE
        DW TWOOVER,TWOOVER,DLESS,qbranch,SLICE1
        DW TWOSWAP,lit,SLICE_SECTORS,MPLUS
        DW DMIN
        DW TWOOVER,DMINUS,DTWOSLASH,DROP
        DW RFETCH,DISKTOLIMIT,STORE
        DW RFROM,DISKTOOFFSET,TWOSTORE,EXIT
SLICE1:
        DW lit,-257,THROW
        DW EXIT


SECTION data

DISKCTX_PTR:
        DEFS DISKCTX_SIZE * DISKCTX_NUM


SECTION code_16k

; DISK/DRIVE helpers ==========================


SECTRDVEC:
        call docolon
        dw DSK,FETCH,DISK,DISKTODRIVE,FETCH,DRIVETOREAD
        dw EXIT

SECTWRVEC:
        call docolon
        dw DSK,FETCH,DISK,DISKTODRIVE,FETCH,DRIVETOWRITE
        dw EXIT

BLKLIMIT:
        call docolon
        dw DSK,FETCH,DISK,DISKTOLIMIT
        dw EXIT

CURRDISKID:
        call docolon
        DW DSK,FETCH,DISK
        DW DUP,ZEROEQUAL,qbranch,CURRDISK1
        DW lit,-256,THROW
CURRDISK1:
        DW EXIT

;Z BLK2LBA   ( dsk blk -- LBA-L LBA-H )
;  S>D D2*   ( dsk LBA-L LBA-H )
;  ROT DISK   ( LBA-L LBA-H  disk-id )
;  DISK>OFFSET 2@ D+ ;  ( LBA-L' LBA-H' )
BLK2LBA:
        call docolon
        dw STOD,DTWOSTAR
        dw ROT,DISK
        dw DISKTOOFFSET,TWOFETCH,DPLUS
        dw EXIT


EXTERN cflash_init
EXTERN cflash_identify
;Z /CFLASH   ( -- drive-id ) initialise the Compact Flash driver
;   clash_init CALL    ( f )
;   IF
;       ." CFLASH OK"
;       CF_DRIVE-ID
;   ELSE ." NO CFLASH" 0 THEN ;
    head_utils(SLASHCFLASH,/CFLASH,docolon)
        dw lit,cflash_init,CALL
        dw qbranch,SLASHCFLASH1
        dw XSQUOTE
        db 11,"CFLASH OK ("
        dw TYPE,CF_CAPACITY,DTWOSLASH,DDOT
        dw XSQUOTE
        db 7,"blocks)"
        dw TYPE,CR
        dw CF_DRIVE_ID
        dw EXIT
SLASHCFLASH1:
        dw XSQUOTE
        db 9,"NO CFLASH"
        dw TYPE
        dw FALSE
        dw EXIT

;Z CF-CAPACITY  ( d -- )   Fetch Compact Flash capacity (sectors)
    head_utils(CF_CAPACITY,CF_CAPACITY,docolon)
        dw lit,512,TEMPBUFF_ALLOC,DUP,TOR,lit,cflash_identify,CALL
;        dw RFETCH,lit,256,MEMDUMP
        dw RFETCH,lit,120,PLUS,FETCH      ; low word of max LBA
        dw RFROM,lit,122,PLUS,FETCH       ; high word of max LBA
        dw TEMPBUFF_FREE
        DW EXIT


SECTION data
CF_CAPACITY_DATA:
        DEFS 4

SECTION code_16k

EXTERN cflash_read_sector
;Z CF-SECTOR-READ  ( lba-l lba-h adrs -- )   Compact Flash read sector at LBA
; Reads the sector from the Compact Flash card into memory
; address found at 'adrs'. 'dsk' and 'blk' are the disk
; and block numbers respectively
;   clash_read_sector CALL ;
    head_utils(CF_SECTOR_READ,CF_SECTOR_READ,docolon)
        dw lit,cflash_read_sector,CALL
        dw branch,SECTOR_READ3

SECTOR_READ2:
        dw INVERT,XSQUOTE
        db 9,"NO DRIVER"
        dw QABORT,EXIT

SECTOR_READ3:
        dw INVERT,XSQUOTE
        db 10,"READ ERROR"
        dw QABORT
        dw EXIT

EXTERN cflash_write_sector
;Z CF-SECTOR-WRITE  ( lba-l lba-h adrs -- )   Compact Flash write sector at LBA
;   clash_write_sector CALL ;
    head_utils(CF_SECTOR_WRITE,CF_SECTOR_WRITE,docolon)
        dw lit,cflash_write_sector,CALL
        dw branch,SECTOR_WRITE3

SECTOR_WRITE2:
        dw INVERT,XSQUOTE
        db 9,"NO DRIVER"
        dw QABORT,EXIT

SECTOR_WRITE3:
        dw INVERT,XSQUOTE
        db 11,"WRITE ERROR"
        dw QABORT
        dw EXIT


    head_utils(CF_DRIVE_ID,CF_DRIVE-ID,docreate)
        dw CF_SECTOR_READ
        dw CF_SECTOR_WRITE
        dw CF_CAPACITY




;Z BLOCK-READ  ( dsk blk adrs -- )  Compact Flash read BLK and DSK
; Reads the block from the Compact Flash card into memory
; address found at 'adrs'. 'dsk' and 'blk' are the disk
; and block numbers respectively
;     >R BLK2LBA 2DUP R@   ( LBA-L LBA-H LBA-L LBA-H adrs ;  R: adrs )
;     CF_SECTOR_READ       ( LBA-L LBA-H ;  R: adrs )
;     1 M+                 ( LBA-L' LBA-H' ;  R: adrs )
;     R>  512 +            ( LBA-L' LBA-H' adrs' )
;     CF_SECTOR_READ       ( )
;     EXIT
BLOCK_READ:
        call docolon
        dw TOR,BLK2LBA,TWODUP,RFETCH    ; convert block to LBA
        dw SECTRDVEC,FETCH,EXECUTE
        dw lit,1,MPLUS
        dw RFROM,lit,512,PLUS
        dw SECTRDVEC,FETCH,EXECUTE
        dw EXIT

;Z BLOCK-WRITE  ( dsk blk adrs -- )  Compact Flash write BLK and DSK
; Writes the block to the Compact Flash card from memory
; address found at 'adrs'. 'dsk' and 'blk' are the disk
; and block numbers respectively
BLOCK_WRITE:
        call docolon
        dw TOR,BLK2LBA,TWODUP,RFETCH    ; convert block to LBA
        dw SECTWRVEC,FETCH,EXECUTE
        dw SWOP,ONEPLUS,SWOP
        dw RFROM,lit,512,PLUS
        dw SECTWRVEC,FETCH,EXECUTE
        dw EXIT

;Z BLOCK-READWRITE    ( ctx f -- )  read or write block
;                              f = 0 read, f = -1 write
;     >R >R
;     R@ BLKCTX>DISK @
;     R@ BLKCTX>BLOCK @
;     R@ BLKCTX>BUFFER @
;     0 R@ BLKCTX>FLAGS !
;     R> DROP  R>      ( dsk blk adrs f )
;     IF BLOCK-WRITE ELSE BLOCK-READ ;
BLOCK_READWRITE:
        call docolon
        dw TOR,TOR
        dw RFETCH,BLKCTXTODISK,FETCH
        dw RFETCH,BLKCTXTOBLOCK,FETCH
        dw RFETCH,BLKCTXTOBUFFER,FETCH
        dw lit,0,RFETCH,BLKCTXTOFLAGS,STORE
        dw RFROM,DROP,RFROM
        dw qbranch,BLOCK_READWRITE1
        dw BLOCK_WRITE,branch,BLOCK_READWRITE2
BLOCK_READWRITE1:
        dw BLOCK_READ
BLOCK_READWRITE2:
        dw EXIT

;Z (BUFFER)      n -- ctx    get buffer context
;     DUP BLKLIMIT @ U< IF
;     DUP BLK !
;     DSK @
;     BLKCTX-GET
;     ELSE  -35 THROW THEN ;
XBUFFER:
        call docolon
        dw DUP,BLKLIMIT,FETCH,ULESS,qbranch,XBUFFER1
        dw DUP,BLK,STORE
        dw DSK,FETCH
        dw BLKCTX_GET
        dw EXIT
XBUFFER1:
        dw lit,-35,THROW
        dw EXIT


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
XFLUSH:
        call docolon
        dw DUP,BLKCTXTOFLAGS,FETCH,TRUE,EQUAL,qbranch,FLUSH1
        dw DUP,TRUE,BLOCK_READWRITE
        dw BLKCTXTOFLAGS,lit,0,SWOP,STORE
        dw EXIT
FLUSH1:
        dw DROP,EXIT

;C EMPTY-BUFFERS          --  release blocks, don't save to disk
    head(EMPTY_BUFFERS,EMPTY-BUFFERS,docolon)
        dw SLASHBLKCTX
        dw EXIT

;C SAVE-BUFFERS           --  save updated blocks to disk
;     ' (FLUSH) BLKCTX-MAP  ;
    head(SAVE_BUFFERS,SAVE-BUFFERS,docolon)
        dw lit,XFLUSH,BLKCTX_MAP
        dw EXIT

;C FLUSH                   --    flush blocks to disk
;    SAVE-BUFFERS /BLKCTX  ;
    head(FLUSH,FLUSH,docolon)
        dw SAVE_BUFFERS, SLASHBLKCTX
        dw EXIT


SECTION data

load_index: DS 2
load_blknum: DS 2
load_buffer: DS 2

SECTION code_16k


; : load-refill  ( -- flag )
;     load-index @ 1023 > IF 
;         -1 load-blk# +!
;         0 load-index !
;         1 BLK +!   THEN
;
;     load-blk# @ 0= IF 0 EXIT THEN
;
;     BLK @ BLOCK    ( addr )
;     load-index @ +  ( index )
;         load_buffer @  C/L CMOVE
;
;     load_buffer @ C/L  'SOURCE 2!
;
;     0 >IN !
;     C/L load-index  +! TRUE    ;
LOAD_REFILL:
        call docolon
        dw lit,load_index,FETCH,lit,1023,GREATER,qbranch,LOAD_REFILL1
        dw lit,-1,lit,load_blknum,PLUSSTORE
        dw lit,0,lit,load_index,STORE
        dw lit,1,BLK,PLUSSTORE

LOAD_REFILL1:
        dw lit,load_blknum,FETCH,ZEROEQUAL,qbranch,LOAD_REFILL2
        dw FALSE,EXIT

LOAD_REFILL2:
        dw BLK,FETCH,BLOCK
        dw lit,load_index,FETCH,PLUS
        dw lit,load_buffer,FETCH,C_L,CMOVE

        dw lit,load_buffer,FETCH,C_L,TICKSOURCE,TWOSTORE

        dw lit,0,TOIN,STORE
        dw C_L,lit,load_index,PLUSSTORE,TRUE
        dw EXIT

; : (LOAD)  ( blk blk# -- )
;    OVER 0= IF -35 THROW THEN
;    SAVE-INPUT
;    ['] load-refill REFILLVEC !
;    load-buffer @ >R 
;    64  TEMPBUFF-ALLOC  load_buffer   !
;    load-blk# @ >R    load-blk# !
;    BLK !
;    load-index @ >R   0 load-index !
;    BEGIN
;      REFILL  IF
;        ( SOURCE TYPE CR )  INTERPRET
;      ELSE
;        R> load-index !
;        R> load-blk# !
;        R> load-buffer !
;        TEMPBUFF-FREE
;        RESTORE-INPUT
;        EXIT
;      THEN
;    AGAIN  ;
XLOAD:
        call docolon
        dw OVER,ZEROEQUAL,qbranch,LOAD0
        dw lit,-35,THROW
LOAD0:
        dw SAVE_INPUT
        dw lit,LOAD_REFILL,REFILLVEC,STORE
        dw lit,load_buffer,FETCH,TOR
        dw lit,64,TEMPBUFF_ALLOC,lit,load_buffer,STORE
        dw lit,load_blknum,FETCH,TOR,lit,load_blknum,STORE
        dw BLK,STORE
        dw lit,load_index,FETCH,TOR,lit,0,lit,load_index,STORE
XLOAD1:
        dw REFILL,qbranch,XLOAD2
        ; debug dw SOURCE,TYPE,CR
        dw INTERPRET
        dw branch,XLOAD1

XLOAD2:
        dw RFROM,lit,load_index,STORE
        dw RFROM,lit,load_blknum,STORE
        dw RFROM,lit,load_buffer,STORE
        dw TEMPBUFF_FREE
        dw RESTORE_INPUT
        dw EXIT


;C  LOAD ( blk -- )
;    1 (LOAD) ;
    head(LOAD,LOAD,docolon)
        dw lit,1,XLOAD
        dw EXIT



;C +LOAD                  n  --    load block BLK + n
;     BLK @ + LOAD  ;
    head(PLUSLOAD,+LOAD,docolon)
        dw BLK,FETCH,PLUS,lit,1,XLOAD
        dw EXIT

;C THRU            n1 n2  --    load blocks n1 to n2
;     OVER - 1+ (LOAD) ;
    head(THRU,THRU,docolon)
        dw OVER,MINUS,ONEPLUS,XLOAD
        dw EXIT

;C +THRU            n1 n2  --    load blocks BLK+n1 to BLK+n2
;     OVER - 1+ SWAP BLK @ + SWAP (LOAD) ;
    head(PLUSTHRU,+THRU,docolon)
        dw OVER,MINUS,ONEPLUS
        dw SWOP,BLK,FETCH,PLUS,SWOP,XLOAD
        dw EXIT

;C COPY            n1 n2  --    copy block n1 to n2
;   SWAP BLOCK  ( n2 blk1 )
;   SWAP BLOCK  ( blk1 blk2 )
;   B/BLK MOVE UPDATE ;
    head(COPY,COPY,docolon)
        dw SWOP,BLOCK,SWOP,BLOCK,B_BLK,MOVE,UPDATE
        dw EXIT

; BLKFILE implementation =====================================

;VARIABLE   blk-ptr
;VARIABLE   blkfile-dirty

SECTION data

blk_ptr: DS 2
blk_curr: DS 2
blk_offset: DS 2
blkfile_dirty: DS 1

SECTION code_16k

;: set-dirty  ( -- )
;   -1 blkfile-dirty !  ;
SET_DIRTY:
    xor a
    dec a
do_set_dirty:
    ld hl,blkfile_dirty
    ld (hl),a
    next

;: clear-dirty  ( -- )
;   0 blkfile-dirty !  ;
CLEAR_DIRTY:
    xor a
    jp do_set_dirty

;: is-dirty?  ( -- )
;   blkfile-dirty @  ;
IS_DIRTYQ:
    push bc
    ld hl,blkfile_dirty
    ld b,(hl)
    ld c,b
    next

;: current-block ( -- )
;   is-dirty? IF UPDATE clear-dirty THEN
;   blk-cur @ BLOCK blk-ptr ! ;
CURRENT_BLOCK:
    call docolon
    DW IS_DIRTYQ,qbranch,CURRBLK1
    DW UPDATE,CLEAR_DIRTY
CURRBLK1:
    DW lit,blk_curr,FETCH,BLOCK,lit,blk_ptr,STORE
    DW EXIT

;: inc-block  ( -- ) 
;   1 blk-cur +!   (  )
;   0 blk-offset ! (  )
;   current-block ;
INC_BLOCK:
    call docolon
    DW lit,1,lit,blk_curr,PLUSSTORE
    DW lit,0,lit,blk_offset,STORE
    DW CURRENT_BLOCK
    DW EXIT

;: inc-offset  ( -- )
;   1 blk-offset +! ( )
;   blk-offset @ 1023 > IF
;      inc-block
;   THEN   ;
INC_OFFSET:
    call docolon
    DW lit,1,lit,blk_offset,PLUSSTORE
    DW lit,blk_offset,FETCH,lit,1023,GREATER
    DW qbranch,INCOFFSET1
    DW INC_BLOCK

INCOFFSET1:
    DW EXIT

;: (write-char) ( c -- )
;   blk-ptr @  ( c blk-ptr )
;   blk-offset @ + c!   ( )
;   inc-offset ;
XWRITE_CHAR:
    call docolon
    DW lit,blk_ptr,FETCH
    DW lit,blk_offset,FETCH,PLUS,CSTORE
    DW INC_OFFSET
    DW EXIT

;: PUTCH ( c )
;   current-block
;   set-dirty
;   (write-char) ;
    head_utils(PUTCH,PUTCH,docolon)
        DW CURRENT_BLOCK
        DW SET_DIRTY
        DW XWRITE_CHAR
        DW EXIT

;: PUTCHARS ( c-addr u -- )
;   ?DUP IF
;     current-block  ( c-addr u )
;     set-dirty
;     0 DO   ( c-addr )
;       DUP I + C@  ( c-addr c )
;       (write-char) ( c-addr )
;     LOOP
;   THEN DROP ;
    head_utils(PUTCHARS,PUTCHARS,docolon)
        DW QDUP,qbranch,PUTCHARS2

        DW CURRENT_BLOCK,SET_DIRTY
        DW lit,0,xdo
PUTCHARS1:
        DW DUP,II,PLUS,CFETCH
        DW XWRITE_CHAR
        DW xloop,PUTCHARS1

PUTCHARS2:
        DW DROP
        DW EXIT

;: GETCH ( -- c )
;   blk-ptr @  ( blk-ptr )
;   blk-offset @ + c@   ( c )
;   inc-offset ;
    head_utils(GETCH,GETCH,docolon)
        DW lit,blk_ptr,FETCH
        DW lit,blk_offset,FETCH,PLUS,CFETCH
        DW INC_OFFSET
        DW EXIT

;: GETCHARS ( c-addr u -- u )
;   DUP >R  ?DUP  IF
;     current-block  ( c-addr u )
;     0 DO   ( c-addr )
;       (read-char)  ( c-addr c )
;       OVER I + C!  ( c-addr )
;     LOOP
;   THEN  DROP R> ;
    head_utils(GETCHARS,GETCHARS,docolon)
        DW DUP,TOR,QDUP,qbranch,GETCHARS2

        DW CURRENT_BLOCK
        DW lit,0,xdo
GETCHARS1:
        DW GETCH
        DW OVER,II,PLUS,CSTORE
        DW xloop,GETCHARS1

GETCHARS2:
        DW DROP,RFROM
        DW EXIT

;: BEGIN-BLKFILE ( blk offset -- )
;   blk-offset !
;   blk-cur !
;   clear-dirty  current-block ;
    head_utils(BEGIN_BLKFILE,``BEGIN-BLKFILE'',docolon)
        DW lit,blk_offset,STORE
        DW lit,blk_curr,STORE
        DW CLEAR_DIRTY
        DW CURRENT_BLOCK
        DW EXIT

;: open-blkfile ( blk -- )
;
;: END-BLKFILE ( -- blk' offset' )
;   is-dirty? IF UPDATE clear-dirty THEN
;   FLUSH ;
    head_utils(END_BLKFILE,``END-BLKFILE'',docolon)
        DW IS_DIRTYQ,qbranch,END_BLKFILE1
        DW UPDATE,CLEAR_DIRTY

END_BLKFILE1:
        DW lit,blk_curr,FETCH,lit,blk_offset,FETCH,EXIT



; RC2014 EXTENSION (SCREENS) ====================

;Z (BLOCK)                 -- a-addr  load block in SCR
;     SCR @ BLOCK ;
XBLOCK:
        call docolon
        dw SCR,FETCH,BLOCK
        dw EXIT

;Z (LINE)           line# -- c-addr   address of line in block
;     C/L * (BLOCK) + ;
    head_utils(XLINE,(LINE),docolon)
        dw C_L,STAR,XBLOCK,PLUS
        dw EXIT

;C TYPE$    c-addr +n --     type line of printable characters to term'l
;   ?DUP IF
;     OVER + SWAP DO I C@
;       DUP PRINTABLE? INVERT IF DROP [CHAR] . THEN EMIT
;     LOOP
;   ELSE DROP THEN ;
    head_utils(TYPESTRING,TYPE$,docolon)
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
    head_utils(LL,LL,docolon)
        dw XLINE,C_L,TYPESTRING,CR
        dw EXIT


;Z  (LIST)            --    runtime for list screen
;     L/B 0 DO I 2 .R SPACE I LL LOOP ;
    head_utils(XLIST,(LIST),docolon)
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
        dw II,lit,5,DOTR,SPACE
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
;              FORTH, EDITOR, VOCS, UTILS.
;              If zero, then the matching wordlist hasn't been
;              extended by the user.
;    0090   | copy of Wordlist order (34 bytes)

;: #SNAPSHOT
;    128 CONSTANT #SNAPSHOT
NUMSNAPSHOT:
        call docon
        dw 128+16+34

;: SNAPSHOT.USER   ( snapaddr -- addr     ptr to user area in snapshot )
SNAPSHOTDOTUSER:
        call docolon
        dw EXIT

;: SNAPSHOT.LINKS   ( snapaddr -- addr     ptr to wordlists ROM links in snapshot )
SNAPSHOTDOTLINKS:
        call docolon
        dw lit,0x80,PLUS,EXIT

;: SNAPSHOT.ORDER   ( snapaddr -- addr     ptr to wordlist order in snapshot )
SNAPSHOTDOTORDER:
        call docolon
        dw lit,0x90,PLUS,EXIT

;: >SNAPSHOT  ( snapaddr  -- )
;        DUP #SNAPSHOT ERASE
;        DUP SNAPSHOT.USER U0 SWAP 128 MOVE
;        DUP SNAPSHOT.ORDER SAVE-ORDER
;        DROP   ;
    head_utils(TOSNAPSHOT,>SNAPSHOT,docolon)
        dw DUP,NUMSNAPSHOT,ERASE
        dw DUP,SNAPSHOTDOTUSER,U0,SWOP,lit,128,MOVE
        dw DUP,SNAPSHOTDOTORDER,SAVE_ORDER
        dw DUP,SNAPSHOTDOTLINKS
        dw    VOCAB_WORDLIST,FETCH,OVER,STORE,CELLPLUS
        dw    FORTH_WORDLIST,FETCH,OVER,STORE,CELLPLUS
        dw    EDITOR_WORDLIST,FETCH,OVER,STORE,CELLPLUS
        dw    UTILS_WORDLIST,FETCH,OVER,STORE,CELLPLUS
        dw    DROP
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
    head_utils(SNAPSHOTFROM,SNAPSHOT>,docolon)
        dw DUP,SNAPSHOTDOTUSER,lit,SNAPSHOT_RST_START,PLUS
        dw U0,lit,SNAPSHOT_RST_START,PLUS,lit,SNAPSHOT_RST_LEN,MOVE
        dw DUP,SNAPSHOTDOTORDER,RESTORE_ORDER
        dw DUP,SNAPSHOTDOTUSER,lit,8,PLUS,FETCH,DP,STORE
        dw DUP,SNAPSHOTDOTLINKS
        dw    DUP,FETCH,VOCAB_WORDLIST,STORE,CELLPLUS
        dw    DUP,FETCH,FORTH_WORDLIST,STORE,CELLPLUS
        dw    DUP,FETCH,EDITOR_WORDLIST,STORE,CELLPLUS
        dw    DUP,FETCH,UTILS_WORDLIST,STORE,CELLPLUS
        dw    DROP
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

;: BSAVE   ( c-addr u blk -- )
;     0 (OPEN-BLKFILE)
;     WRITE-BLKFILE
;     (CLOSE-BLKFILE)   ;
    head(BSAVE,BSAVE,docolon)
        dw lit,0,BEGIN_BLKFILE
        dw PUTCHARS
        dw END_BLKFILE,TWODROP
        dw EXIT


;: BLOAD   ( c-addr u blk -- )
;     0 (OPEN-BLKFILE)
;     READ-BLKFILE,DROP
;     (CLOSE-BLKFILE)   ;
    head(BLOAD,BLOAD,docolon)
        dw lit,0,BEGIN_BLKFILE
        dw GETCHARS,DROP
        dw END_BLKFILE,TWODROP
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

XSAVEHDR:
        call docolon
        dw DUP,lit,BLK_HEADER_SIZE,ERASE
        dw lit,BLK_HEADER_SIZE,OVER,STORE
        dw CELLPLUS,TWODUP,STORE
        dw CELLPLUS,TOSNAPSHOT
        dw EXIT

;: SAVE   ( blk -- )
;    DUP WIPE
;    enddict   DP @ enddict -    ( blk c-addr u )
;    ROT BUFFER                  ( c-addr u buffer -- ; blk in BLK)
;    (SAVEHDR)                   ( c-addr u )
;
;    BLK @  BLK_HEADER_SIZE
;    BEGIN-BLKFILE
;    PUTCHARS
;    END-BLKFILE  2DROP
;    FLUSH  ;
    head(SAVE,SAVE,docolon)
        dw DUP,WIPE
        dw lit,enddict,DP,FETCH,lit,enddict,MINUS  ;  ( block c-addr u )
        dw ROT,BUFFER                              ;  ( c-addr u buffer )
        dw XSAVEHDR

        dw BLK,FETCH,lit,BLK_HEADER_SIZE
        dw BEGIN_BLKFILE
        dw PUTCHARS
        dw END_BLKFILE,TWODROP,FLUSH
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
;    BLK @  BLK_HEADER_SIZE
;    BEGIN-BLKFILE
;    GETCHARS,DROP
;    END-BLKFILE 2DROP    ;
    head(RESTORE,RESTORE,docolon)
        dw BLOCK
        dw DUP,FETCH               ; header size
        dw OVER,PLUS
        dw SWOP,CELLPLUS,DUP,FETCH
        dw SWOP,CELLPLUS
        dw SNAPSHOTFROM

        dw SWOP,lit,enddict,ROT

        dw BLK,FETCH,lit,BLK_HEADER_SIZE
        dw BEGIN_BLKFILE
        dw GETCHARS,DROP
        dw END_BLKFILE,TWODROP
        dw EXIT




; an alternative implementation of recognizers. It
; uses a separate stack module, that can be used
; to implement the search order words independently
;
; Based on the RECOGNIZER by Author: Matthias Trute
; License: Public Domain

; 4 STACK VALUE FORTH-RECOGNIZER

        head_utils(RECOGNIZERS,RECOGNIZERS,docon)
            DW STACK_RECOGNIZERS

SECTION data

defc STACK_RECOGNIZERS_SIZE = 36 ; 16 cells + stack top pointer
STACK_RECOGNIZERS:
        ds STACK_RECOGNIZERS_SIZE

SECTION code_16k

; define a recognizer with three actions. Suggesting RECTYPE-* names
;: RECTYPE: ( XT-INTERPRET XT-COMPILE XT-POSTPONE "<spaces>name" -- )
;  CREATE SWAP ROT , , ,
;    ;
    head_utils(RECTYPECOLON,RECTYPE:,docolon)
        DW CREATE,SWOP,ROT,COMMA,COMMA,COMMA
        DW EXIT

;: RECTYPE>POST ( RECTYPE-TOKEN -- XT-POSTPONE ) CELL+ CELL+ @ ;
    head_utils(RECTYPETOPOST,RECTYPE>POST,docolon)
        DW CELLPLUS,CELLPLUS,FETCH
        DW EXIT

;: RECTYPE>COMP ( RECTYPE-TOKEN -- XT-COMPILE  )       CELL+ @ ;
    head_utils(RECTYPETOCOMP,RECTYPE>COMP,docolon)
        DW CELLPLUS,FETCH
        DW EXIT

;: RECTYPE>INT  ( RECTYPE-TOKEN -- XT-INTERPRET)             @ ;
    head_utils(RECTYPETOINT,RECTYPE>INT,docolon)
        DW FETCH
        DW EXIT

; :NONAME  ABORT" ?"  ;
REC_NULL_XT:
        call docolon
        DW XSQUOTE
        DB 1,"?"
        DW QABORT

; ' NOOP ' NOOP ' NOOP  RECTYPE: RECTYPE-NULL
    head_utils(RECTYPE_NULL,RECTYPE-NULL,docreate)
        dw REC_NULL_XT
        dw REC_NULL_XT
        dw REC_NULL_XT

; ' NOOP ' NOOP ' NOOP  RECTYPE: RECTYPE-NOOP
    head_utils(RECTYPE_NOOP,RECTYPE-NOOP,docreate)
        dw NOOP
        dw NOOP
        dw NOOP

;: (recognize) ( addr len XT -- addr len 0 | i*x RECTYPE-TOKEN -1 )
;   ROT ROT 2DUP 2>R ROT EXECUTE 2R> ROT
;   DUP RECTYPE-NULL =          ( -- i*x addr len RECTYPE-TOKEN f )
;   IF DROP 0 ELSE NIP NIP -1 THEN   ;
XRECOGNIZE:
        call docolon
        dw ROT,ROT,TWODUP,TOR,TOR,ROT,EXECUTE,RFROM,RFROM,ROT
        dw DUP,RECTYPE_NULL
        dw EQUAL,qbranch,XRECOGNIZE1
        dw DROP,FALSE,branch,XRECOGNIZE2
XRECOGNIZE1:
        dw NIP,NIP,TRUE
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
    head_utils(RECOGNIZE,RECOGNIZE,docolon)
        DW lit,XRECOGNIZE,SWOP,STACKUNTIL
        DW ZEROEQUAL,qbranch,RECOGNIZE1
        DW TWODROP,RECTYPE_NULL
RECOGNIZE1:
        DW EXIT


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
    head_utils(REC_FIND,REC-FIND,docolon)
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
    head_utils(REC_NUMBER,REC-NUMBER,docolon)
        DW lit,0,DUP,TWOSWAP
        DW QSIGN,TOR,TONUMBER,qbranch,RECNUM1
        DW RFROM,TWODROP,TWODROP,lit,0
        DW branch,RECNUM3
RECNUM1:  DW TWODROP,RFROM,qbranch,RECNUM2,NEGATE
RECNUM2:  DW TRUE

RECNUM3:
        DW qbranch,RECNUM4
        DW RECTYPE_NUM
        DW EXIT
RECNUM4:
        DW RECTYPE_NULL
        DW EXIT


;    RECTYPE: RECTYPE-NUM
    head_utils(RECTYPE_NUM,RECTYPE-NUM,docreate)
        DW NOOP
        DW LITERAL
        DW LITERAL

; : POSTPONE ( "name" -- )  \ COMPILE
; This is the 16K ROM Next Generation version
;   BL WORD  COUNT
;     RECOGNIZERS RECOGNIZE   ( xt flags RECTYPE_XT | RECTYPE_NULL )
;     DUP
;     >R                 ( call POST action )
;     RECTYPE>POST EXECUTE
;     R>
;     RECTYPE>COMP COMMA   ( add compile action to definition )     ;
POSTPONE_16K:
        call docolon
        DW BL,WORD,COUNT
        DW RECOGNIZERS,RECOGNIZE
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
;       RECOGNIZERS RECOGNIZE   ( i*x RECTYPE_XXX | RECTYPE_NULL )
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
           DW RECOGNIZERS,RECOGNIZE
           DW DUP,RECTYPE_NULL,NOTEQUAL,qbranch,INTRP_NG2
           DW STATE,FETCH,qbranch,INTRP_NG3
           DW RECTYPETOCOMP,EXECUTE
           DW branch,INTRP_NG4
INTRP_NG3: DW RECTYPETOINT,EXECUTE
INTRP_NG4: DW RFROM,DROP,branch,INTRP_NG5
INTRP_NG2: DW DROP,RFROM,COUNT,TYPE,lit,63,EMIT,CR,ABORT
INTRP_NG5: DW branch,INTRP_NG1
INTRP_NG9: DW CHECK_SP,DROP
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
    head_utils(RECTYPE_IHEX,RECTYPE-IHEX,docreate)
        DW REC_IHEX_XT
        DW REC_IHEX_COMP
        DW NOOP


;: REC-IHEX ( addr len -- src dest n RECTYPE_IHEX   if ok, RECTYPE_NULL if not recognised )
;    IHEX? DUP 1 = IF
;       DROP
;       RECTYPE_NOOP EXIT
;    THEN
;    DUP 0= IF
;       DROP
;       RECTYPE_NULL EXIT
;    THEN
;    DROP RECTYPE_IHEX  ;
    head_utils(REC_IHEX,REC-IHEX,docolon)
        DW IHEXQ,DUP,lit,1,EQUAL,qbranch,REC_IHEX1
        DW DROP
        DW RECTYPE_NOOP,EXIT
REC_IHEX1:
        DW DUP,ZEROEQUAL,qbranch,REC_IHEX2
        DW DROP
        DW RECTYPE_NULL,EXIT
REC_IHEX2:
        DW DROP,RECTYPE_IHEX,EXIT


;Z SAVE-INPUT
;   REFILL-VEC @ SOURCE-ID @ BLK @ 'SOURCE 2@  >IN @  4 N>R  ;
    head(SAVE_INPUT,SAVE-INPUT,docolon)
        DW REFILLVEC,FETCH
        DW SOURCE_ID,FETCH
        DW BLK,FETCH
        DW TICKSOURCE,TWOFETCH
        DW TOIN,FETCH

        DW lit,6

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
;   NR> DROP  >IN !  'SOURCE 2! BLK ! SOURCE-ID ! REFILL-VEC ! ;
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
        DW BLK,STORE
        DW SOURCE_ID,STORE
        DW REFILLVEC,STORE
        DW EXIT


; RC2014 Multitasking ====================

;Z UP!
    head(UPSTORE,UP!,docode)
        push bc
        pop iy
        pop bc
        next

;Z UP@
    head(UPFETCH,UP@,docode)
        push bc
        push iy
        pop bc
        next


;Z <INIT>   - "init" task state
;  R> CELL- UP!
;  S0 SP!
;  R0 RP!
;  L0 LP !
;  0 STATE !
;  0 HANDLER !
;  0 BLK !
;  ['] <WAKE> U0 !   ( we're away! )
;  ENTRY @ EXECUTE
;  ['] <SLEEP> U0 !  ( if we come back, the stop this thread )
;  (PAUSE)   ;
    head_utils(XINIT,<INIT>,docolon)
       dw RFROM,CELLMINUS,UPSTORE
XINIT1:
       dw S0,SPSTORE
       dw R0,RPSTORE
       dw L0,LP,STORE
       dw lit,0,STATE,STORE
       dw lit,0,HANDLER,STORE
       dw lit,0,BLK,STORE
       dw lit,XWAKE,U0,STORE
       dw ENTRY,FETCH,EXECUTE
       dw lit,XSLEEP,U0,STORE
       dw XPAUSE
       dw branch,XINIT1

;Z <WAKE>   -- "wake" task state
;  R> CELL- UP!
;  STACKTOP @ SP! RP!  ;
    head_utils(XWAKE,<WAKE>,docolon)
       dw RFROM,CELLMINUS,UPSTORE
       dw STACKTOP,FETCH,SPSTORE,RPSTORE
       dw EXIT

;Z <SLEEP>  -- "sleeping" task state
;  R> CELL- UP! LINK @ >R  ;
    head_utils(XSLEEP,<SLEEP>,docolon)
       dw RFROM,CELLMINUS,UPSTORE
       dw LINK,FETCH,TOR
       dw EXIT

;Z (PAUSE)     -- pause run-time
;  RP@ SP@ STACKTOP !
;  LINK @ >R ; COMPILE-ONLY
    head_utils(XPAUSE,(PAUSE),docolon)
        dw RPFETCH,SPFETCH,STACKTOP,STORE
        dw LINK,FETCH,TOR
        dw EXIT

;Z INIT-TASK     ( task-id --  )
    head_utils(INIT_TASK,INIT-TASK,docolon)
       dw U0,OVER,lit,256,MOVE  ; copy USER variables
       dw DUP,LINK,STORE
       dw lit,XINIT,SWOP,STORE      ; set new task STATE to <INIT>
       dw EXIT


    head_utils(TASKSIZE,TASK%,docon)
        dw 768

; RC2014 16K initialisation ====================


;Z /16KROM    init enhanced features
SLASH16KROM:
        call docolon
        DW U0,LINK,STORE
        DW lit,XWAKE,U0,STORE
        DW lit,FIND_16K,lit,xt_find,STORE
        DW lit,POSTPONE_16K,lit,xt_postpone,STORE
        DW lit,INTERPRET_16K,lit,xt_interpret,STORE
        DW lit,WORDS_16K,lit,xt_words,STORE
        DW lit,editor_lastword,EDITOR_WORDLIST,STORE
        DW lit,utils_lastword,UTILS_WORDLIST,STORE
        DW lit,vocab_lastword,VOCAB_WORDLIST,STORE
        DW WORDLISTS,lit,STACK_WORDLISTS_SIZE,SLASHSTACK
        DW VOCAB_WORDLIST,FORTH_WORDLIST,lit,2,WORDLISTS,STACKSET
        DW RECOGNIZERS,lit,STACK_RECOGNIZERS_SIZE,SLASHSTACK
        DW lit,REC_IHEX,lit,REC_NUMBER,lit,REC_FIND,lit,3,RECOGNIZERS,STACKSET
        DW FORTH_WORDLIST,CURRENT,STORE
        DW SLASHBLKCTX
        DW SLASHTEMPBUFF
        DW XSQUOTE
        DB 10," - 16K ROM"
        DW TYPE,CR
        DW SLASHCFLASH
        DW lit,0,DISK
        DW DUP,TOR,DISKTODRIVE,STORE
        DW lit,0x2000,RFETCH,DISKTOLIMIT,STORE
        DW lit,0,lit,0,RFETCH,DISKTOOFFSET,TWOSTORE
        DW lit,NOOP,RFROM,DISKTOTRANSLATOR,STORE
        DW lit,0,DSK,STORE
        dw EXIT

