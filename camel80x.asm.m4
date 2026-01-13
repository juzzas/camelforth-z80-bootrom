; RC2014 Extension Words - 16K
;
; ===============================================
; CamelForth for the Zilog Z80
; Copyright (c) 1994,1995 Bradford J. Rodriguez
; Copyright (c) 2020-2025 Justin Skists
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


;Z BLK      -- a-addr                      block number storage
;  20 USER BLK
    head(USERBLK,BLK,docode)
        jp BLK

;Z SLICE-ID      -- a-addr          storage of current slice-id
;  22 USER SLICE-ID
    head(USERSLICE_ID,SLICE-ID,douser)
        JP SLICE_ID

;Z SCR          -- a-addr             last edited screen number
;  24 USER SCR
    head(SCR,SCR,douser)
        dw 24

;Z ENTRY      -- a-addr   address of xt for entry point of task
;  28 USER ENTRY
    head(ENTRY,ENTRY,douser)
        dw 28

;Z 'REFILL      -- xt           if set, use XT as REFILL source
;   USER 'REFILL
    head(TICKREFILL,'REFILL,docode)
        jp REFILLVEC

;Z LINK        -- a-addr     link to following round-robin task
;  36 USER LINK
    head(LINK,LINK,douser)
        dw 36

;Z STACKTOP    -- a-addr   address of stored stack top for task
;  38 USER STACKTOP
    head(STACKTOP,STACKTOP,douser)
        dw 38


EXTERN intvec_ptr
;Z 'INT  -- a-addr        pointer to address holding int vector
;  intvec_ptr CONSTANT INTVEC
    head(INTVEC,'INT,docon)
        dw intvec_ptr

EXTERN nmivec_ptr
;Z 'NMI      -- a-addr    pointer to address holding int vector
;  intvec_ptr CONSTANT NMIVEC
    head(NMIVEC,'NMI,docon)
        dw nmivec_ptr

EXTERN pausevec_ptr
;Z 'PAUSE   -- a-addr   pointer to address holding pause vector
;  pausevec_ptr CONSTANT 'INTVEC
    head(TICKPAUSE,'PAUSE,docode)
        jp PAUSEVEC


;Z RAMTOP      -- u                         return RAMPTOP as u
;  ramtop_ptr CONSTANT RAMTOP
    head_system(RAMTOP,RAMTOP,docolon)
        dw lit,ramtop_ptr,FETCH,EXIT

;Z RAMTOP!    u --      set RAMTOP to be address specified by u
;  ramtop_ptr CONSTANT RAMTOP
    head_system(RAMTOPSTORE,RAMTOP!,docolon)
        dw lit,ramtop_ptr,STORE
        DW SLASHBLKCTX
        dw EXIT

;Z @EXECUTE   i*x addr -- j*x      execute Forth word at 'addr'
    head(FETCHEXECUTE,@EXECUTE,docode)
        ld h,b          ; address in hl
        ld l,c
        ld c, (hl)
        inc hl
        ld b, (hl)
        jp EXECUTE

;Z ?@EXECUTE   i*x addr-- j*x  execute word at 'addr', if not 0
    head(QFETCHEXECUTE,?@EXECUTE,docode)
        ld h,b          ; address in hl
        ld l,c
        ld c, (hl)
        inc hl
        ld b, (hl)
        jp QEXECUTE

SECTION data

ramtop_ptr:
        DEFS 2

SECTION code_16k

;Z CFA>NFA   cfa -- nfa                 code field -> namefield
;  32 1 DO      ( cfa )
;    DUP I -    ( cfa nfa? )
;    2DUP NFA>CFA =   ( cfa nfa? f )
;    IF NIP UNLOOP EXIT ELSE DROP THEN
;  LOOP
;  DROP 0 ;
    head(CFATONFA,CFA>NFA,docolon)
        DW lit,32,lit,1,xdo
CFATONFA1:
        DW DUP,II,MINUS
        DW TWODUP,NFATOCFA,EQUAL
        DW qbranch,CFATONFA2
        DW NIP,UNLOOP,EXIT
CFATONFA2:
        DW DROP,xloop,CFATONFA1
        DW DROP,ZERO
        DW EXIT

;Z (ID)  xt -- c-addr u   or   xt 0           return name of xt
;    DUP CFA>NFA   ( xt nfa|0 )
;    DUP IF  ( xt nfa|0 )
;       NIP COUNT $7F AND
;    THEN  ;
    head(XID,(ID),docolon)
        DW DUP,CFATONFA
        DW DUP,qbranch,XID1
        DW NIP,COUNT,lit,0x7f,AND
XID1:
        DW EXIT

;Z ID.   xt --                 print out NFA, if found, else XT
;    (ID)  ?DUP IF TYPE ELSE U. THEN ;
    head(DOTID,.ID,docolon)
        DW XID,QDUP,qbranch,DOTID1
        DW TYPE,EXIT
DOTID1:
        DW UDOT,EXIT


;X FORTH-WORDLIST ( -- wid )        return wordlist id of FORTH
;\ Return wid, the identifier of the word list that includes all standard
;\ words provided by the implementation. This word list is initially the
;\ compilation word list and is part of the initial search order.
        head(FORTH_WORDLIST,FORTH-WORDLIST,docode)
            jp LATEST

;Z VOCAB-WORDLIST ( -- wid )           get wordlist id of VOCAB
        head(VOCAB_WORDLIST,VOCAB-WORDLIST,docon)
            dw vocab_wordlist_head

;Z SYSTEM-WORDLIST ( -- wid )         get wordlist id of SYSTEM
        head(SYSTEM_WORDLIST,SYSTEM-WORDLIST,docon)
            dw system_wordlist_head

;Z ENVIRONMENT-WORDLIST ( -- wid )  get wordlist id of envir'mt
        head(ENVIRONMENT_WORDLIST,ENVIRONMENT-WORDLIST,docon)
            dw environment_wordlist_head

;Z LOCALS-WID   ( -- wid )
    head_system(LOCALS_WID,LOCALS-WID,docon)
        DW locals_wid_head


;Z (CREATE-WID)  c-addr u wid --    create an empty def. to WID
;   DUP WID>NFA , 0 C,         link & `immed' field
;   HERE SWAP WID>NFA!               new "latest" link on wid
;   2DUP HERE PLACE 1+ ALLOT DROP        name field
;   docreate ,CF                code field
    head(XCREATE_WID,(CREATE-WID),docolon)
        DW DUP,WIDTONFA,COMMA,ZERO,CCOMMA
        DW HERE,SWOP,WIDTONFASTORE
        DW TWODUP,HERE,PLACE,ONEPLUS,ALLOT,DROP
        DW lit,docreate,COMMACF,EXIT

;C .(    --                                  emit input until )
;   [ HEX ] 29 WORD COUNT TYPE ; IMMEDIATE
    immed(DOTPAREN,``.('',docolon)
        DW lit,29H,WORD,COUNT,TYPE,EXIT


dnl ;X :NONAME  -- xt                       define anonymous xt
dnl ;    CURRENT @ @ , 0 C,    ( last link + immed flag )
dnl ;    HERE CURRENT @ !      ( new "latest" )
dnl ;    0 C,          ( empty NFA )
dnl ;    HERE               ( push xt to stack             )
dnl ;    HIDE ] !COLON  ;   ( start compiling as a docolon )
    head(NONAME,:NONAME,docolon)
        dw CURRENT,FETCH,FETCH,COMMA,ZERO,CCOMMA
        dw HERE,CURRENT,FETCH,STORE
        dw ZERO,CCOMMA
        dw HERE
        dw HIDE,RIGHTBRACKET,lit,docolon,COMMACF
        dw EXIT

;Z MS  n --                                delay n milliseconds
    head(MS,MS,docode)
        push hl
        push de
        call asm_z80_delay_ms
        pop de
        pop hl
        pop bc
        next

;C UNUSED   -- u               return unused space in data area
    head(UNUSED,UNUSED,docolon)
        dw BLKFIRST,HERE,MINUS
        dw EXIT

;C BOUNDS   c-addr n -- n-end n-start       get bounds for a DO
;    OVER + SWAP ;
    head(BOUNDS,BOUNDS,docolon)
        DW OVER,PLUS,SWOP,EXIT

;Z MAP  xt n-end n-start --  execute xt for every item in array
;\ XT should not return anything on stack
;      ?DO     ( xt )
;          I @ SWAP DUP >R EXECUTE R>
;      CELL +LOOP
;      DROP ;
    head_system(MAP,MAP,docolon)
        DW TWODUP,EQUAL,qbranch,MAP0
        DW TWODROP,DROP,EXIT
MAP0:
        DW xdo
MAP1:
        DW II,FETCH,SWOP,DUP,TOR,EXECUTE,RFROM
        DW CELL,xplusloop,MAP1
        DW DROP
        DW EXIT

;Z MAP-UNTIL  ( x*i xt n-end n-start -- x*j flag )
;\    execute xt for every item in array, until xt returns true
;\ XT should return flag on stack
;    ?DO          ( x*i xt )
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
    head_system(MAP_UNTIL,MAP-UNTIL,docolon)
        DW TWODUP,EQUAL,qbranch,MAPUNTIL0
        DW TWODROP,DROP,FALSE,EXIT
MAPUNTIL0:
        DW xdo
MAPUNTIL1:
        DW II,FETCH,SWOP
        DW DUP,TOR
        DW EXECUTE
        DW RFROM
        DW OVER,qbranch,MAPUNTIL2
        DW DROP,UNLOOP,EXIT
MAPUNTIL2:
        DW NIP
        DW CELL,xplusloop,MAPUNTIL1
MAPUNTIL3:
        DW DROP,FALSE,EXIT


;Z >SPADC    addr u -- c-addr'    allocate string PAD for c str
;\  allocate an SPAD and copy counted string to it
;   SPAD               ( addr u addr' )
;   DUP >R         ( addr u addr'   r: addr' )
;   PLACE R>         ( c-addr' )
    head(TOSPADC,>SPADC,docolon)
       DW SPAD,DUP,TOR,PLACE,RFROM
       DW EXIT

;Z (C")     -- c-addr u                    run-time code for C"
;   R> DUP COUNT + ALIGNED >R  ;
    head(XCQUOTE,(C"),docolon)
        DW RFROM,DUP,COUNT,PLUS,ALIGNED,TOR
        DW EXIT

;C C"       --                   compile in-line counted string
;\ C"       -- addr            interpret in-line counted string
;   22 PARSE       ( addr u )
;  STATE @ IF
;   COMPILE (C")  [ HEX ]
;   HERE >counted HERE C@ 1+ ALIGNED ALLOT EXIT
;  ELSE
;   SPAD DUP >R  >SPAD              ( addr' u' )
;  THEN ; IMMEDIATE
    immed(CQUOTE,C",docolon)
        DW lit,22H,PARSE
        DW STATE,FETCH,qbranch,CQUOTE1

        DW lit,XCQUOTE,COMMAXT
        DW HERE,PLACE,HERE,CFETCH,ONEPLUS
        DW ALIGNED,ALLOT,EXIT

CQUOTE1:
        DW TOSPADC
        DW EXIT



;Z $,             \ caddr len --                 compile string
;\ Lay the string into the dictionary at HERE, reserve
;\ space for it and ALIGN the dictionary.
;   HERE >counted HERE C@ 1+ ALIGNED ALLOT ;
    head(STRINGCOMMA,``$,'',docolon)
        DW HERE,PLACE
        DW HERE,CFETCH,ONEPLUS,ALIGNED,ALLOT
        DW EXIT

;Z ",            \ "ccc<quote>"  --    parse and compile string
;\ Parse text up to the closing quote and compile into
;\ the dictionary at HERE as a counted string.
;\ The end of the string is aligned.
;   [char] " parse $,   ;
    immed(QUOTECOMMA,``",'',docolon)
        DW lit,'"',PARSE,STRINGCOMMA,EXIT


;Z ADDCHAR    c c-addr --           append char to counted str.
;\ Add the character to the end of the counted string.
;  TUCK COUNT + C!
;  1 SWAP C+!    ;
dnl    head(ADDCHAR,ADDCHAR,docolon)
;        DW TUCK,COUNT,PLUS,CSTORE
;        DW lit,1,SWOP,CPLUSSTORE
;        DW EXIT

;Z APPEND    c-addr u c-dest  --  append dtring to counted str.
;\ Add the string described by c-addr/u to the counted
;\ string at c-dest.
;  2DUP C@ TUCK +    \ -- caddr u $dest lend lens+lend ; new len
;  2 PICK C!         \ -- caddr u $dest lend ; update length
;  CHAR+ +           \ -- caddr u $d+1+lend ; dest buffer addr
;  SWAP MOVE   ;      \ -- ; copy string
dnl    head(APPEND,APPEND,docolon)
;        DW TWODUP,CFETCH,TUCK,PLUS
;        DW lit,2,PICK,CSTORE
;        DW CHARPLUS,PLUS
;        DW SWOP,MOVE
;        DW EXIT

;X D0<   d1 -- f                                        d1 < 0?
;   NIP 32768 AND 0<> ;
    head(DZEROLESS,D0<,docolon)
        DW NIP,lit,32768,AND,ZERONOTEQUAL,EXIT

; RC2014 EXTENSION output ====================

;X ?DO       -- if-adrs -1 adrs   L: -- 0        conditional DO
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


dnl ;: VT-ESC  ( --  emit escape character )
dnl ;    27 EMIT [CHAR] [ EMIT ;
dnl     head(VT_ESC,VT-ESC,docolon)
dnl         dw lit,0x1b,EMIT
dnl         dw lit,'[',EMIT
dnl         dw EXIT

dnl ;: DEFER    ( "name" -- )      \  create a deferred word
dnl ;   CREATE ['] NOOP ,
dnl ;   DOES>
dnl ;   @EXECUTE ;
dnl     head(DEFER,DEFER,docolon)
dnl         DW CREATE,lit,-3,ALLOT
dnl         DW lit,dodefer,COMMACF
dnl         DW lit,NOOP,COMMAXT,EXIT
dnl 
dnl ;: DEFER!
dnl ;    >BODY ! ;
dnl     head(DEFERSTORE,DEFER!,docolon)
dnl         DW TOBODY,STORE,EXIT
dnl 
dnl ;: DEFER@
dnl ;    >BODY @ ;
dnl     head(DEFERFETCH,DEFER@,docolon)
dnl         DW TOBODY,FETCH,EXIT
dnl 
dnl 
dnl ;: IS       ( xt "name" -- )     \ define a deferred word
dnl ;   STATE @  IF  POSTPONE [']  POSTPONE DEFER!
dnl ;   ELSE  ' DEFER!  THEN ; IMMEDIATE
dnl     immed(IS,IS,docolon)
dnl         DW STATE,FETCH,qbranch,IS1
dnl         DW BRACTICK,lit,DEFERSTORE,COMMAXT,EXIT
dnl IS1:
dnl         DW TICK,DEFERSTORE,EXIT

;X SLITERAL    c-addr u --               compile string literal
;    (SLITERAL)   ; IMMEDIATE
    immed(SLITERAL,SLITERAL,docode)
        jp XSLITERAL


;X 2LITERAL  x1 x2 --             append double numeric literal
;   STATE @ IF ['] DLIT ,XT , , THEN ; IMMEDIATE
; This tests STATE so that it can also be used
; interpretively.  (ANSI doesn't require this.)
    immed(TWOLITERAL,2LITERAL,docolon)
        DW STATE,FETCH,qbranch,DLITER1
        DW lit,dlit,COMMAXT,COMMA,COMMA
DLITER1: DW EXIT

;X 2VARIABLE   --                define a Forth double variable
;   CREATE 2 CELLS ALLOT ;
    head(TWOVARIABLE,2VARIABLE,docolon)
        DW CREATE,lit,4,ALLOT,EXIT

;X 2CONSTANT   x1 x2  --         define a Forth double constant 
;   CREATE , , DOES> 2@  ;
    head(TWOCONSTANT,2CONSTANT,docolon)
        DW CREATE,COMMA,COMMA
        DW XDOES
        call dodoes
        DW TWOFETCH
        DW EXIT

;X 2>R   x x --                                    2 cells to R
    head(TWOTOR,2>R,docode)
        ld l,c
        ld h,b
        pop bc
        dec ix          ; push TOS onto rtn stk
        ld (ix+0),b
        dec ix
        ld (ix+0),c
        dec ix          ; push TOS onto rtn stk
        ld (ix+0),h
        dec ix
        ld (ix+0),l
        pop bc          ; pop new TOS
        next


;X 2R>   -- x x                            fetch 2 cells from R
    head(TWORFROM,2R>,docode)
        push bc         ; push old TOS
        ld c,(ix+0)     ; pop top rtn stk item
        inc ix          ;       to TOS
        ld b,(ix+0)
        inc ix
        ld l,(ix+0)     ; pop top rtn stk item
        inc ix          ;       to TOS
        ld h,(ix+0)
        inc ix
        push hl
        next

;X 2R@   -- x x                            fetch 2 cells from R
    head(TWORFETCH,2R@,docode)
        push bc         ; push old TOS
        ld c,(ix+2)     ; fetch top rtn stk item
        ld b,(ix+3)     ;       to TOS
        push bc
        ld c,(ix+0)     ; fetch top rtn stk item
        ld b,(ix+1)     ;       to TOS
        next

;X 2NIP     x1 x2 x3 x4 -- x3 x4              per stack diagram
;   2>R 2DROP 2R> ;
    head(TWONIP,2NIP,docolon)
        DW TWOTOR,TWODROP,TWORFROM
        DW EXIT

;X N>R    ( i * n +n -- ) ( R: -- j * x +n )       n cells to R
    head(NTOR,N>R,docode)
        push bc
        exx
        pop hl          ; hl = count
        ld a,h
        or l
        jr z, ntor_done
        ld b,l
ntor_loop:
        pop de
        dec ix          ; push item onto rtn stk
        ld (ix+0),d
        dec ix
        ld (ix+0),e
        djnz ntor_loop
ntor_done:
        dec ix          ; push stack count onto rtn stk
        ld (ix+0),h
        dec ix
        ld (ix+0),l
        exx 
        pop bc
        next

;X NR>   ( -- i * x +n ) ( R: j * x +n -- )      n cells from R
    head(NRFROM,NR>,docode)
        push bc
        exx
        ld l,(ix+0)     ; pop count from rtn skt
        inc ix          ;
        ld h,(ix+0)
        inc ix

        ld a,h
        or l
        jr z, nrfrom_done
        ld b, l
nrfrom_loop:
        ld e,(ix+0)     ; pop item from rtn skt
        inc ix          ;       
        ld d,(ix+0)
        inc ix
        push de
        djnz nrfrom_loop

nrfrom_done:
        push hl
        exx
        pop bc
        next


;C 0<>     x1 -- flag                          test not eq to 0
    head(ZERONOTEQUAL,0<>,docode)
        ld a,b
        or c
        jp nz, tostrue
        jp tosfalse

;C 0>     n -- flag                         test greater than 0
    head(ZEROGREATER,0>,docode)
        ld a,b
        or c
        jp z,tosfalse
        ld a,b
        and 0x80
        jp nz, tosfalse
        jp tostrue

;X D-    d1 d2 -- d1-d2                 Subtract double numbers
    head(DMINUS,D-,docode)
        exx
        pop bc          ; BC'=d2lo
        exx
        pop hl          ; HL=d1hi,BC=d2hi
        exx
        pop hl          ; HL'=d1lo
        or a            ; clear cy
        sbc hl,bc
        push hl         ; TOS=d1lo-d2lo
        exx
        sbc hl,bc       ; HL=d1hi-2hi+cy
        ld b,h
        ld c,l
        next


;X D=  d1 d2 -- f                       equality of two doubles
;   rot = -rot = and ;
    head(DEQUAL,D=,docolon)
        dw ROT,EQUAL,ROT,ROT,EQUAL,AND,EXIT

;X D<  ( d1 d2 -- flag )                               d1 < d2?
;    rot 2dup = if 2drop u< EXIT THEN 2nip >  ;
    head(DLESS,D<,docolon)
        DW ROT,TWODUP,EQUAL,qbranch,DLESS1
        DW TWODROP,ULESS,EXIT
DLESS1:
        DW TWONIP,GREATER
        DW EXIT

;X D0=   d -- f                                  is double a 0?
;    OR 0=  ;
    head(DZEROEQUAL,D0=,docolon)
        DW OR,ZEROEQUAL,EXIT

;X D2/  d -- d               shift double right, perserving msb
    head(DTWOSLASH,D2/,docode)
        pop hl
        sra b
        rr c
        rr h
        rr l
        push hl
        next

;X D2*   d -- d                               shift double left
    head(DTWOSTAR,D2*,docode)
       pop hl 
       sla l
       rl h
       rl c
       rl b
       push hl
       next


;X DMAX     d1 d2 -- d              get max between two doubles
; 2over 2over D< if 
;    drop  2swap 2drop  exit
;  then
;  drop  2drop   ;
    head(DMAX,DMAX,docolon)
        DW TWOOVER,TWOOVER,DLESS
        DW qbranch,DMAX1
        DW TWONIP,EXIT
DMAX1:
        DW TWODROP,EXIT

;X DMIN    d1 d2 -- d               get min between two doubles
;  2over 2over 2swap d< if
;    drop  2swap 2drop  exit
;  then
;  drop  2drop   ;
;
    head(DMIN,DMIN,docolon)
        DW TWOOVER,TWOOVER,TWOSWAP,DLESS
        DW qbranch,DMIN1
        DW TWONIP,EXIT
DMIN1:
        DW TWODROP,EXIT

;C AHEAD       -- orig             unconditional forward branch
;   ['] branch ,BRANCH  HERE DUP ,DEST ;
;   IMMEDIATE
    immed(AHEAD,AHEAD,docolon)
        DW lit,branch,COMMABRANCH
        DW HERE,DUP,COMMADEST,EXIT

;X BUFFER:    u "name" --      create a named buffer of u bytes
;\        execution:   -- addr 
    head(BUFFERCOLON,BUFFER:,docolon)
        DW CREATE,ALLOT
        DW EXIT

;: SKIP-SPACE   c-addr u -- c-addr' u'
;                          skip chars <32
dnl    head(SKIP_SPACES,SKIP-SPACES,docode)
SKIP_SPACES:
        push bc
        exx
        pop bc      ; count
        pop hl      ; address
skipspaceloop:
        ld a,b
        or c
        ld e,a      ; test for count=0
        ld a,b
        or c
        jr z,skipspacedone
        ld a,32
        cp (hl)     ; If A >= (hl), then C flag is reset. 
        jr c,skipspacedone  ; match, BC & HL ok
        inc hl
        dec bc
        jr skipspaceloop
skipspacedone:
        push hl     ; updated address
        push bc     ; updated count
        exx
        pop bc      ; TOS in bc
        next

;: SCAN-SPACE    c-addr u -- c-addr' u'
;                      find char <32
dnl    head(SCAN_SPACE,SCAN-SPACE,docode)
SCAN_SPACE:
        push bc
        exx
        pop bc      ; count
        pop hl      ; address
scanspaceloop:
        ld a,b      ; test for count=0
        or c
        jr z,scanspacedone
        ld a,32
        cp (hl)     ; If A >= (hl), then C flag is reset. 
        jr nc,scanspacedone  ; match, BC & HL ok
        inc hl
        dec bc
        jr scanspaceloop
scanspacedone:
        push hl     ; updated address
        push bc     ; updated count
        exx
        pop bc      ; TOS in bc
        next

;X PARSE-NAME   "<spaces>name<space>" -- c-addr u    parse word
;\ Skip leading space delimiters. Parse name delimited by a space.
; 
; c-addr is the address of the selected string within the input
; buffer and u is its length in characters. If the parse area
; is empty or contains only white space, the resulting string
; has length zero. 
;   SOURCE >IN @ /STRING
;   SKIP-SPACES OVER >R
;   SCAN-SPACE   ( end-word restlen r: start-word )
;   2DUP 1 MIN + SOURCE DROP - >IN !
;   DROP R> TUCK - ;
    head(PARSE_NAME,PARSE-NAME,docolon)
        dw SOURCE,TOIN,FETCH,SLASHSTRING
        dw SKIP_SPACES,OVER,TOR
        dw SCAN_SPACE
        dw TWODUP,lit,1,MIN,PLUS,SOURCE,DROP,MINUS,TOIN,STORE
        dw DROP,RFROM,TUCK,MINUS
        dw EXIT


;X HOLDS ( addr u -- )                add string to HOLD buffer
; Adds the string represented by c-addr u to the pictured 
; numeric output string. An ambiguous condition exists if 
; HOLDS executes outside of a <# #> delimited number 
; conversion.
;    BEGIN DUP WHILE 1- 2DUP + C@ HOLD REPEAT 2DROP ;
    head(HOLDS,HOLDS,docolon)
HOLDS1:
        dw DUP
        dw qbranch,HOLDS2
        dw ONEMINUS,TWODUP,PLUS,CFETCH,HOLD
        dw branch,HOLDS1
HOLDS2:
        dw TWODROP,EXIT


dnl ; +USER defines next USER variables 
PLUSUSERPTR:
        call docon
        DW plususer_ptr


dnl ;Z +USER                      define next user variable 'n'
dnl ;   CREATE PLUSUSERPTR, DOES> (machine code fragment)
    head(PLUSUSER,+USER,docolon)
        DW CREATE,PLUSUSERPTR,FETCH
        DW DUP,COMMA,CELLPLUS,PLUSUSERPTR,STORE
        DW XDOES
        jp douser



SECTION data

plususer_ptr:
        DS 2

SECTION code_16k

; RC2014 EXTENDED STRINGS =======================

; RC2014 EXTENDED STRUCTURES ====================

;Z CELL-    a-addr1 -- a-addr2               subtract cell size
;   2 - ;
    head(CELLMINUS,CELL-,docode)
        dec bc
        dec bc
        next


dnl ; http://www.forth.org/svfig/Len/softstak.htm

dnl ;     lifo+0 -> ptr to top of stack - 2
dnl ;     lifo+3 -> S0: bottom of stack
dnl ;     lifo+2+n -> bottom of stack

dnl ; note: on empty stack, S0 and SP point to beyond end of area.
dnl ;       SP decremented before storing.

; initialise a stack at addr with n bytes
; : /STACK   ( addr n -- )
;      OVER +         ( addr S0 )
;      SWAP 2DUP CELL+     ( SP sp-addr S0 s0-addr )
;      !  !   ;
dnl ;    head_system(SLASHSTACK,``/STACK'',docolon)
SLASHSTACK:
        call docolon
        DW TWODUP,ZERO,FILL
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
    head_system(STACK,STACK:,docolon)
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
     head_system(TOSTACK,>S,docolon)
;TOSTACK:
;        call docolon
        DW SWOP,OVER,FETCH
        DW CELLMINUS,STORE
        DW CELL,NEGATE,SWOP,PLUSSTORE
        DW EXIT

; Pop number from STACK
; : S> ( lifo -- x )
;      DUP @ @        ( lifo x )
;      SWAP           ( x lifo  )
;      CELL SWAP +!  ;  ( x )
    head_system(STACKFROM,S>,docolon)
;STACKFROM:
;        call docolon
        DW DUP,FETCH,FETCH
        DW SWOP
        DW CELL,SWOP,PLUSSTORE
        DW EXIT

; Fetch the value at the top of the STACK
; : S@ ( lifo -- x )
;      @  @ ;
dnl    head_system(STACKFETCH,S@,docolon)
STACKFETCH:
        call docolon
        DW FETCH,FETCH
        DW EXIT

; Replace the value at the top of the STACK
; : S! ( x lifo -- )
;      @ ! ;
dnl    head_system(STACKSTORE,S!,docolon)
STACKSTORE:
        call docolon
        DW FETCH,STORE
        DW EXIT

; Drop the value at the top of the STACK
; : SDROP ( lifo -- )
;      S> DROP ;
dnl    head_system(STACKDROP,SDROP,docolon)
;STACKDROP:
;        call docolon
;        DW STACKFROM,DROP
;        DW EXIT

; Duplicate the value at the top of the STACK
; : SDUP ( lifo -- )
;      DUP S@ SWAP >S ;
dnl    head_system(STACKDUP,SDUP,docolon)
STACKDUP:
        call docolon
        DW DUP,STACKFETCH,SWOP,TOSTACK
        DW EXIT

dnl ; PICK for stack
dnl ; : SPICK ( n lifo -- x )
dnl ;     @ SWAP CELLS + @ ;
dnl     head_system(SPICK,SPICK,docolon)
dnl         DW FETCH,SWOP,CELLS,PLUS,FETCH
dnl         DW EXIT

; : STACK-DEPTH ( lifo -- n )
;      STACK.BOUNDS - CELL /  ;
dnl    head_system(STACKDEPTH,SDEPTH,docolon)
STACKDEPTH:
        call docolon
        DW STACKBOUNDS,MINUS
        DW TWOSLASH      ;  optimize "DW CELL,SLASH" for 16bit
        DW EXIT

dnl ; : STACK-EMPTY? ( lifo -- flag )
dnl ;      STACK.BOUNDS = ;
dnl     head_system(STACKEMPTYQ,SEMPTY?,docolon)
dnl         DW STACKBOUNDS,EQUAL
dnl         DW EXIT

; Create parameters for a ?DO loop that will scan every item currently in STACK. The intended use is:
;      ( lifo )   SBOUNDS ?DO -- CELL +LOOP
; : STACK-BOUNDS ( lifo -- addr1 addr2 )
;     DUP CELL+ @ SWAP @ ;
dnl    head_system(STACKBOUNDS,SBOUNDS,docolon)
STACKBOUNDS:
        call docolon
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
   head_system(STACKSET,STACK-SET,docolon)
;STACKSET:
;        call docolon
        DW OVER,ZEROLESS,qbranch,STACKSET0
        DW lit,-4,THROW
STACKSET0:
        DW OVER,qbranch,STACKSET2
        DW DUP,TOR,CELLPLUS,FETCH
        DW OVER,CELLS,MINUS
        DW DUP,TOR
        DW SWOP,ZERO,xdo
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
   head_system(STACKGET,STACK-GET,docolon)
;STACKGET:
;        call docolon
        DW DUP,STACKDEPTH
        DW DUP,qbranch,STACKGET2
        DW TOR,CELLPLUS,FETCH,CELLMINUS,RFETCH
        DW ZERO,xdo
STACKGET1:
        DW DUP,II,CELLS,MINUS,FETCH
        DW SWOP
        DW xloop,STACKGET1
        DW DROP,RFROM
        DW EXIT

STACKGET2:
        DW NIP
        DW EXIT



; RC2014 EXTENSION CONSTANTS ====================

;Z C/L      -- n                               columns per line
    head(C_L,C/L,docon)
        dw 64

;Z L/B      -- n                                lines per block
    head(L_B,L/B,docon)
        dw 16

;Z B/BLK      -- n                              bytes per block
    head(B_BLK,B/BLK,docon)
        dw 1024


; RC2014 EXTENSION (MISC) =======================

;C ERASE   a-addr u --                            fill with 0's
;    0 FILL
;    ;
    head(ERASE,ERASE,docolon)
        dw ZERO,FILL
        dw EXIT

;C BLANK   a-addr u --                         fill with spaces
;    BL FILL
;    ;
    head(BLANK,BLANK,docolon)
        dw lit,32,FILL
        dw EXIT


; RC2014 EXTENSIONS (TERMINAL) ==================

;Z D.R   d width --                   right align double output
;    >R (D.)
;    R> OVER - SPACES TYPE ;
    head(DDOTR,D.R,docolon)
        dw TOR,XDDOT
        dw RFROM,OVER,MINUS,DUP,ZEROLESS,qbranch,DDOTR1
        dw DROP,TYPE,EXIT
DDOTR1:
        dw SPACES,TYPE,EXIT

;Z .R   n width --                     right align value output
;    >R S>D R> D.R ;
    head(DOTR,.R,docolon)
        dw TOR,STOD,RFROM,DDOTR
        dw EXIT

;Z U.R   u width --         right align unassigned value output
;   0 SWAP D.R ;     ( quick convert to double )
    head(UDOTR,U.R,docolon)
        dw ZERO,SWOP,DDOTR
        dw EXIT

dnl ;Z (D.W)    d width --   print width with leading 0's )
dnl ;    1- DUP 1 < IF DROP 1 THEN <# 0 DO # LOOP #S #> ;
dnl XDDOTW:
dnl         call docolon
dnl         dw ONEMINUS,DUP,lit,1,LESS,qbranch,XDDOTW1
dnl         dw DROP,lit,1
dnl XDDOTW1:
dnl         dw LESSNUM,ZERO,xdo
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
dnl         dw ZERO,SWOP,XDDOTW
dnl         dw EXIT
dnl 
dnl ;Z U.W         ( u width --   width with leading 0's )
dnl ;    (U.W) TYPE SPACE ;
dnl     head(UDOTW,U.W,docolon)
dnl         dw XUDOTW,TYPE,SPACE
dnl         dw EXIT
dnl 
;Z PRINTABLE?   n -- f                  is character printable?
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

system_wordlist_head:
        ds 2

environment_wordlist_head:
        ds 2

locals_wid_head:
        DEFS 2

SECTION code_16k

;X WORDLIST ( -- wid )
; Create a new empty word list, returning its word list identifier wid.
; The new word list may be returned from a pool of preallocated word lists
; or may be dynamically allocated in data space. A system shall allow the
; creation of at least 8 new word lists in addition to any provided as part
; of the system.
;   HERE  0 , ;
        head(WORDLIST,WORDLIST,docolon)
            dw HERE,ZERO,COMMA
            dw EXIT

;X GET-ORDER  ( -- wid1 .. widn n )
        head(GET_ORDER,GET-ORDER,docolon)
            dw WORDLISTS,STACKGET
            dw EXIT

;X SET-ORDER  ( wid1 .. widn n -- )
    head(SET_ORDER,SET-ORDER,docolon)
        DW ALLONES,OVER,EQUAL,qbranch,SETORDER1
        DW DROP,VOCAB_WORDLIST,FORTH_WORDLIST,lit,2
        DW WORDLISTS,STACKSET,EXIT

SETORDER1:
        DW WORDLISTS,STACKSET
        DW EXIT

;Z SAVE-ORDER    addr --            save wordlist order to addr
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
            dw ZERO,xdo
SAVEORDER2:
            dw CELLPLUS,DUP,TOR,STORE,RFROM
            dw xloop,SAVEORDER2
            dw branch,SAVEORDER3
SAVEORDER1:
            dw STORE
SAVEORDER3:
            dw DROP
            dw EXIT

;Z RESTORE-ORDER   addr --     restore wordlist ordor from addr
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
            dw ZERO,xdo
RESTOREORDER2:
            dw DUP,TOR,FETCH,RFROM,lit,1,CELLS,MINUS
            dw xloop,RESTOREORDER2
RESTOREORDER1:
            dw DROP,RFROM,SET_ORDER
            dw EXIT


;X >ORDER   wid --             add wordlist to the search order
        head(TOORDER,>ORDER,docolon)
            dw WORDLISTS,TOSTACK
            dw EXIT

;X SET-CURRENT   wid --        set wordlist as current wordlist
;    CURRENT !  ;
        head(SET_CURRENT,SET-CURRENT,docolon)
            dw CURRENT,STORE
            dw EXIT

;X GET-CURRENT   -- wid      get worlist id of current wordlist
;    CURRENT @  ;
        head(GET_CURRENT,GET-CURRENT,docolon)
            dw CURRENT,FETCH
            dw EXIT

;: COMPARE-NAME ( c-addr len  cstring -- c-addr len  cstring  f )
COMPARE_NAME:
        push bc
        exx

        pop de   ; cstring
        pop bc   ; len
        pop hl   ; c-addr
        push hl
        push bc
        push de

        ld a,(de)
        cp c
        jr nz, CMPNAME_NOT_EQ
        inc de   ; skip cstring count byte

        ld b,c
CMPNAME_LOOP:
        ld a,(de)
        cp (hl)
        jr nz,CMPNAME_NOT_EQ
        inc de
        inc hl
        djnz CMPNAME_LOOP

        exx
        jp tostrue

CMPNAME_NOT_EQ:
        exx
        jp tosfalse

;X FIND-NAME-IN   c-addr len wid   --  c-addr len 0       if not found
;                                      c-addr len nfa     if found
;   WID>NFA DUP 0= IF DROP 0 EXIT THEN
;   BEGIN                      -- a len nfa
;       COMPARE_NAME           -- a len nfa f
;       IF  EXIT
;       ELSE
;           NFA>LFA @ DUP      -- a len link
;       THEN
;   0= UNTIL                   -- a len nfa  OR  a len 0
;    ;
FIND_NAME_IN:
        call docolon
        DW WIDTONFA
        DW DUP,ZEROEQUAL,qbranch,FINDIN1
        DW DROP,FALSE,EXIT
FINDIN1:
        DW COMPARE_NAME
        DW qbranch,FINDIN2
        DW EXIT
FINDIN2:
        DW NFATOLFA,FETCH,DUP
        DW ZEROEQUAL,qbranch,FINDIN1
FINDIN3:
        DW EXIT


;: ?FIND-LOCALS    c-addr len      --  c-adddr len 0   if not found
;                                      c-adddr len nt  if found
;    LOCALS-WID  FIND-NAME-IN  ;
QFIND_LOCALS:
        call docolon
        DW LOCALS_WID,FIND_NAME_IN
        DW EXIT

;: FIND-NAME   c-addr len      -- 0   if not found
;                                 nt  if found
;    ?FIND-LOCALS DUP 0= IF
;       DROP
;       ' FIND-NAME-IN WORDLISTS STACK.UNTIL
;                         ( c-addr len 0       if not found )
;                         ( c-addr len nfa     if found )
;    THEN
;    NIP NIP ;
FIND_NAME:
        call docolon
        DW QFIND_LOCALS,DUP,ZEROEQUAL,qbranch,FINDNAME1
        DW DROP
        DW lit,FIND_NAME_IN,WORDLISTS,STACKBOUNDS,MAP_UNTIL

FINDNAME1:
        DW NIP,NIP
        DW EXIT

;X SEARCH-WORDLIST   c-addr u wid -- 0 | xt 1 | xt -1 
;\ search for name as string in wordlist
;    FIND-NAME-IN   ( c-addr len nfa|0 )
;    NIP NIP
;    DUP IF         ( nfa | 0 )
;       DUP NFA>CFA            -- nfa xt
;       SWAP IMMED?            -- xt iflag
;       0= 1 OR                -- xt 1/-1
;    THEN ;
; Find the definition identified by the string c-addr u in the word list
; identified by wid. If the definition is not found, return zero. If the
; definition is found, return its execution token xt and one (1) if the
; definition is immediate, minus-one (-1) otherwise.
    head(SEARCH_WORDLIST,SEARCH-WORDLIST,docolon)
        DW FIND_NAME_IN,NIP,NIP
        DW DUP,qbranch,SEARCHWID1
        DW DUP,NFATOCFA
        DW SWOP,IMMEDQ
        DW ZEROEQUAL,lit,1,OR
SEARCHWID1:
        DW EXIT


;C FIND   c-addr -- c-addr 0   if not found
;\                  xt  1      if immediate
;\                  xt -1      if "normal"
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

;C FORTH  --               add FORTH wordlist to wordlist order
;CREATE FORTH  FORTH-WORDLIST , DO-VOCABULARY
    head_vocab(FORTH,FORTH,docolon)
        dw FORTH_WORDLIST
        dw WORDLISTS,STACKSTORE
        dw EXIT

;Z SYSTEM  --             add SYSTEM wordlist to wordlist order
;CREATE SYSTEM  SYSTEM-WORDLIST , DO-VOCABULARY
    head_vocab(SYSTEM,SYSTEM,docolon)
        dw SYSTEM_WORDLIST
        dw WORDLISTS,STACKSTORE
        dw EXIT


;X VOCABULARY   "<spaces>name" --       create a new vocabulary
;  WORDLIST CREATE ,
;   VOC-LINK @ WID>NFA , 0 C,         link & `immed' field
;   HERE VOC-LINK @ WID>NFA!           new "latest" link
;   BL WORD C@ 1+ ALLOT         name field
;   docreate ,CF                code field
;   WORDLIST ,
;
;    DOES>  @ WORDLISTS TOSTACK
;  ;
    head(VOCABULARY,VOCABULARY,docolon)
        dw VOCAB_WORDLIST,WIDTONFA,COMMA,ZERO,CCOMMA
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

;X ALSO   --      duplicate vocabulary at top of wordlist order
; WORDLISTS STACK-DUP ;
    head(ALSO,ALSO,docolon)
        dw WORDLISTS,STACKDUP
        dw EXIT

;X PREVIOUS  --                      drop top of wordlist order
;  WORDLISTS STACK> DROP ;
    head(PREVIOUS,PREVIOUS,docolon)
        dw WORDLISTS,STACKFROM,DROP
        dw EXIT

;X DEFINITIONS  --     set top of wordlist order for definit'ns
;  WORDLISTS STACK@ SET-CURRENT ;
    head(DEFINITIONS,DEFINITIONS,docolon)
        dw WORDLISTS,STACKFETCH,SET_CURRENT
        dw EXIT

;X ONLY  --                                clear wordlist order
;   -1 SET-ORDER ;
    head(ONLY,ONLY,docolon)
        dw ALLONES,SET_ORDER
        dw EXIT

;Z VOCS  --                       list all vocabularies in dict
;   VOCLINK @ (WORDS) ;
    head(VOCS,VOCS,docolon)
        DW VOCAB_WORDLIST,XWORDS,EXIT

;: WORDS_16K ( -- )      list all words in search order
;   ['] (WORDS) WORDLISTS STACK-BOUNDS MAP ;
WORDS_16K:
        call docolon
        DW lit,XWORDS,WORDLISTS,STACKBOUNDS,MAP
        DW EXIT

;Z VLIST  --                  list all words in current context
;   CONTEXT (WORDS) ;
    head(VLIST,VLIST,docolon)
        DW CONTEXT,XWORDS
        DW EXIT

; ================================================

;X PICK   xu...x1 x0 u -- xu...x1 x0 xu     get u item in stack
;\ Remove u. Copy the xu to the top of the stack.
; An ambiguous
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

;X ROLL   xu xu-1 ... x0 u -- xu-1 ... x0 xu         roll stack 
;\ Remove u. Rotate u+1 items on the top of the stack.
;\ An ambiguous condition exists if there are less than u+2
;\ items on the stack before ROLL is executed.
;\ 2 ROLL is equivalent to ROT
;\ 1 ROLL is equivalent to SWAP and
;\ 0 ROLL is a null operation.
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
        inc de
        lddr
        exx

        pop hl  ; do nip and discard
roll_end:
        next


;X CASE   -- 0                  start a CASE..ENDCASE statement
; 0 >L ; IMMEDIATE
    immed(CASE,CASE,docolon)
        DW ZERO,TOL,EXIT

;X OF   dest? -- orig        add an stetement for CASE..ENDCASE
;     POSTPONE OVER POSTPONE =
;     POSTPONE IF POSTPONE DROP ; IMMEDIATE
    immed(OF,OF,docolon)
        DW lit,OVER,COMMAXT
        DW lit,EQUAL,COMMAXT
        DW IF
        DW lit,DROP,COMMAXT
        DW EXIT

;X ?OF   dest? -- orig           conditional stetement for CASE

;     POSTPONE IF POSTPONE DROP ; IMMEDIATE
    immed(QOF,?OF,docolon)
        DW IF
        DW lit,DROP,COMMAXT
        DW EXIT

;X ENDOF   orig --                        end a clause for CASE
; POSTPONE ELSE ; IMMEDIATE
;   ['] branch ,BRANCH   HERE DUP ,DEST  >L
;   POSTPONE THEN
;   ; IMMEDIATE      unconditional forward branch
    immed(ENDOF,ENDOF,docolon)
        DW lit,branch,COMMABRANCH
        DW HERE,DUP,COMMADEST,TOL
        DW THEN,EXIT

;Z ESAC   dest? --                   resolves branches for CASE
;     BEGIN L> ?DUP WHILE POSTPONE THEN REPEAT
;                                 resolve LEAVEs
;     ; IMMEDIATE
    immed(ESAC,ESAC,docolon)
ESAC1:
        DW LFROM,QDUP,qbranch,ESAC2
        DW THEN,branch,ESAC1

ESAC2:
        DW EXIT

;X ENDCASE   dest? --   resolves branches for CASE dropping TOS 
;     POSTPONE DROP
;     BEGIN L> ?DUP WHILE POSTPONE THEN REPEAT
;                                 resolve LEAVEs
;     ; IMMEDIATE
    immed(ENDCASE,ENDCASE,docolon)
        DW lit,DROP,COMMAXT
        DW ESAC,EXIT


;X BEGIN-STRUCTURE                       ( -- addr 0 ; -- size )
;   CREATE   HERE 0 0 ,                ( mark stack, lay dummy )
;   DOES> @  ;                              ( -- record length )
    head(BEGIN_STRUCTURE,BEGIN-STRUCTURE,docolon)
        DW CREATE,HERE,ZERO,ZERO,COMMA

        DW XDOES
        call dodoes
        DW FETCH
        DW EXIT


;X +FIELD ( # n -- #'  define a field with offset # and size n )
;   CREATE OVER , +
;   DOES> @ + ;  ( addr1 -- addr2 ; calculate address of field )
    head(PLUSFIELD,+FIELD,docolon)
        DW CREATE,OVER,COMMA,PLUS

        DW XDOES
        call dodoes
        DW FETCH,PLUS
        DW EXIT


;X FIELD: 
;   ALIGNED 1 CELLS +FIELD ;
    head(FIELDCOLON,FIELD:,docolon)
        DW CELL,PLUSFIELD
        DW EXIT


;X CFIELD: 1 CHARS +FIELD ;
    head(CFIELDCOLON,CFIELD:,docolon)
        DW lit,1,PLUSFIELD
        DW EXIT

;X END-STRUCTURE
;   SWAP ! ;
    head(END_STRUCTURE,END-STRUCTURE,docolon)
        DW SWOP,STORE
        DW EXIT

; BLOCK implementation ==========================

; BLOCKCTX structure
;   Each context struct is indexed to a 1024byte block buffer
;    SLICE ID (1 cell)
;    BLOCK number (1 cell)
;    BUFFER address (1 cell)
;    BLOCK update flag (1 cell)

DEFC BLOCKCTX_SIZE = 8
DEFC BLOCKCTX_NUM = 4

BLKCTXS:
        call docon
        DW blkctxs_ptr

BLKCTX_IDX:
        call docon
        DW blkctx_idx_ptr

BLKCTX_CURR:
        call docon
        DW blkctx_curr_ptr

;: /BLKCTX   ( -- ) initialise the block contexts
;    BLKCTXS BLKCTX# 0 DO   ( ctx[i] )
;       0xffff OVER BLKCTX>BLOCK !  ( ctx[i] )
;       0xffff OVER BLKCTX>SLICE !  ( ctx[i] )
;       0x0000 OVER BLKCTX>BUFFER !  ( ctx[i] )
;       0x0000 OVER BLKCTX>FLAGS !  ( ctx[i] )
;       BLOCKCTX_SIZE +             ( ctx[i+1 ]
;    LOOP
;    DROP  0 BLKCTX_IDX !   0 BLKCTX_CURR ! ;
SLASHBLKCTX:
        call docolon
        dw BLKCTXS,BLKCTXNUM,ZERO,xdo
SLASHBLKCTX1:
        dw lit,0xffff,OVER,BLKCTXTOBLOCK,STORE
        dw lit,0xffff,OVER,BLKCTXTOSLICE,STORE
        dw ZERO,OVER,BLKCTXTOBUFFER,STORE
        dw ZERO,OVER,BLKCTXTOFLAGS,STORE
        dw BLKCTXSIZE,PLUS
        dw xloop,SLASHBLKCTX1
        dw DROP
        dw ZERO,BLKCTX_IDX,STORE
        dw ZERO,BLKCTX_CURR,STORE
        dw EXIT

;: BLKCTX>SLICE  ( ctx -- a-addr' )  get address of slice-id 
;    ;
BLKCTXTOSLICE:
        jp ctx_next

;: BLKCTX>BLOCK  ( ctx -- a-addr' )  get address of BLOCK number
;    ;
BLKCTXTOBLOCK:
        jp ctx_plus_2

;: BLKCTX>BUFFER  ( ctx -- a-addr' )  get address of BUFFER
;    ;
BLKCTXTOBUFFER:
        jp ctx_plus_4

;: BLKCTX>FLAGS  ( ctx -- a-addr' )  get address of FLAGS
;    ;
BLKCTXTOFLAGS:
ctx_plus_6:
        inc bc
        inc bc
ctx_plus_4:
        inc bc
        inc bc
ctx_plus_2:
        inc bc
        inc bc
ctx_next:
        next

;: BLKCTX%  (  -- u )  size of stucture
BLKCTXSIZE:
        call docon
        dw BLOCKCTX_SIZE

;: BLKCTX#  ( -- u )  number of buffer structures
BLKCTXNUM:
        call docon
        dw BLOCKCTX_NUM


;: BLKFIRST      -- a-adrs      address of first block buffer
;   RAMTOP 0xFC00 AND 0x1000 - ;
BLKFIRST:
        call docolon
        dw RAMTOP,lit,0xFC00,AND,lit,0x1000,MINUS
        dw EXIT

;:  BLKCTX_NOT_INUSE?   ( ctx -- f )
;   DUP BLKCTX>BLOCK @ SWAP BLKCTX>SLICE @
;   BLK @  SLICE
;   D=  INVERT  ;
BLKCTX_NOT_INUSEQ:
        call docolon
        DW DUP,BLKCTXTOBLOCK,FETCH,SWOP,BLKCTXTOSLICE,FETCH
        DW BLK,FETCH,SLICE
        DW DEQUAL,INVERT
        DW EXIT

;: IDX>BLKCTX   ( -- ctx )
;      BLKCTXS   BLKCTX_IDX @
;      BLKCTX% * +  ( new-ctx )   ;
IDX_TO_BLKCTX:
        call docolon
        dw BLKCTXS
        dw BLKCTX_IDX,FETCH
        dw BLKCTXSIZE,STAR,PLUS
        dw EXIT

;: IDX>BUFFER   ( -- c-addr )
;   BLKCTX_IDX @  B/BLK *  BLKFIRST PLUS  ( buffer )  ;
IDX_TO_BUFFER:
        call docolon
        dw BLKCTX_IDX,FETCH
        dw B_BLK,STAR,BLKFIRST,PLUS
        dw EXIT

;: IDX++   ( -- )
;      BLKCTX_IDX @ 1+ BLKCTXNUM MOD  BLKCTX_IDX !    ;
IDXPLUSPLUS:
        call docolon
        dw BLKCTX_IDX,FETCH,ONEPLUS,BLKCTXNUM,MOD,BLKCTX_IDX,STORE
        dw EXIT

;: BLKCTX-NEXT  ( -- ctx )  increment buffer structure
;   BEGIN
;      IDX++
;      IDX>BLKCTX   ( ctx )
;   BLKCTX_NOT_INUSE?  UNTIL
;   IDX>BLKCTX   ( ctx )
;   DUP (FLUSH)
;   IDX>BUFFER    ( ctx buffer )
;   OVER BLKCTX>BUFFER !  ( new-ctx )
;   ;
BLKCTX_NEXT:
        call docolon
BLKCTX_NEXT1:
        dw IDXPLUSPLUS
        dw IDX_TO_BLKCTX
        dw BLKCTX_NOT_INUSEQ,qbranch,BLKCTX_NEXT1
        dw IDX_TO_BLKCTX
        dw DUP,XFLUSH
        dw IDX_TO_BUFFER
        dw OVER,BLKCTXTOBUFFER,STORE
        dw EXIT

;: BLKCTX-FIND   blk slice-id -- ctx    address of matching buffer, if exists, else 0
;    BLKCTXS BLKCTX# 0 DO   ( blk slice-id ctx[i] )
;       >R                ( blk slice-id ; r: ctx[i] )
;       2DUP              ( blk slice-id blk slice-id ; r: ctx[i] )
;       R@ BLKCTX>BLOCK @  ( blk slice-id blk slice-id blk[i] ; r: ctx[i] )
;       R@ BLKCTX>SLICE  @  ( blk slice-id blk slice-id blk[i] slice-id[i] ; r: ctx[i] )
;       D=  IF            ( blk slice-id ; r: ctx[i] )
;           2DROP R> UNLOOP EXIT
;       THEN
;       R> BLKCTX% + ( blk slice-id ctx[i+1] )
;    LOOP
;    2DROP DROP 0   ;
BLKCTX_FIND:
        call docolon
        dw BLKCTXS,BLKCTXNUM,ZERO,xdo
BLKCTXF1:
        dw TOR
        dw TWODUP
        dw RFETCH,BLKCTXTOBLOCK,FETCH
        dw RFETCH,BLKCTXTOSLICE,FETCH
        dw DEQUAL,qbranch,BLKCTXF2
        dw TWODROP,RFROM,UNLOOP
        dw EXIT
BLKCTXF2:
        dw RFROM,BLKCTXSIZE,PLUS
        dw xloop,BLKCTXF1
        dw TWODROP,DROP,FALSE
        dw EXIT


;: BLKCTX-GET   blk slice-id -- ctx    incr. buffer structure
;     2DUP BLKCTX-FIND ?DUP IF   ( blk slice-id ctx )
;         NIP NIP
;     ELSE                       ( blk slice-id )
;         BLKCTX-NEXT    ( blk slice-id ctx )
;         >R
;         R@ BLKCTX>SLICE !
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
        dw RFETCH,BLKCTXTOSLICE,STORE
        dw RFETCH,BLKCTXTOBLOCK,STORE
        dw RFETCH,BLKCTXTOFLAGS,lit,1,SWOP,STORE
        dw RFROM
        dw EXIT

;: BLKCTX-MAP   xt --     execute xt for each blkctx
;    BLKCTXS BLKCTX# 0 DO   ( xt ctx[i] )
;       >R                ( xt ; r: ctx[i] )
;       DUP R@ SWAP EXECUTE
;       R> BLKCTX% +      ( xt ctx[i+1] )
;    LOOP   2DROP  ;
BLKCTX_MAP:
        call docolon
        dw BLKCTXS,BLKCTXNUM,ZERO,xdo
BLKCTXMAP1:
        dw TOR
        dw DUP
        dw RFETCH,SWOP,EXECUTE
        dw RFROM,BLKCTXSIZE,PLUS
        dw xloop,BLKCTXMAP1
        dw TWODROP
        dw EXIT


SECTION data

blkctxs_ptr:
        DEFS 32  ;  4 * 4 words

blkctx_idx_ptr:
        DEFS 2

blkctx_curr_ptr:
        DEFS 2

SECTION code_16k


; DRIVE implementation ==========================

; DRIVECTX structure
;   Each context struct is drive device "driver"
;    XT READ sector (1 cell)
;    XT WRITE sector BLOCK number (1 cell)
;    XT CAPACITY number of sectores (1 cell)

DEFC DRIVECTX_SIZE = 8


;Z DRIVE>READ   drive-id -- a-addr'         address of  READ xt
    head_system(DRIVETOREAD,DRIVE.READ,docode)
        jp ctx_next

;Z DRIVE>WRITE   drive-id -- a-addr'        address of WRITE xt
    head_system(DRIVETOWRITE,DRIVE.WRITE,docode)
        jp ctx_plus_2

;Z DRIVE>CAPACITY   drive-id -- a-addr'     addr of CAPACITY xt
    head_system(DRIVETOCAPACITY,DRIVE.CAPACITY,docode)
        jp ctx_plus_4

;Z DRIVE%    -- u                        size of DRIVE stucture
    head_system(DRIVECTX,DRIVE%,docon)
        DW DRIVECTX_SIZE

;Z DRIVE:  ( xt-read xt-write xt-capacity    "ccc" -- )
;\          ( execution: -- drive-id )
;\ Create a named DRIVE structure
    head_system(DRIVECOLON,DRIVE:,docolon)
        DW CREATE
        DW ROT,COMMA,SWOP,COMMA,COMMA
        DW EXIT

; SLICE implementation ==========================

; SLICECTX structure
;   Each context struct is drive device "driver"
;    DRIVE-ID sector (1 cell)
;    LBA OFFSET  (2 cells)
;    LIMIT number of blocks (1 cell)

DEFC SLICECTX_SIZE = 8


;Z SLICE>DRIVE   slice-id -- a-addr'           addr of drive ID
    head_system(SLICETODRIVE,SLICE.DRIVE,docode)
        jp ctx_next

;Z SLICE>OFFSET   slice-id -- a-addr'        addr of LBA OFFSET
    head_system(SLICETOOFFSET,SLICE.OFFSET,docode)
        jp ctx_plus_2

;Z SLICE>LIMIT    slice-id -- a-addr'             addr of LIMIT
    head_system(SLICETOLIMIT,SLICE.LIMIT,docode)
        jp ctx_plus_6

;Z SLICE%   -- u                 size of SLICE context stucture
    head_system(SLICESIZE,SLICE%,docon)
        dw SLICECTX_SIZE


        DEFC SLICE_SECTORS = 8192*2
;Z RESLICE   n slice-id --     set n*8MB LBA offset to slice-id 
;  >R SLICE_SECTORS UM*                ( lba-offset ; slice-id )
;  2DUP R@ SLICE>DRIVE @ DRIVE>CAPACITY @EXECUTE  ( lba-offset lba-offset capacity )
;  2OVER 2OVER  D<  IF                            ( lba-offset lba-offset capacity )
;  2SWAP SLICE_SECTORS M+  DMIN                   ( lba-offset lba-end )
;  2OVER  D-  D2/  D>S                            ( lba-offset limit )
;  R@ SLICE>LIMIT !
;  R> SLICE>OFFSET 2!
;  ELSE  -257 THROW THEN   ;
    head_system(RESLICE,RESLICE,docolon)
        DW FLUSH
        DW TOR,lit,SLICE_SECTORS,UMSTAR
        DW TWODUP,RFETCH,SLICETODRIVE,FETCH,DRIVETOCAPACITY,FETCHEXECUTE
        DW TWOOVER,TWOOVER,DLESS,qbranch,SLICE1
        DW TWOSWAP,lit,SLICE_SECTORS,MPLUS
        DW DMIN
        DW TWOOVER,DMINUS,DTWOSLASH,DROP
        DW RFETCH,SLICETOLIMIT,STORE
        DW RFROM,SLICETOOFFSET,TWOSTORE,EXIT
SLICE1:
        DW lit,-257,THROW
        DW EXIT

;Z /SLICE   drive-id n c-addr --       initialise slice context
;      TUCK TWOSWAP !   ( n slice-id )
;      RESLICE   ;
    head_system(SLASHSLICE,/SLICE,docolon)
        DW TUCK,TWOSWAP,STORE
        DW RESLICE
        DW EXIT

;Z SLICE:   drive-id n "ccc" --           create named slice id
;           execution:  -- slice-id )
    head_system(SLICECOLON,SLICE:,docolon)
        DW CREATE,HERE
        DW SLICESIZE,ALLOT
        DW SLASHSLICE
        DW EXIT

;Z SLICE   -- slice-id          get currently selected slice id
    head_system(SLICE,SLICE,docolon)
        DW SLICE_ID,FETCH,EXIT

;Z SELECT   slice-id --                 set slice id to current
    head_system(SELECT,SELECT,docolon)
        DW SLICE_ID,STORE,EXIT

;Z SUBSLICE   blk len slice-id --     reduce slice to len blocks from blk
;     DUP >R SLICE>LIMIT !        ( blk ; r: slice-id )
;     R@  BLK2LBA          ( d ; r: slice-id )
;     R> SLICE>OFFSET 2!  ;
    head_system(SUBSLICE,SUBSLICE,docolon)
       DW DUP,TOR,SLICETOLIMIT,STORE
       DW RFETCH,BLK2LBA
       DW RFROM,SLICETOOFFSET,TWOSTORE
       DW EXIT


; SLICE/DRIVE helpers ==========================


SECTRDVEC:
        call docolon
        dw SLICE,SLICETODRIVE,FETCH,DRIVETOREAD
        dw EXIT

SECTWRVEC:
        call docolon
        dw SLICE,SLICETODRIVE,FETCH,DRIVETOWRITE
        dw EXIT

BLKLIMIT:
        call docolon
        dw SLICE,SLICETOLIMIT,FETCH
        dw EXIT

;: BLK2LBA   ( blk slice-id -- LBA-L LBA-H )
;   SLICE>OFFSET  >R
;   S>D D2*   ( LBA-L LBA-H )
;   R>       ( LBA-L LBA-H  slice-to-offset )
;   2@ D+ ;  ( LBA-L' LBA-H' )
BLK2LBA:
        call docolon
        DW SLICETOOFFSET,TOR
        dw STOD,DTWOSTAR
        dw RFROM,TWOFETCH,DPLUS
        dw EXIT


EXTERN cflash_init
EXTERN cflash_identify
;Z /CFLASH   -- drive-id    initialise the Compact Flash driver
;   clash_init CALL    ( ior )
;   IF
;       ." CFLASH OK"
;       CF-DRIVE-ID
;   ELSE ." NO CFLASH" 0 THEN ;
    head_system(SLASHCFLASH,/CFLASH,docolon)
        dw lit,cflash_init,CALL
        dw qbranch,SLASHCFLASH1
        dw XSQUOTE
        db 9,"NO CFLASH"
        dw TYPE,CR
        dw ZERO
        dw EXIT

SLASHCFLASH1:
        dw XSQUOTE
        db 11,"CFLASH OK ("
        dw TYPE,CF_CAPACITY,DTWOSLASH,DDOT
        dw XSQUOTE
        db 7,"blocks)"
        dw TYPE,CR
        dw CF_DRIVE_ID
        dw EXIT


;Z CF-CAPACITY   -- d    Fetch Compact Flash capacity (sectors)
    head_system(CF_CAPACITY,CF-CAPACITY,docolon)
        dw lit,512,HERE,PLUS,DUP,TOR,lit,cflash_identify,CALL
;        dw RFETCH,lit,256,MEMDUMP
        dw RFETCH,lit,120,PLUS,FETCH      ; low word of max LBA
        dw RFROM,lit,122,PLUS,FETCH       ; high word of max LBA
        DW EXIT


SECTION data
CF_CAPACITY_DATA:
        DEFS 4

SECTION code_16k

EXTERN cflash_read_sector
;Z CF-SECTOR-READ   lba-l lba-h adrs -- ior
;\                             Compact Flash read sector at LBA
; Reads the sector from the Compact Flash card into memory
; address found at 'adrs'. 'slice-id' and 'blk' are the disk
; and block numbers respectively
;   clash_read_sector CALL ;
    head_system(CF_SECTOR_READ,CF-SECTOR-READ,docolon)
        dw lit,cflash_read_sector,CALL
        dw EXIT

EXTERN cflash_write_sector
;Z CF-SECTOR-WRITE   lba-l lba-h adrs -- ior )
;\                            Compact Flash write sector at LBA
;   clash_write_sector CALL ;
    head_system(CF_SECTOR_WRITE,CF-SECTOR-WRITE,docolon)
        dw lit,cflash_write_sector,CALL
        dw EXIT

;Z CF-DRIVE-ID   -- drive-id          drive-id of compact flash
    head_system(CF_DRIVE_ID,CF-DRIVE-ID,docreate)
        dw CF_SECTOR_READ
        dw CF_SECTOR_WRITE
        dw CF_CAPACITY


;Z CF-SLICE-ID   -- slice-id          slice-id of compact flash
    head_system(CF_SLICE_ID,CF-SLICE-ID,docon)
        DW CFLASH_SLICE_CTX


SECTION data

CFLASH_SLICE_CTX:
        DEFS SLICECTX_SIZE


SECTION code_16k


;: BLOCK-READ  ( blk slice-id adrs -- )  Compact Flash read BLK and SLICE-ID
; Reads the block from the Compact Flash card into memory
; address found at 'adrs'. 'slice-id' and 'blk' are the disk
; and block numbers respectively
;     >R BLK2LBA 2DUP R@   ( LBA-L LBA-H LBA-L LBA-H adrs ;  R: adrs )
;     CF-SECTOR-READ       ( LBA-L LBA-H ;  R: adrs )
;     1 M+                 ( LBA-L' LBA-H' ;  R: adrs )
;     R>  512 +            ( LBA-L' LBA-H' adrs' )
;     CF-SECTOR-READ  THROW     ( )
;     EXIT
BLOCK_READ:
        call docolon
        dw TOR,BLK2LBA,TWODUP,RFETCH    ; convert block to LBA
        dw SECTRDVEC,FETCHEXECUTE,THROW
        dw lit,1,MPLUS
        dw RFROM,lit,512,PLUS
        dw SECTRDVEC,FETCHEXECUTE,THROW
        dw EXIT

;: BLOCK-WRITE  ( blk slice-id adrs -- )  Compact Flash write BLK and SLICE-ID
; Writes the block to the Compact Flash card from memory
; address found at 'adrs'. 'slice-id' and 'blk' are the disk
; and block numbers respectively
BLOCK_WRITE:
        call docolon
        dw TOR,BLK2LBA,TWODUP,RFETCH    ; convert block to LBA
        dw SECTWRVEC,FETCHEXECUTE,THROW
        dw SWOP,ONEPLUS,SWOP
        dw RFROM,lit,512,PLUS
        dw SECTWRVEC,FETCHEXECUTE,THROW
        dw EXIT

;: BLOCK-READWRITE    ( ctx f -- )  read or write block
;                              f = 0 read, f = -1 write
;     >R >R
;     R@ BLKCTX>BLOCK @
;     R@ BLKCTX>SLICE @
;     R@ BLKCTX>BUFFER @
;     0 R> BLKCTX>FLAGS !
;     R>      ( slice-id blk adrs f )
;     IF BLOCK-WRITE ELSE BLOCK-READ ;
BLOCK_READWRITE:
        call docolon
        dw TOR,TOR
        dw RFETCH,BLKCTXTOBLOCK,FETCH
        dw RFETCH,BLKCTXTOSLICE,FETCH
        dw RFETCH,BLKCTXTOBUFFER,FETCH
        dw ZERO,RFROM,BLKCTXTOFLAGS,STORE
        dw RFROM
        dw qbranch,BLOCK_READWRITE1
        dw BLOCK_WRITE,branch,BLOCK_READWRITE2
BLOCK_READWRITE1:
        dw BLOCK_READ
BLOCK_READWRITE2:
        dw EXIT

;: (BUFFER)      n -- ctx    get buffer context
;     DUP BLKLIMIT U< IF
;        SLICE
;        BLKCTX-GET
;        DUP BLKCTX_CURR !
;     ELSE  -35 THROW THEN ;
XBUFFER:
        call docolon
        dw DUP,BLKLIMIT,ULESS,qbranch,XBUFFER1
        dw SLICE
        dw BLKCTX_GET
        dw DUP,BLKCTX_CURR,STORE
        dw EXIT
XBUFFER1:
        dw lit,-35,THROW
        dw EXIT


;C BUFFER   n -- addr                       push buffer address
;     (BUFFER)       ( ctx )
;     BLKCTX>BUFFER @  ;
    head(BUFFER,BUFFER,docolon)
        dw XBUFFER
        dw BLKCTXTOBUFFER,FETCH
        dw EXIT

;C BLOCK  n -- addr                    load block
;        (BUFFER)     ( ctx )
;        DUP BLKCTX>FLAGS @ 1 = IF  ( ctx )
;            DUP 0 BLOCK-READWRITE
;        THEN
;        BLKCTX>BUFFER @
;        ;
    head(BLOCK,BLOCK,docolon)
        dw XBUFFER
        dw DUP,BLKCTXTOFLAGS,FETCH,lit,1,EQUAL,qbranch,BLOCK1
        dw DUP,ZERO,BLOCK_READWRITE
BLOCK1:
        dw BLKCTXTOBUFFER,FETCH
        dw EXIT

;C UPDATE  --                    mark last used block to update
;     BLKCTX_CURR @ ?DUP IF
;        BLKCTX>FLAGS -1 SWAP !
;     THEN ;
    head(UPDATE,UPDATE,docolon)
        dw BLKCTX_CURR,FETCH,QDUP,qbranch,UPDATE1
        dw BLKCTXTOFLAGS,lit,0xffff,SWOP,STORE
UPDATE1:
        dw EXIT

;C UPDATED?   blk -- f                  are any blocks updated?
;     SLICE BLKCTX-FIND DUP IF
;         BLKCTX>FLAGS @ -1 =
;     THEN ;
    head(UPDATEDQ,UPDATED?,docolon)
        dw SLICE,BLKCTX_FIND,DUP,qbranch,UPDATEDQ1
        dw BLKCTXTOFLAGS,FETCH,ALLONES,EQUAL
UPDATEDQ1:
        dw EXIT

;: (FLUSH)  ( ctx -- )  flush blkctx to disk
;    DUP BLKCTX>FLAGS @ -1 = IF    ( ctx )
;      DUP -1 BLOCK-READWRITE        ( ctx )
;      BLKCTX>FLAGS 0 SWAP !
;    ELSE DROP
;    THEN ;
XFLUSH:
        call docolon
        dw DUP,BLKCTXTOFLAGS,FETCH,TRUE,EQUAL,qbranch,FLUSH1
        dw DUP,TRUE,BLOCK_READWRITE
        dw BLKCTXTOFLAGS,ZERO,SWOP,STORE
        dw EXIT
FLUSH1:
        dw DROP,EXIT

;C EMPTY-BUFFERS   --        release blocks, don't save to disk
    head(EMPTY_BUFFERS,EMPTY-BUFFERS,docolon)
        dw SLASHBLKCTX
        dw EXIT

;C SAVE-BUFFERS   --                save updated blocks to disk
;     ' (FLUSH) BLKCTX-MAP  ;
    head(SAVE_BUFFERS,SAVE-BUFFERS,docolon)
        dw lit,XFLUSH,BLKCTX_MAP
        dw EXIT

;C FLUSH   --                  flush all updated blocks to disk
;    SAVE-BUFFERS /BLKCTX  ;
    head(FLUSH,FLUSH,docolon)
        dw SAVE_BUFFERS, SLASHBLKCTX
        dw EXIT

;: \-BLK   ( blk code path of the \ word )
;    >in @  b/line 1- +  b/line negate and  >in !
XBACKSLASH_BLK:
    call docolon
    DW TOIN,FETCH,C_L,ONEMINUS,PLUS
    DW C_L,NEGATE,AND,TOIN,STORE
    DW EXIT

;: LOAD-REFILL  ( -- flag )
;  BLK @  BLKLIMIT  U< IF
;    1 BLK +!
;    BLK @ BLOCK  B/BLK   'SOURCE 2!  0 >IN !
;    TRUE  
;  ELSE
;    FALSE
;  THEN   ;
LOAD_REFILL:
        call docolon
        dw BLK,FETCH,BLKLIMIT,ULESS,qbranch,LOAD_REFILL1
        dw lit,1,BLK,PLUSSTORE
        dw BLK,FETCH,BLOCK,B_BLK,TICKSOURCE,TWOSTORE
        dw ZERO,TOIN,STORE
        dw TRUE,EXIT

LOAD_REFILL1:
        dw FALSE,EXIT


;C LOAD   blk --                        load and evaluate block
;    SAVE-INPUT N>R
;    0 'SOURCE-ID !
;    DUP BLK !
;    BLOCK B/BLK 'SOURCE 2!  0 >IN !
;    INTERPRET
;    NR> RESTORE-INPUT DROP  ;
    head(LOAD,LOAD,docolon)
        dw SAVE_INPUT,NTOR
        dw DUP,BLK,STORE
        dw ZERO,TICKSOURCE_ID,STORE
        dw BLOCK,B_BLK,TICKSOURCE,TWOSTORE
        dw ZERO,TOIN,STORE
        dw INTERPRET
        dw NRFROM,RESTORE_INPUT,DROP
        dw EXIT

;C +LOAD   n --                   load and evaluate block BLK+n
;     BLK @ + LOAD  ;
    head(PLUSLOAD,+LOAD,docolon)
        dw BLK,FETCH,PLUS,LOAD
        dw EXIT

;C THRU   n1 n2 --            load and evaluate blocks n1 to n2
;   1+ SWAP
;   ?DO  I LOAD  LOOP ;

    head(THRU,THRU,docolon)
        dw ONEPLUS,SWOP
        dw TWODUP,EQUAL,qbranch,THRU1
        dw TWODROP,EXIT
THRU1:
        dw xdo
THRU2:
        dw II,LOAD,xloop,THRU2
        dw EXIT

;C +THRU   n1 n2 --                load blocks BLK+n1 to BLK+n2
;     OVER - 1+ SWAP BLK @ + SWAP (LOAD) ;
    head(PLUSTHRU,+THRU,docolon)
        dw BLK,FETCH,PLUS,SWOP
        dw BLK,FETCH,PLUS,SWOP
        dw THRU
        dw EXIT

;C COPY   n1 n2 --                          copy block n1 to n2
;   SWAP BLOCK  ( n2 blk1 )
;   SWAP BLOCK  ( blk1 blk2 )
;   B/BLK MOVE UPDATE ;
    head(COPY,COPY,docolon)
        dw SWOP,BLOCK,SWOP,BLOCK,B_BLK,MOVE,UPDATE
        dw EXIT


dnl ;: -->       \ -- ; LOAD NEXT screen
dnl ;   blk @ 0= blk_blk0 ?throw
dnl ;   refill drop   ; immediate
dnl     immed(LOADNEXT,``-->'',docolon)
dnl         DW BLK,FETCH,ZEROEQUAL,qbranch,LOADNEXT1
dnl         DW lit,-35,THROW
dnl LOADNEXT1:
dnl         DW REFILL,DROP
dnl 
dnl ;Z ;S         \ -- ; terminate loading of current screen
dnl ;  source nip >in !   ;
dnl     head(SEMICOLONS,``;S'',docolon)
dnl         DW SOURCE,NIP,TOIN,STORE
dnl         DW EXIT

; BLKFILE implementation =====================================

; BLKFCTX structure
;   Each context struct is block file
;    OFFSET  (1 cell)
;    BLK  (1 cell)
;    ORIGIN starting blk    (1 cell)
;    SLICE  limit BLKF to slice-id (SLICE% bytes)

DEFC BLKFCTX_SIZE = 6 + SLICECTX_SIZE


;Z BLKF.OFFSET   blkfile-id -- a-addr'       addr of current blk
    head_system(BLKFDOTOFFSET,BLKF.OFFSET,docode)
        jp ctx_next

;Z BLKF.BLK   blkfile-id -- a-addr'   addr of current offs.
    head_system(BLKFDOTBLK,BLKF.BLK,docode)
        jp ctx_plus_2

;Z BLKF.ORIGIN    blkfile-id -- a-addr'       addr of origin blk
    head_system(BLKFDOTORIGIN,BLKF.ORIGIN,docode)
        jp ctx_plus_4

;Z BLKF.SLICE    blkfile-id -- a-addr'       addr of slice
    head_system(BLKFDOTSLICE,BLKF.SLICE,docode)
        jp ctx_plus_6

;Z BLKF%   -- u              size of BLKFILE context stucture
    head_system(BLKFSIZE,BLKF%,docon)
        dw BLKFCTX_SIZE

;VARIABLE    current-blkfile-id
CURRENT_BLKFILE_ID:
        call docon
        DW current_blkfile_id

SECTION data

current_blkfile_id: DS 2

blkf_rw_flag: DS 2

SECTION code_16k

;Z /BLKF   ( blk blkfile-id -- )
;   2DUP BLKF.ORIGIN !        ( blk blkfile-id )
;   SWAP OVER BLKF.BLK !      ( blkfile-id )
;   0 OVER BLKF.OFFSET !      ( blkfile-id )
;   SLICE OVER BLKF.SLICE SLICE% MOVE  ( blkfile-id )
;   DROP ;
    head_system(SLASHBLKF,``/BLKF'',docolon)
        DW TWODUP,BLKFDOTORIGIN,STORE
        DW SWOP,OVER,BLKFDOTBLK,STORE
        DW ZERO,OVER,BLKFDOTOFFSET,STORE
        DW SLICE,OVER,BLKFDOTSLICE,SLICESIZE,MOVE
        DW DROP,EXIT

;Z ?SET-BLKFILE  ( blkfile-id -- )
;   current-blkfile-id @      ( blkf-id curr-blkf-id )
;   ?DUP IF  OVER  <> IF
;         FLUSH
;      THEN
;   THEN
;   current-blkfile-id !  ;
QSET_BLKFILE:
        call docolon
        DW CURRENT_BLKFILE_ID,FETCH
        DW QDUP,qbranch,QSET_BLKFILE1
        DW OVER,NOTEQUAL,qbranch,QSET_BLKFILE1
        DW FLUSH
QSET_BLKFILE1:
        DW CURRENT_BLKFILE_ID,STORE
        DW EXIT

;Z BLKF-FLUSH   ( -- )
;   0 ?SET-BLKFILE    ;
    head_system(BLKF_FLUSH,BLKF-FLUSH,docolon)
        DW ZERO,QSET_BLKFILE
        DW EXIT

;: bytes-to-read  ( u blkfile-id -- u' )   
;   BLKF.OFFSET @  B/BLK  SWAP -    \ bytes left in blk
;   2DUP U>   IF SWAP THEN DROP  ;  \ bytes to read -- get MIN
BYTES_TO_READ:
        call docolon
        DW FETCH,B_BLK,SWOP,MINUS
        DW TWODUP,UGREATER,qbranch,BYTES_TO_READ1
        DW SWOP
BYTES_TO_READ1:
        DW DROP
        DW EXIT

;Z BLKF>BUFFERIDX  ( blkfile-id -- c-addr )   -- get buffer index of blkfile
;   SLICE >R DUP BLK.SLICE  SELECT
;   DUP BLKF.BLK @ BLOCK
;   SWAP BLKF.OFFSET @ + 
;   R> SELECT    ;
    head_system(BLKFTOBUFFERIDX,BLKF>BUFFERIDX,docolon)
        DW SLICE,TOR,DUP,BLKFDOTSLICE,SELECT
        DW DUP,CELLPLUS,FETCH,BLOCK
        DW SWOP,FETCH,PLUS
        DW RFROM,SELECT
        DW EXIT

;Z BLKF>POSITION@ ( blkfile -- d )
;    DUP  BLKF.BLK @  OVER BLKF.ORIGIN @  -   1024 M*   ( blkfile-id d )
;    ROT  BLKF.OFFSET @   M+  ;
    head_system(BLKFTOPOSITIONFETCH,BLKF>POSITION@,docolon)
        DW DUP,CELLPLUS,FETCH,OVER,BLKFDOTORIGIN,FETCH,MINUS,B_BLK,MSTAR
        DW ROT,FETCH,MPLUS
        DW EXIT

;Z BLKF>POSITION!  ( d blkfile -- )
;    >R 1024  FM/MOD   (  offset blk   r: blkfile-id )
;    R@  BLKF.ORIGIN @ +
;    R@  BLKF.BLK !
;    R>  BLKF.OFFSET !  ;
    head_system(BLKFTOPOSITIONSTOR,BLKF>POSITION!,docolon)
        DW TOR,B_BLK,FMSLASHMOD
        DW RFETCH,BLKFDOTORIGIN,FETCH,PLUS
        DW RFETCH,CELLPLUS,STORE
        DW RFROM,STORE
        DW EXIT

;: blkf>offset+  ( n blkfile -- )
;\                where n is assumed to be <= 1024
;   TUCK BLKF.OFFSET @  +   ( blkfile-id offset' )
;   DUP 1023 > IF   ( blkfile-id offset' )
;      OVER BLKF.BLK   1 SWAP +!
;      1023 AND
;   THEN
;   SWAP BLKF.OFFSET !    ;
BLKFTOOFFSETPLUS:
        call docolon
        DW TUCK,FETCH,PLUS
        DW DUP,lit,1023,GREATER,qbranch,blkftooffsetplus1
        DW OVER,CELLPLUS,lit,1,SWOP,PLUSSTORE
        DW lit,1023,AND

blkftooffsetplus1:
        DW SWOP,STORE
        DW EXIT

;: incr-index  ( c-addr u n -- c-addr' u' )
;   TUCK 2>R  +
;   2R>  -   ;
INCR_INDEX:
        call docolon
        DW TUCK,TWOTOR,PLUS
        DW TWORFROM,MINUS
        DW EXIT

BLKF_RW_FLAG:
        call docon
        DW blkf_rw_flag

;: ((BLKF-DOCHARS))   ( c-addr u blkfile-id -- c-addr' u' )   get/put chars from blkfile stream
;   >R 2DUP R>              ( c-addr u c-addr u blkfile-id )
;   DUP >R blkf>bufferidx   ( c-addr u c-addr u dest   r: blkfile-id )
;   BLKF-RW-FLAG @ IF
;      SWAP                    ( c-addr u c-addr dest u   r: blkfile-id )
;      UPDATE
;   ELSE
;      -ROT                    ( c-addr u src c-addr u   r: blkfile-id )
;   THEN
;   R@  bytes-to-read       ( c-addr u src dest bytes   r: blkfile-id )
;   DUP >R                  ( c-addr u src dest bytes   r: blkfile-id bytes )
;   MOVE                    ( c-addr u                r: blkfile-id bytes )
;   R@  incr-index          ( c-addr' u'              r: blkfile-id bytes )
;   R> R> blkf>offset+  ;
XXBLKF_DOCHARS:
        call docolon
        DW TOR,TWODUP,RFROM
        DW DUP,TOR,BLKFTOBUFFERIDX
        DW BLKF_RW_FLAG,FETCH,qbranch,XXBLKF_DOCHARS1
        DW SWOP
        DW UPDATE
        DW branch,XXBLKF_DOCHARS2

XXBLKF_DOCHARS1:
        DW ROT,ROT

XXBLKF_DOCHARS2:
        DW RFETCH,BYTES_TO_READ
        DW DUP,TOR
        DW MOVE
        DW RFETCH,INCR_INDEX
        DW RFROM,RFROM,BLKFTOOFFSETPLUS
        DW EXIT


;: (BLKF-DOCHARS)   ( c-addr u blkfile-id -- c-addr' u' )   get/put u chars from blkfile stream
;   BEGIN
;   OVER WHILE
;      DUP >R                ( c-addr u blkfile-id  r: blkfile-id )
;      ((BLKF-DOCHARS))     ( c-addr' u'  r: blkfile-id  )
;      R>                    ( c-addr' u' blkfile-id  )
;   REPEAT   DROP   ;
XBLKF_DOCHARS:
         call docolon
XBLKF_DOCHARS1:
        DW OVER,qbranch,XBLKF_DOCHARS2

        DW DUP,TOR
        DW XXBLKF_DOCHARS
        DW RFROM
        DW branch,XBLKF_DOCHARS1

XBLKF_DOCHARS2:
        DW DROP
        DW EXIT



;Z BLKF-DOCHARS   ( c-addr u blkfile-id f -- u )   put u chars to blkfile stream
;   BLKF-RW-FLAG !
;   DUP  ?SET-BLKFILE
;   SLICE >R  DUP  BLKF.SLICE SELECT
;   OVER >R              ( c-addr u blkfile-id    r: slice-id u )
;   (BLKF-DOCHARS)      ( c-addr' u'   r: slice-id u )
;   NIP  R>   SWAP -          \ return characters read
;   R> SELECT  ;
BLKF_DOCHARS:
        call docolon
        DW BLKF_RW_FLAG,STORE
        DW DUP,QSET_BLKFILE
        DW SAVE_INPUT,NTOR,DUP,BLKFDOTSLICE,SELECT
        DW OVER,TOR
        DW XBLKF_DOCHARS
        DW NIP,RFROM,SWOP,MINUS
        DW NRFROM,RESTORE_INPUT,DROP
        DW EXIT

;Z BLKF-PUTCHARS   ( c-addr u blkfile-id -- u )   put u chars to blkfile stream
;    TRUE    BLKF-DOCHARS-DO ;
    head_system(BLKF_PUTCHARS,BLKF-PUTCHARS,docolon)
        DW TRUE
        DW BLKF_DOCHARS
        DW EXIT

;Z BLKF-GETCHARS   ( c-addr u blkfile-id -- u )   get u chars to blkfile stream
;    FALSE    BLKF-DOCHARS-DO ;
    head_system(BLKF_GETCHARS,BLKF-GETCHARS,docolon)
        DW FALSE
        DW BLKF_DOCHARS
        DW EXIT


; RC2014 EXTENSION (SCREENS) ====================

;: (BLOCK)                 -- a-addr  load block in SCR
;     SCR @ BLOCK ;
XBLOCK:
        call docolon
        dw SCR,FETCH,BLOCK
        dw EXIT

;Z (LINE)   line# -- c-addr           address of line# in block
;     C/L * (BLOCK) + ;
    head_system(XLINE,(LINE),docolon)
        dw C_L,STAR,XBLOCK,PLUS
        dw EXIT

;Z TYPE$   c-addr +n --       type line of printable characters
;   ?DUP IF
;     OVER + SWAP DO I C@
;       DUP PRINTABLE? INVERT IF DROP [CHAR] . THEN EMIT
;     LOOP
;   ELSE DROP THEN ;
    head_system(TYPESTRING,TYPE$,docolon)
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

;Z LL   line# --                                List Line line#
;     (LINE) C/L TYPE CR ;
    head_system(LL,LL,docolon)
        dw XLINE,C_L,TYPESTRING,CR
        dw EXIT


;Z (LIST)   --                          runtime for list screen
;       L/B 0 DO I 2 .R SPACE I LL LOOP    ;
    head_system(XLIST,(LIST),docolon)
        dw L_B,ZERO,xdo
XLIST1:
        dw II,lit,2,DOTR,SPACE,II,LL,xloop,XLIST1
        dw EXIT

;C LIST   n --                               list screen number
;     DUP SCR ! ." SCR # " . CR (LIST) ;
    head(LIST,LIST,docolon)
        dw DUP,SCR,STORE,XSQUOTE
        db 6,"SCR # "
        dw TYPE,DOT,CR
        dw XLIST
        dw EXIT

;C INDEX   from to --           print first line of each screen
;     BLK @ >R
;     CR 1+ SWAP DO I 2 .R SPACE I DUP SCR ! BLOCK DROP 0 LL LOOP
;     R> BLK !   ;
    head(INDEX,INDEX,docolon)
        dw CR,ONEPLUS,SWOP,xdo
INDEX1:
        dw II,lit,5,DOTR,SPACE
        dw II,DUP,SCR,STORE,BLOCK,DROP
        dw ZERO,LL,xloop,INDEX1
        dw EXIT

;C WIPE   n --                                    erase block n 
;    BUFFER 1024 BLANK
;    UPDATE ;
    head(WIPE,WIPE,docolon)
        dw BUFFER,lit,1024,BLANK
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
;              FORTH, VOCS, SYSTEM.
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

;Z >SNAPSHOT   addr --           save internal snapshot to addr
;        DUP #SNAPSHOT ERASE
;        DUP SNAPSHOT.USER U0 SWAP 128 MOVE
;        DUP SNAPSHOT.ORDER SAVE-ORDER
;        DROP   ;
dnl    head_system(TOSNAPSHOT,>SNAPSHOT,docolon)
TOSNAPSHOT:
        call docolon
        dw DUP,NUMSNAPSHOT,ERASE
        dw DUP,SNAPSHOTDOTUSER,U0,SWOP,lit,128,MOVE
        dw DUP,SNAPSHOTDOTORDER,SAVE_ORDER
        dw DUP,SNAPSHOTDOTLINKS
        dw    INTVEC,FETCH,OVER,STORE,CELLPLUS
        dw    NMIVEC,FETCH,OVER,STORE,CELLPLUS
        dw    VOCAB_WORDLIST,FETCH,OVER,STORE,CELLPLUS
        dw    FORTH_WORDLIST,FETCH,OVER,STORE,CELLPLUS
        dw    SYSTEM_WORDLIST,FETCH,OVER,STORE,CELLPLUS
        dw    DROP
        dw DROP
        dw EXIT

; USER space index to restore
defc SNAPSHOT_RST_START = 24
defc SNAPSHOT_RST_LEN = 128-24

;Z SNAPSHOT>   addr --               restore snapshot from addr
;        DUP SNAPSHOT.USER SNAPSHOT_RST_START +
;           U0 SNAPSHOT_RST_START +   SNAPSHOT_RST_LEN MOVE
;        DUP SNAPSHOT.ORDER RESTORE-ORDER
;        DUP SNAPSHOT.USER 8 + @ DP !
;        DROP   ;
dnl    head_system(SNAPSHOTFROM,SNAPSHOT>,docolon)
SNAPSHOTFROM:
        call docolon
        dw SAVE_INPUT,NTOR
        dw DUP,SNAPSHOTDOTUSER,U0,lit,128,MOVE
        dw NRFROM,RESTORE_INPUT,DROP
        dw DUP,SNAPSHOTDOTORDER,RESTORE_ORDER
        dw DUP,SNAPSHOTDOTLINKS
        dw    DUP,FETCH,INTVEC,STORE,CELLPLUS
        dw    DUP,FETCH,NMIVEC,STORE,CELLPLUS
        dw    DUP,FETCH,VOCAB_WORDLIST,STORE,CELLPLUS
        dw    DUP,FETCH,FORTH_WORDLIST,STORE,CELLPLUS
        dw    DUP,FETCH,SYSTEM_WORDLIST,STORE,CELLPLUS
        dw    DROP
        dw DROP
        dw EXIT

;X MARKER   "name" --                 create marker forget word
;    HERE 32 + DUP R> >SNAPSHOT  ( temp copy ; save snapshot pointer )
;    CREATE
;        R> HERE #SNAPSHOT MOVE
;        #SNAPSHOT CHARS ALLOT
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
        dw EXIT

;Z BSAVE   c-addr u blk --            save memory range to disk
;     internal-blkfid TUCK /BLKF
;     BLKF-PUTCHARS
;     BLKF-FLUSH  ;
    head(BSAVE,BSAVE,docolon)
        dw lit,internal_blkfid,TUCK,SLASHBLKF
        dw BLKF_PUTCHARS
        dw BLKF_FLUSH
        dw EXIT


;Z BLOAD   c-addr u blk --          load memory range from disk
;     internal-blkfid TUCK /BLKF
;     BLKF-GETCHARS ;
    head(BLOAD,BLOAD,docolon)
        dw lit,internal_blkfid,TUCK,SLASHBLKF
        dw BLKF_PUTCHARS
        dw EXIT

SECTION data

internal_blkfid:
        DEFS BLKFCTX_SIZE

SECTION code_16k



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
;    BUFFER     ( c-addr u buffer -- )
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

;Z SAVE   blk --                    save internal state to disk
;    internal-blkfid /BLKF
;    enddict   DP @ enddict -    ( c-addr u  )
;    internal-blkfid BLKF>BUFFERIDX DUP B/BLK ERASE   ( c-addr u buffer )
;    (SAVEHDR)                   ( c-addr u )
;    UPDATE
;    BLK_HEADER_SIZE internal-blkfid BLKF.OFFSET !
;    internal-blkfid BLKF-PUTCHARS  DROP
;    BLKF-FLUSH  ;
    head(SAVE,SAVE,docolon)
        dw lit,internal_blkfid,SLASHBLKF
        dw lit,enddict,DP,FETCH,lit,enddict,MINUS
        dw lit,internal_blkfid,BLKFTOBUFFERIDX,DUP,B_BLK,ERASE
        dw XSAVEHDR,UPDATE
        dw lit,BLK_HEADER_SIZE,lit,internal_blkfid,STORE
        dw lit,internal_blkfid,BLKF_PUTCHARS,DROP
        dw BLKF_FLUSH
        dw EXIT


;Z RESTORE   blk --            restore internal state from disk
;\ if ENTRY has been set with an xt, then it is automatically
;\ called on restore.
;    DUP >R BLOCK                   ( buffer -- ; blk )
;    DUP @                         ( buffer hdr_size ; blk )
;    BLK_HEADER_SIZE = IF
;      CELL+ DUP @                 ( buffer' data_size ; blk )
;      SWAP CELL+ SNAPSHOT>        ( data_size ; blk )
;
;      enddict SWAP               ( enddict u=data_size ; blk )
;
;      R> internal-blkfid /BLKF
;      BLK_HEADER_SIZE internal-blkfid BLKF.OFFSET !
;      internal-blkfid BLKF-GETCHARS  DROP
;
;      ENTRY ?@EXECUTE
;   ELSE -258 THROW THEN   ;
    head(RESTORE,RESTORE,docolon)
        dw DUP,TOR,BLOCK
        dw DUP,FETCH               ; header size
        dw lit,BLK_HEADER_SIZE,EQUAL,qbranch,RESTORE1
        dw CELLPLUS,DUP,FETCH
        dw SWOP,CELLPLUS
        dw SNAPSHOTFROM

        dw lit,enddict,SWOP

        dw RFROM,lit,internal_blkfid,SLASHBLKF
        dw lit,BLK_HEADER_SIZE,lit,internal_blkfid,STORE
        dw lit,internal_blkfid,BLKF_GETCHARS,DROP
        dw ENTRY,QFETCHEXECUTE
        dw EXIT

RESTORE1:
        dw lit,-258,THROW


; an alternative implementation of recognizers. It
; uses a static list, that can be used
; to implement the search order words independently
;
; Based on the RECOGNIZER by Author: Matthias Trute
; License: Public Domain


; define a recognizer with three actions. Suggesting RECTYPE-* names
;: RECTYPE: ( XT-INTERPRET XT-COMPILE XT-POSTPONE "<spaces>name" -- )
;  CREATE SWAP ROT , , ,
;    ;
dnl ;    head_system(RECTYPECOLON,RECTYPE:,docolon)
dnl ;        DW CREATE,SWOP,ROT,COMMA,COMMA,COMMA
dnl ;        DW EXIT

;: RECTYPE>POST ( RECTYPE-TOKEN -- XT-POSTPONE ) CELL+ CELL+ @ ;
dnl ;    head_system(RECTYPETOPOST,RECTYPE>POST,docode)
RECTYPETOPOST:
        inc bc
        inc bc
rectype_plus2:
        inc bc
        inc bc
rectype_plus0:
        ld hl,bc
        ld c,(hl)
        inc hl
        ld b,(hl)
        next

;: RECTYPE>COMP ( RECTYPE-TOKEN -- XT-COMPILE  )       CELL+ @ ;
dnl ;    head_system(RECTYPETOCOMP,RECTYPE>COMP,docode)
RECTYPETOCOMP:
        jp rectype_plus2

;: RECTYPE>INT  ( RECTYPE-TOKEN -- XT-INTERPRET)             @ ;
dnl ;    head_system(RECTYPETOINT,RECTYPE>INT,docode)
RECTYPETOINT:
        jp rectype_plus0

; :NONAME  ABORT" ?"  ;
REC_NULL_XT:
        call docolon
        DW lit,-13,THROW

; ' NOOP ' NOOP ' NOOP  RECTYPE: RECTYPE-NULL
dnl ;   head_system(RECTYPE_NULL,RECTYPE-NULL,docreate)
RECTYPE_NULL:
        call docreate
        dw REC_NULL_XT
        dw REC_NULL_XT
        dw REC_NULL_XT

; ' NOOP ' NOOP ' NOOP  RECTYPE: RECTYPE-NOOP
dnl ;    head_system(RECTYPE_NOOP,RECTYPE-NOOP,docreate)
dnl RECTYPE_NOOP:
dnl         call docreate
dnl         dw NOOP
dnl         dw NOOP
dnl         dw NOOP

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


;: RECOGNIZE ( addr len -- i*x rectype-token | rectype-null )
;    ['] (recognize) recognizers_end recogizers MAP_UNTIL ( -- i*x rectype-token -1 | addr len 0 )
;    0= IF                           \ no recognizer did the job, remove addr/len
;     2DROP RECTYPE-NULL
;    THEN    ;
dnl ; head_system(RECOGNIZE,RECOGNIZE,docolon)
RECOGNIZE:
        call docolon
        DW lit,XRECOGNIZE,lit,RECOGNIZERS_END,lit,RECOGNIZERS,MAP_UNTIL
        DW ZEROEQUAL,qbranch,RECOGNIZE1
        DW TWODROP,RECTYPE_NULL
RECOGNIZE1:
        DW EXIT

; static list of recognizers in order of priority
RECOGNIZERS:
        DW REC_FIND
        DW REC_NUM
        DW REC_DNUM
        DW REC_CHAR
RECOGNIZERS_END:


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
dnl ;    head_system(REC_FIND,REC-FIND,docolon)
REC_FIND:
        call docolon
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
dnl ;    head(RECTYPE_XT,RECTYPE-XT,docreate)
RECTYPE_XT:
        call docreate
        DW REC_FIND_XT
        DW REC_FIND_COMP
        DW REC_FIND_POST

dnl ; \
dnl ; \ helper words for number recognizers
dnl ; \
dnl ; \ set BASE according the the character at addr
dnl ; \ returned string is stripped of the prefix
dnl ; \ character(s) if found.
dnl ; \ #: 10, $: 16, %: 2, &: 10 (again)
dnl ; create num-bases 10 , 16 , 2 , 10 ,

num_bases:
    defb 10,16,2,10

dnl ; : set-base ( addr len -- addr' len' )
dnl ;   over c@ [CHAR] # - dup 0 4 within if 
dnl ;     cells num-bases + @ base ! 1 /string
dnl ;   else
dnl ;     drop
dnl ;   then 
SET_BASE:
        call docolon
        dw OVER,CFETCH,lit,'#',MINUS,DUP
        dw ZERO,lit,4,WITHIN,qbranch,SET_BASE1
        dw lit,num_bases,PLUS,CFETCH,BASE,STORE
        dw lit,1,SLASHSTRING
        dw branch,SET_BASEX
SET_BASE1:
        DW DROP
SET_BASEX:
        dw EXIT


dnl ; \ check for a character. return string is 
dnl ; \ without it if found. f is true if found.
dnl ; : skip-char? ( addr len c -- addr' len' f)
dnl ;   >r over c@ r> = dup >r
dnl ;   if 1 /string then r> 
dnl ; ;
SKIP_CHARQ:
       call docolon
       DW TOR,OVER,CFETCH,RFROM,EQUAL,DUP,TOR
       DW qbranch,SKIP_CHARQ1
       DW lit,1,SLASHSTRING
SKIP_CHARQ1:
       DW RFROM
       DW EXIT

dnl ; : -sign? ( addr len -- addr' len' f )
dnl ;    [char] - skip-char?
dnl ; ;
MINUS_SIGNQ:
       call docolon
       DW lit,'-',SKIP_CHARQ
       DW EXIT

dnl ; : +sign  ( addr len -- addr' len' )
dnl ;    [char] + skip-char? drop
dnl ; ;
PLUS_SIGN:
       call docolon
       DW lit,'+',SKIP_CHARQ,DROP
       DW EXIT

dnl ; \ allows $- and -$ combinations. skip +
dnl ; \ f is true is - sign is found (and stripped off)
dnl ; : base-and-sign? ( addr len -- addr' len' f)
dnl ;   set-base +sign -sign? >r set-base r>
dnl ; ;
BASE_AND_SIGNQ:
        call docolon
        DW SET_BASE,PLUS_SIGN,MINUS_SIGNQ,TOR,SET_BASE,RFROM
        DW EXIT

dnl ; \ a factor the recognizers below.
dnl ; : (rec-number) ( addr len -- d addr' len' f )
dnl ;    base @ >r           \ save BASE
dnl ;    base-and-sign? >r   \ handle prefix and sign characters
dnl ;    2>r 0 0 2r> >number \ do the actual conversion
dnl ;    r> r> base !        \ restore BASE and sign information
dnl ; ;
XREC_NUMBER:
        call docolon
        DW BASE,FETCH,TOR
        DW BASE_AND_SIGNQ,TOR
        DW TOR,TOR,ZERO,DUP,RFROM,RFROM,TONUMBER
        DW RFROM,RFROM,BASE,STORE
        DW EXIT

;: rec-dnum ( addr len -- d rectype-dnum | rectype-null )
;    \ simple syntax check: last character in addr/len is a dot . ?
;    2dup + 1- c@ [char] . = if
;      1-              \ strip trailing dot
;      (rec-number) >r \ do the dirty work
;      \ a number and only a number?
;      nip if
;        2drop r> drop rectype-null
;      else 
;        r> if dnegate then rectype-dnum 
;      then
;    else 
;      2drop rectype-null  \ no, it cannot be a double cell number.
;    then   ;
dnl ;    head_system(REC_DNUM,REC-DNUM,docolon)
REC_DNUM:
        call docolon
        DW TWODUP,PLUS,ONEMINUS,CFETCH,lit,'.',EQUAL,qbranch,REC_DNUM3
        DW ONEMINUS
        DW XREC_NUMBER,TOR
        DW NIP,qbranch,REC_DNUM1
        DW TWODROP,RFROM,DROP,RECTYPE_NULL,EXIT

REC_DNUM1:
        DW RFROM,qbranch,REC_DNUM2
        DW DNEGATE
REC_DNUM2:
        DW RECTYPE_DNUM,EXIT

REC_DNUM3:
        DW TWODROP,RECTYPE_NULL
        DW EXIT


dnl ;: rec-snum ( addr len -- n rectype-num | rectype-null )
dnl ;    (rec-number) >r
dnl ;    nip if 
dnl ;      2drop r> drop rectype-null
dnl ;    else
dnl ;      \ drop the significant portion of the 'd' value
dnl ;      drop  r> if negate then rectype-num
dnl ;    then
dnl ;;
dnl ;    head_system(REC_NUM,REC-NUM,docolon)
REC_NUM:
        call docolon
        DW XREC_NUMBER,TOR
        DW NIP,qbranch,REC_SNUM1
        DW TWODROP,RFROM,DROP,RECTYPE_NULL,EXIT

REC_SNUM1:
        DW DROP,RFROM,qbranch,REC_SNUM2
        DW NEGATE
REC_SNUM2:
        DW RECTYPE_NUM
        DW EXIT

dnl ;: rec-char ( addr len -- n rectype-num | rectype-null )
dnl ;  3 = if                       \ a three character string
dnl ;    dup c@ [char] ' = if       \ that starts with a ' (tick)
dnl ;      dup 2 + c@ [char] ' = if \ and ends with a ' (tick)
dnl ;        1+ c@ rectype-num exit
dnl ;      then
dnl ;    then
dnl ;  then
dnl ;  drop rectype-null
dnl ;;
dnl ;    head_system(REC_CHAR,REC-CHAR,docolon)
REC_CHAR:
        call docolon
        DW lit,3,EQUAL,qbranch,REC_CHARX
        DW DUP,CFETCH,lit,39,EQUAL,qbranch,REC_CHARX
        DW DUP,ONEPLUS,ONEPLUS,CFETCH,lit,39,EQUAL,qbranch,REC_CHARX
        DW ONEPLUS,CFETCH,RECTYPE_NUM,EXIT
REC_CHARX:
        DW DROP,RECTYPE_NULL
        DW EXIT


;    RECTYPE: RECTYPE-NUM
dnl ;    head_system(RECTYPE_NUM,RECTYPE-NUM,docreate)
RECTYPE_NUM:
        call docreate
        DW NOOP
        DW LITERAL
        DW LITERAL

;    RECTYPE: RECTYPE-DNUM
dnl ;    head_system(RECTYPE_DNUM,RECTYPE-DNUM,docreate)
RECTYPE_DNUM:
        call docreate
        DW NOOP
        DW TWOLITERAL
        DW TWOLITERAL

; : POSTPONE ( "name" -- )  \ COMPILE
; This is the 16K ROM Next Generation version
;   BL WORD  COUNT
;     RECOGNIZE   ( xt flags RECTYPE_XT | RECTYPE_NULL )
;     DUP
;     >R                 ( call POST action )
;     RECTYPE>POST EXECUTE
;     R>
;     RECTYPE>COMP COMMA   ( add compile action to definition )     ;
POSTPONE_16K:
        call docolon
        DW BL,WORD,COUNT
        DW RECOGNIZE
        DW DUP,TOR
        DW RECTYPETOPOST,EXECUTE
        DW RFROM,RECTYPETOCOMP,COMMA
        DW EXIT


;: INTERPRET    i*x c- -- j*x
;                       interpret given buffer
; This is a common factor of EVALUATE and QUIT.
; ref. dpANS-6, 3.4 The Forth Text Interpreter
; This is the 16K ROM Next Generation version
;   BEGIN
;   BL WORD DUP C@ WHILE        -- textadr
;       DUP >R COUNT            -- c-addr n  ; textadr
;       RECOGNIZE   ( i*x RECTYPE_XXX | RECTYPE_NULL )
;       DUP RECTYPE_NULL <> IF  -- i*x RECTYPE_XXX
;           STATE @ IF
;             RECTYPE>COMP EXECUTE
;           ELSE
;             RECTYPE>INT EXECUTE
;           THEN
;           R> DROP
;       ELSE
;           DROP R> COUNT TYPE 3F EMIT -13 THROW  err
;       THEN
;   REPEAT DROP ;
INTERPRET_16K:
        call docolon
INTRP_NG1: DW BL,WORD,DUP,CFETCH,qbranch,INTRP_NG9
           DW DUP,TOR,COUNT
           DW RECOGNIZE
           DW DUP,RECTYPE_NULL,NOTEQUAL,qbranch,INTRP_NG2
           DW STATE,FETCH,qbranch,INTRP_NG3
           DW RECTYPETOCOMP,EXECUTE
           DW branch,INTRP_NG4
INTRP_NG3: DW RECTYPETOINT,EXECUTE
INTRP_NG4: DW RFROM,DROP,branch,INTRP_NG5
INTRP_NG2: DW DROP,RFROM,COUNT,lit,exception_msg,PLACE,lit,-13,THROW
INTRP_NG5: DW branch,INTRP_NG1
INTRP_NG9: DW CHECK_SP,DROP
        DW EXIT



;: REFILL      -- f  refill input buffer
; 16K version
; SOURCE-ID  
;   0 OVER = IF DROP 
;       BLK @ IF LOAD_REFILL EXIT 
;             ELSE  XREFILL8K  EXIT ( TIB version )  THEN
;   THEN
;   -1 OVER = IF DROP FALSE EXIT THEN
;   DROP  'REFILL ?@EXECUTE   ;
XREFILL_16K:
        call docolon
        dw SOURCE_ID
        dw ZERO,OVER,EQUAL,qbranch,XREFILL16K2

        dw DROP,BLK,FETCH,qbranch,XREFILL16K3
        dw LOAD_REFILL,EXIT

XREFILL16K3:
        dw XREFILL8K,EXIT

XREFILL16K2:
        dw ALLONES,OVER,EQUAL,qbranch,XREFILL16K4
        dw DROP,FALSE,EXIT

XREFILL16K4:
        dw DROP,TICKREFILL,FETCHEXECUTE,EXIT


;X SOURCE-ID   'SOURCE-ID @ ;
        head(SOURCE_ID,SOURCE-ID,docolon)
            dw TICKSOURCE_ID,FETCH,EXIT


;X SAVE-INPUT   -- xn ... x1 n                 save input state
;   REFILL-VEC @ SOURCE-ID   BLK @ 'SOURCE 2@  >IN @   ;
    head(SAVE_INPUT,SAVE-INPUT,docolon)
        DW REFILLVEC,FETCH
        DW SOURCE_ID
        DW BLK,FETCH
        DW SLICE_ID,FETCH
        DW TICKSOURCE,TWOFETCH
        DW TOIN,FETCH
        DW lit,7
        DW EXIT

;X RESTORE-INPUT   xn ... x1 n -- flag      restore input state
;   7 = IF  >IN !  'SOURCE 2! BLK ! 'SOURCE-ID ! REFILL-VEC !  FALSE ELSE TRUE THEN ;
    head(RESTORE_INPUT,RESTORE-INPUT,docolon)
        DW lit,7,EQUAL,qbranch,RESTORE_INPUT1
        DW TOIN,STORE
        DW TICKSOURCE,TWOSTORE
        DW SLICE_ID,STORE
        DW BLK,STORE
        DW TICKSOURCE_ID,STORE
        DW REFILLVEC,STORE
        dw BLK,FETCH,QDUP,qbranch,RI1
        DW BLOCK,B_BLK,TICKSOURCE,TWOSTORE
RI1:
        DW FALSE
        DW EXIT

RESTORE_INPUT1:
        DW TRUE
        DW EXIT


; RC2014 Multitasking ====================

;Z UP!  addr --                       set user pointer from TOS
    head_system(UPSTORE,UP!,docode)
        push bc
        pop iy
        pop bc
        next

;Z UP@   -- addr                         get user space pointer
    head_system(UPFETCH,UP@,docode)
        push bc
        push iy
        pop bc
        next


;Z <INIT>   --                                "init" task state
;  R> CELL- UP!
;  S0 SP!
;  R0 RP!
;  L0 LP !
;  0 STATE !
;  0 HANDLER !
;  0 BLK !
;  ['] <WAKE> U0 !   ( we're away! )
;  ENTRY ?@EXECUTE
;  ['] <SLEEP> U0 !  ( if we come back, the stop this thread )
;  (PAUSE)   ;
    head_system(XINIT,<INIT>,docolon)
       dw RFROM,CELLMINUS,UPSTORE
XINIT1:
       dw S0,SPSTORE
       dw R0,RPSTORE
       dw L0,LP,STORE
       dw ZERO,STATE,STORE
       dw ZERO,HANDLER,STORE
       dw ZERO,BLK,STORE
       dw lit,XWAKE,U0,STORE
       dw ENTRY,QFETCHEXECUTE
       dw lit,XSLEEP,U0,STORE
       dw XPAUSE
       dw branch,XINIT1

;Z <WAKE>   --                                "wake" task state
;  R> CELL- UP!
;  STACKTOP @ SP! RP!  ;
    head_system(XWAKE,<WAKE>,docolon)
       dw RFROM,CELLMINUS,UPSTORE
       dw STACKTOP,FETCH,SPSTORE,RPSTORE
       dw EXIT

;Z <SLEEP>  --                            "sleeping" task state
;  R> CELL- UP! LINK @ >R  ;
    head_system(XSLEEP,<SLEEP>,docolon)
       dw RFROM,CELLMINUS,UPSTORE
       dw LINK,FETCH,TOR
       dw EXIT

;Z (PAUSE)     --                                pause run-time
;  RP@ SP@ STACKTOP !
;  LINK @ >R ; COMPILE-ONLY
    head_system(XPAUSE,(PAUSE),docolon)
        dw RPFETCH,SPFETCH,STACKTOP,STORE
        dw LINK,FETCH,TOR
        dw EXIT

;Z INIT-TASK   task-id --         initialise task control block
;  U0 OVER 256 MOVE   ( task-id )
;  DUP LINK !         ( task-id )
;  ['] <INIT>  SWAP !
    head_system(INIT_TASK,INIT-TASK,docolon)
       dw U0,OVER,lit,256,MOVE  ; copy USER variables
       dw DUP,LINK,STORE
       dw lit,XINIT,SWOP,STORE      ; set new task STATE to <INIT>
       dw EXIT


;Z TASK%   -- u                      size of task control block
    head_system(TASKSIZE,TASK%,docon)
        dw 768

; RC2014 16K initialisation ====================


;: /16KROM    init enhanced features
SLASH16KROM:
        call docolon
        DW NINIT,PLUSUSERPTR,STORE
        DW U0,LINK,STORE
        DW lit,XWAKE,U0,STORE
        DW ZERO,RAMTOPSTORE
        DW ZERO,LOCALS_WID,STORE
        DW lit,default_xt_16k_start,lit,default_xt,lit,default_xt_16k_len,MOVE
        DW lit,system_lastword,SYSTEM_WORDLIST,STORE
        DW lit,vocab_lastword,VOCAB_WORDLIST,STORE
        DW FALSE,ENVIRONMENT_WORDLIST,STORE
        DW WORDLISTS,lit,STACK_WORDLISTS_SIZE,SLASHSTACK
        DW VOCAB_WORDLIST,FORTH_WORDLIST,lit,2,WORDLISTS,STACKSET
        DW FORTH_WORDLIST,CURRENT,STORE
        DW SLASHBLKCTX
        DW XSQUOTE
        DB 7,"16K ROM"
        DW TYPE,CR
        DW SLASHCFLASH,qbranch,SLASH16K_1
        DW CF_DRIVE_ID,ZERO,CF_SLICE_ID,SLASHSLICE
        DW CF_SLICE_ID,SELECT
        DW ZERO,CURRENT_BLKFILE_ID,STORE
SLASH16K_1:
        dw EXIT

default_xt_16k_start:
        DW INTERPRET_16K
        DW POSTPONE_16K
        DW FIND_16K
        DW WORDS_16K
        DW XREFILL_16K
        DW NOOP         ; pause
default_xt_16k_end:
defc default_xt_16k_len = default_xt_16k_end - default_xt_16k_start

