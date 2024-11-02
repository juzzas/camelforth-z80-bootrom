; LISTING 2.
;
; ===============================================
; CamelForth for the Zilog Z80
; Copyright (c) 1994,1995 Bradford J. Rodriguez
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
; CAMEL80H.AZM: High Level Words
;   Source code is for the Z80MR macro assembler.
;   Forth words are documented as follows:
;*   NAME     stack -- stack    description
;   Word names in upper case are from the ANS
;   Forth Core word set.  Names in lower case are
;   "internal" implementation words & extensions.
; ===============================================

; SYSTEM VARIABLES & CONSTANTS ==================

SECTION code

;C BL      -- char            an ASCII space
    head(BL,BL,docon)
        dw 20h

;Z tibsize  -- n         size of TIB
    head(TIBSIZE,TIBSIZE,docon)
        dw 254          ; 2 chars safety zone

;X tib     -- a-addr     Terminal Input Buffer
    ;  HEX -80 USER TIB      128bytes below user area
        head(TIB,TIB,docon)
            dw $8500

    ;Z u0      -- a-addr       current user area adrs
    ;  0 USER U0
        head(U0,U0,douser)
            dw 0

    ;C >IN     -- a-addr        holds offset into TIB
    ;  2 USER >IN
        head(TOIN,>IN,douser)
            dw 2

    ;C BASE    -- a-addr       holds conversion radix
    ;  4 USER BASE
        head(BASE,BASE,douser)
            dw 4

    ;C STATE   -- a-addr       holds compiler state
    ;  6 USER STATE
        head(STATE,STATE,douser)
            dw 6

    ;Z dp      -- a-addr       holds dictionary ptr
    ;  8 USER DP
        head(DP,DP,douser)
            dw 8

    ;Z 'source  -- a-addr      two cells: len, adrs
    ; 10 USER 'SOURCE
        head(TICKSOURCE,'SOURCE,douser)
            dw 10

    ;Z latest    -- a-addr     last word in dict.
    ;   14 USER LATEST
        head(LATEST,LATEST,douser)
            dw 14

    ;Z hp       -- a-addr     HOLD pointer
    ;   16 USER HP
        head(HP,HP,douser)
            dw 16

    ;Z LP       -- a-addr     Leave-stack pointer
    ;   18 USER LP
        head(LP,LP,douser)
            dw 18

    ;  20 USER BLK
    ;  22 USER DSK
    ;  24 USER BLKOFFSET
    ;  26 USER BLKLIMIT
    ;  28 USER SECTWRVEC
    ;  30 USER SECTRDVEC
    ;  32 USER SCR
    ;  34 USER REC-USERVEC

    ;Z CURRENT      -- a-addr   address of CURRENT wid
    ;  36 USER CURRENT
        head(CURRENT,CURRENT,douser)
            dw 36

    ;Z EMITVEC      -- xt     if set, use XT as EMIT destination
    ;  40 USER EMITVEC
        head(EMITVEC,EMITVEC,douser)
            dw 40

    ;Z REFILLVEC      -- xt    if set, use XT as REFILL source
    ;  42 USER REFILLVEC
        head(REFILLVEC,REFILLVEC,douser)
            dw 42

    ;Z HANDLER      -- xt    if set, use XT as THROW handler
    ;  44 USER HANDLER
        head(HANDLER,HANDLER,douser)
            dw 44

    ;Z SOURCE-ID      -- addr   current source ID for interpreter
    ;  46 USER SOURCE-ID
        head(SOURCE_ID,SOURCE-ID,douser)
            dw 46

    ;  48 USER WORDLISTS

    ;Z s0       -- a-addr     end of parameter stack
        head(S0,S0,douser)
            dw 100h

    ;X PAD       -- a-addr    user PAD buffer
    ;                         = end of hold area!
        head(PAD,PAD,douser)
            dw 128h

    ;Z l0       -- a-addr     bottom of Leave stack
        head(L0,L0,douser)
            dw 180h

    ;Z r0       -- a-addr     end of return stack
        head(R0,R0,douser)
            dw 200h

    ;Z uinit    -- addr  initial values for user area
        head(UINIT,UINIT,docreate)
            DW 0,0,10,0     ; reserved,>IN,BASE,STATE
            DW enddict      ; DP                         8
            DW 0,0          ; SOURCE init'd elsewhere    10
            DW lastword     ; LATEST
            DW 0            ; HP init'd elsewhere
            DW 0            ; LP init'd elsewhere
            DW 0            ; BLK                        20
            DW 1            ; DSK
            DW 0            ; BLKOFFSET
            DW 8192         ; BLKLIMIT
            DW NOOP         ; SECTWRVEC
            DW NOOP         ; SECTRDVEC                  30
            DW 0            ; SCR
            DW 0            ; REC-USERVEC
            DW 1            ; CURRENT
            DW vocab_lastword            ; VOC-LINK
            DW TOCONSOLE    ; EMITVEC                    40
            DW XREFILL8K   ; REFILLVEC
            DW 0            ; HANDLER
            DW 0            ; SOURCE-ID
            DW 3            ; number of wordlists        48
            DW lastword     ; LFA of FORTH wordlist
            DW editor_lastword   ; LFA of EDITOR wordlist
            DW vocab_lastword    ; LFA of VOCS wordlist


    ;Z #init    -- n    #bytes of user area init data
        head(NINIT,``#INIT'',docon)
            DW 66

    ; ARITHMETIC OPERATORS ==========================

    ;C S>D    n -- d          single -> double prec.
    ;   DUP 0< ;
        head(STOD,S>D,docolon)
            dw DUP,ZEROLESS,EXIT

    ;Z ?NEGATE  n1 n2 -- n3  negate n1 if n2 negative
    ;   0< IF NEGATE THEN ;        ...a common factor
        head(QNEGATE,?NEGATE,docolon)
            DW ZEROLESS,qbranch,QNEG1,NEGATE
    QNEG1:  DW EXIT

    ;C ABS     n1 -- +n2     absolute value
    ;   DUP ?NEGATE ;
        head(ABS,ABS,docolon)
            DW DUP,QNEGATE,EXIT

    ;X DNEGATE   d1 -- d2     negate double precision
    ;   SWAP INVERT SWAP INVERT 1 M+ ;
        head(DNEGATE,DNEGATE,docolon)
            DW SWOP,INVERT,SWOP,INVERT,lit,1,MPLUS
            DW EXIT

    ;Z ?DNEGATE  d1 n -- d2   negate d1 if n negative
    ;   0< IF DNEGATE THEN ;       ...a common factor
        head(QDNEGATE,?DNEGATE,docolon)
            DW ZEROLESS,qbranch,DNEG1,DNEGATE
    DNEG1:  DW EXIT

    ;X DABS     d1 -- +d2    absolute value dbl.prec.
    ;   DUP ?DNEGATE ;
        head(DABS,DABS,docolon)
            DW DUP,QDNEGATE,EXIT

    ;C M*     n1 n2 -- d    signed 16*16->32 multiply
    ;   2DUP XOR >R        carries sign of the result
    ;   SWAP ABS SWAP ABS UM*
    ;   R> ?DNEGATE ;
        head(MSTAR,M*,docolon)
            DW TWODUP,XOR,TOR
            DW SWOP,ABS,SWOP,ABS,UMSTAR
            DW RFROM,QDNEGATE,EXIT

    ;C SM/REM   d1 n1 -- n2 n3   symmetric signed div
    ;   2DUP XOR >R              sign of quotient
    ;   OVER >R                  sign of remainder
    ;   ABS >R DABS R> UM/MOD
    ;   SWAP R> ?NEGATE
    ;   SWAP R> ?NEGATE ;
    ; Ref. dpANS-6 section 3.2.2.1.
        head(SMSLASHREM,SM/REM,docolon)
            DW TWODUP,XOR,TOR,OVER,TOR
            DW ABS,TOR,DABS,RFROM,UMSLASHMOD
            DW SWOP,RFROM,QNEGATE,SWOP,RFROM,QNEGATE
            DW EXIT

    ;C FM/MOD   d1 n1 -- n2 n3   floored signed div'n
    ;   DUP >R              divisor
    ;   2DUP XOR >R         sign of quotient
    ;   >R                  divisor
    ;   DABS R@ ABS UM/MOD
    ;   SWAP R> ?NEGATE SWAP  apply sign to remainder
    ;   R> 0< IF              if quotient negative,
    ;       NEGATE
    ;       OVER IF             if remainder nonzero,
    ;         R@ ROT -  SWAP 1-     adjust rem,quot
    ;       THEN
    ;   THEN  R> DROP ;
    ; Ref. dpANS-6 section 3.2.2.1.
    ; FM/MOD bugfix: https://www.camelforth.com/fmmod.html
        head(FMSLASHMOD,FM/MOD,docolon)
            DW DUP,TOR
            DW TWODUP,XOR,TOR
            DW TOR
            DW DABS,RFETCH,ABS,UMSLASHMOD
            DW SWOP,RFROM,QNEGATE,SWOP
            DW RFROM,ZEROLESS,qbranch,FMMOD1
            DW NEGATE,OVER,qbranch,FMMOD1
            DW RFETCH,ROT,MINUS,SWOP,ONEMINUS
    FMMOD1: DW RFROM,DROP
            DW EXIT

    ;C *      n1 n2 -- n3       signed multiply
    ;   M* DROP ;
        head(STAR,*,docolon)
            dw MSTAR,DROP,EXIT

    ;C /MOD   n1 n2 -- n3 n4    signed divide/rem'dr
    ;   >R S>D R> FM/MOD ;
        head(SLASHMOD,/MOD,docolon)
            dw TOR,STOD,RFROM,FMSLASHMOD,EXIT

    ;C /      n1 n2 -- n3       signed divide
    ;   /MOD nip ;
        head(SLASH,/,docolon)
            dw SLASHMOD,NIP,EXIT

    ;C MOD    n1 n2 -- n3       signed remainder
    ;   /MOD DROP ;
        head(MOD,MOD,docolon)
            dw SLASHMOD,DROP,EXIT

    ;C */MOD  n1 n2 n3 -- n4 n5    n1*n2/n3, rem&quot
    ;   >R M* R> FM/MOD ;
        head(SSMOD,*/MOD,docolon)
            dw TOR,MSTAR,RFROM,FMSLASHMOD,EXIT

    ;C */     n1 n2 n3 -- n4        n1*n2/n3
    ;   */MOD nip ;
        head(STARSLASH,*/,docolon)
            dw SSMOD,NIP,EXIT

    ;C MAX    n1 n2 -- n3       signed maximum
    ;   2DUP < IF SWAP THEN DROP ;
        head(MAX,MAX,docolon)
            dw TWODUP,LESS,qbranch,MAX1,SWOP
    MAX1:   dw DROP,EXIT

    ;C MIN    n1 n2 -- n3       signed minimum
    ;   2DUP > IF SWAP THEN DROP ;
        head(MIN,MIN,docolon)
            dw TWODUP,GREATER,qbranch,MIN1,SWOP
    MIN1:   dw DROP,EXIT

    ; DOUBLE OPERATORS ==============================

    ;C 2@    a-addr -- x1 x2    fetch 2 cells
    ;   DUP CELL+ @ SWAP @ ;
    ;   the lower address will appear on top of stack
        head(TWOFETCH,2@,docolon)
            dw DUP,CELLPLUS,FETCH,SWOP,FETCH,EXIT

    ;C 2!    x1 x2 a-addr --    store 2 cells
    ;   SWAP OVER ! CELL+ ! ;
    ;   the top of stack is stored at the lower adrs
        head(TWOSTORE,2!,docolon)
            dw SWOP,OVER,STORE,CELLPLUS,STORE,EXIT

    ;C 2DROP  x1 x2 --          drop 2 cells
    ;   DROP DROP ;
        head(TWODROP,2DROP,docolon)
            dw DROP,DROP,EXIT

    ;C 2DUP   x1 x2 -- x1 x2 x1 x2   dup top 2 cells
    ;   OVER OVER ;
        head(TWODUP,2DUP,docolon)
            dw OVER,OVER,EXIT

    ;C 2SWAP  x1 x2 x3 x4 -- x3 x4 x1 x2  per diagram
    ;   ROT >R ROT R> ;
        head(TWOSWAP,2SWAP,docolon)
            dw ROT,TOR,ROT,RFROM,EXIT

    ;C 2OVER  x1 x2 x3 x4 -- x1 x2 x3 x4 x1 x2
    ;   >R >R 2DUP R> R> 2SWAP ;
        head(TWOOVER,2OVER,docolon)
            dw TOR,TOR,TWODUP,RFROM,RFROM
            dw TWOSWAP,EXIT


    ; INPUT/OUTPUT ==================================

    ;C EMIT   char --        output character
    ;   EMITVEC @ EXECUTE ;
        head(EMIT,EMIT,docolon)
            dw EMITVEC,FETCH,EXECUTE
            dw EXIT


    ;C COUNT   c-addr1 -- c-addr2 u  counted->adr/len
    ;   DUP CHAR+ SWAP C@ ;
        head(COUNT,COUNT,docolon)
            dw DUP,CHARPLUS,SWOP,CFETCH,EXIT

    ;C CR      --               output newline
    ;   0D EMIT 0A EMIT ;
        head(CR,CR,docolon)
            dw lit,0dh,EMIT,lit,0ah,EMIT,EXIT

    ;C SPACE   --               output a space
    ;   BL EMIT ;
        head(SPACE,SPACE,docolon)
            dw BL,EMIT,EXIT

    ;C SPACES   n --            output n spaces
    ;   BEGIN DUP WHILE SPACE 1- REPEAT DROP ;
        head(SPACES,SPACES,docolon)
    SPCS1:  DW DUP,qbranch,SPCS2
            DW SPACE,ONEMINUS,branch,SPCS1
    SPCS2:  DW DROP,EXIT

    ;Z umin     u1 u2 -- u      unsigned minimum
    ;   2DUP U> IF SWAP THEN DROP ;
        head(UMIN,UMIN,docolon)
            DW TWODUP,UGREATER,qbranch,UMIN1,SWOP
    UMIN1:  DW DROP,EXIT

    ;Z umax    u1 u2 -- u       unsigned maximum
    ;   2DUP U< IF SWAP THEN DROP ;
        head(UMAX,UMAX,docolon)
            DW TWODUP,ULESS,qbranch,UMAX1,SWOP
    UMAX1:  DW DROP,EXIT

; DO_KEY   ( -- char     line ending converted to 13 )
;   BEGIN
;    KEY    ( c )
;    DUP 10 <> IF  ( c )
;        DUP LAST_KEY !    EXIT
;    ELSE
;        LAST_KEY @  13 <> IF  ( c )
;          DROP  13  EXIT
;        ELSE  DROP  THEN
;    THEN
;   AGAIN   ;
;
DO_KEY:
    call docolon
DO_KEY1:
    DW KEY
    DW DUP,lit,10,NOTEQUAL,qbranch,DO_KEY2
    DW DUP,lit,LAST_KEY,STORE,EXIT

DO_KEY2:
    DW lit,LAST_KEY,FETCH  ; is DOS Line-ending?
    DW lit,13,NOTEQUAL,qbranch,DO_KEY3
    DW DROP,lit,13,EXIT

DO_KEY3:   ; line ending is Unix style
    DW DROP
    DW branch,DO_KEY1

SECTION data

LAST_KEY:
    DS 2

SECTION code

    ;C ACCEPT  c-addr +n -- +n'  get line from term'l
    ;   OVER + 1- OVER      -- sa ea a
    ;   BEGIN DO_KEY        -- sa ea a c
    ;   DUP 0D <> WHILE
    ;       DUP EMIT        -- sa ea a c
    ;       DUP 8 = IF  BL EMIT 8 EMIT   THEN
    ;       DUP 8 = IF  DROP 1-    >R OVER R> UMAX
    ;             ELSE  OVER C! 1+ OVER UMIN
    ;       THEN            -- sa ea a
    ;   REPEAT              -- sa ea a c
    ;   DROP NIP SWAP - ;
    head(ACCEPT,ACCEPT,docolon)
        DW OVER,PLUS,ONEMINUS,OVER
ACC1:   DW DO_KEY,DUP,lit,0DH,NOTEQUAL,qbranch,ACC5
        DW DUP,EMIT
        DW DUP,lit,8,EQUAL,qbranch,ACC2
        DW BL,EMIT,lit,8,EMIT
ACC2:   DW DUP,lit,8,EQUAL,qbranch,ACC3
        DW DROP,ONEMINUS,TOR,OVER,RFROM,UMAX
        DW branch,ACC4
ACC3:   DW OVER,CSTORE,ONEPLUS,OVER,UMIN
ACC4:   DW branch,ACC1
ACC5:   DW DROP,NIP,SWOP,MINUS,EXIT

;C TYPE    c-addr +n --     type line to term'l
;   ?DUP IF
;     OVER + SWAP DO I C@ EMIT LOOP
;   ELSE DROP THEN ;
    head(TYPE,TYPE,docolon)
        DW QDUP,qbranch,TYP4
        DW OVER,PLUS,SWOP,xdo
TYP3:   DW II,CFETCH,EMIT,xloop,TYP3
        DW branch,TYP5
TYP4:   DW DROP
TYP5:   DW EXIT

;Z (S")     -- c-addr u   run-time code for S"
;   R> COUNT 2DUP + ALIGNED >R  ;
    head(XSQUOTE,(S"),docolon)
        DW RFROM,COUNT,TWODUP,PLUS,ALIGNED,TOR
        DW EXIT

; allocate buffer for S"
;Z (SBUFFER)      -- c-addr
;   sbuffer_index_ptr @ 3 AND    ( index )
;   1+ 7 LSHIFT
;   SQUOTE_TOP -         ( addr )
;   1 sbuffer_index_ptr +!  ;
XSBUFFER:
        call docolon
        DW lit,sbuffer_index_ptr,FETCH,lit,0x3,AND
        DW ONEPLUS,lit,7,LSHIFT
        DW lit,SQUOTE_TOP,SWOP,MINUS
        DW lit,1,lit,sbuffer_index_ptr,PLUSSTORE
        DW EXIT

;C S"       --         compile in-line string
;C S"       -- addr u  interpret in-line string
;     supports 4 x 128byte strings at SQUOTE_TOP
;  STATE @ IF
;   COMPILE (S")  [ HEX ]
;   22 WORD C@ 1+ ALIGNED ALLOT EXIT
;  ELSE
;   22 WORD COUNT      ( addr u )
;   (XSBUFFER)          ( addr u addr' )
;   SWAP 2DUP >R >R    ( addr addr' u   r: u addr )
;   MOVE R> R>         ( addr u )
;  THEN  ; IMMEDIATE
    immed(SQUOTE,S",docolon)
        DW STATE,FETCH,qbranch,SQUOTE1
        DW lit,XSQUOTE,COMMAXT
        DW lit,22H,WORD,CFETCH,ONEPLUS
        DW ALIGNED,ALLOT,EXIT

SQUOTE1:
        DW lit,22H,WORD,COUNT
        DW XSBUFFER
        DW SWOP,TWODUP,TOR,TOR
        DW MOVE,RFROM,RFROM
        DW EXIT

SECTION data

sbuffer_index_ptr:
        DEFS 2

SECTION code

;C ."       --         compile string to print
;   POSTPONE S"  POSTPONE TYPE ; IMMEDIATE
    immed(DOTQUOTE,.",docolon)
        DW SQUOTE
        DW lit,TYPE,COMMAXT
        DW EXIT

; NUMERIC OUTPUT ================================
; Numeric conversion is done l.s.digit first, so
; the output buffer is built backwards in memory.

; Some double-precision arithmetic operators are
; needed to implement ANSI numeric conversion.

;Z UD/MOD   ud1 u2 -- u3 ud4   32/16->32 divide
;   >R 0 R@ UM/MOD  ROT ROT R> UM/MOD ROT ;
    head(UDSLASHMOD,UD/MOD,docolon)
        DW TOR,lit,0,RFETCH,UMSLASHMOD,ROT,ROT
        DW RFROM,UMSLASHMOD,ROT,EXIT

;Z UD*      ud1 d2 -- ud3      32*16->32 multiply
;   DUP >R UM* DROP  SWAP R> UM* ROT + ;
    head(UDSTAR,UD*,docolon)
        DW DUP,TOR,UMSTAR,DROP
        DW SWOP,RFROM,UMSTAR,ROT,PLUS,EXIT

;C HOLD  char --        add char to output string
;   -1 HP +!  HP @ C! ;
    head(HOLD,HOLD,docolon)
        DW lit,-1,HP,PLUSSTORE
        DW HP,FETCH,CSTORE,EXIT

;C <#    --             begin numeric conversion
;   PAD HP ! ;          (initialize Hold Pointer)
    head(LESSNUM,``<#'',docolon)
        DW PAD,HP,STORE,EXIT

;Z >digit   n -- c      convert to 0..9A..Z
;   [ HEX ] DUP 9 > 7 AND + 30 + ;
    head(TODIGIT,>DIGIT,docolon)
        DW DUP,lit,9,GREATER,lit,7,AND,PLUS
        DW lit,30H,PLUS,EXIT

;C #     ud1 -- ud2     convert 1 digit of output
;   BASE @ UD/MOD ROT >digit HOLD ;
    head(NUM,``#'',docolon)
        DW BASE,FETCH,UDSLASHMOD,ROT,TODIGIT
        DW HOLD,EXIT

;C #S    ud1 -- ud2     convert remaining digits
;   BEGIN # 2DUP OR 0= UNTIL ;
    head(NUMS,``#S'',docolon)
NUMS1:  DW NUM,TWODUP,OR,ZEROEQUAL,qbranch,NUMS1
        DW EXIT

;C #>    ud1 -- c-addr u    end conv., get string
;   2DROP HP @ PAD OVER - ;
    head(NUMGREATER,``#>'',docolon)
        DW TWODROP,HP,FETCH,PAD,OVER,MINUS,EXIT

;C SIGN  n --           add minus sign if n<0
;   0< IF 2D HOLD THEN ;
    head(SIGN,SIGN,docolon)
        DW ZEROLESS,qbranch,SIGN1,lit,2DH,HOLD
SIGN1:  DW EXIT

;Z (U.)    u -- c-addr +n   u unsigned to counted string
;   <# 0 #S #> ;
    head(XUDOT,(U.),docolon)
        DW LESSNUM,lit,0,NUMS,NUMGREATER
        DW EXIT

;C U.    u --               display u unsigned
;   (U.) TYPE SPACE ;
    head(UDOT,U.,docolon)
        DW XUDOT,TYPE,SPACE,EXIT

;Z (.)   n -- c-addr +n     n signed to counted string
;   <# DUP ABS 0 #S ROT SIGN #> ;
    head(XDOT,(.),docolon)
        DW LESSNUM,DUP,ABS,lit,0,NUMS
        DW ROT,SIGN,NUMGREATER,EXIT

;C .     n --           display n signed
;   (.) TYPE SPACE ;
    head(DOT,.,docolon)
        DW XDOT,TYPE,SPACE,EXIT

;C DECIMAL  --      set number base to decimal
;   10 BASE ! ;
    head(DECIMAL,DECIMAL,docolon)
        DW lit,10,BASE,STORE,EXIT

;X HEX     --       set number base to hex
;   16 BASE ! ;
    head(HEX,HEX,docolon)
        DW lit,16,BASE,STORE,EXIT

; DICTIONARY MANAGEMENT =========================

;C HERE    -- addr      returns dictionary ptr
;   DP @ ;
    head(HERE,HERE,docolon)
        dw DP,FETCH,EXIT

;C ALLOT   n --         allocate n bytes in dict
;   DP +! ;
    head(ALLOT,ALLOT,docolon)
        dw DP,PLUSSTORE,EXIT

; Note: , and C, are only valid for combined
; Code and Data spaces.

;C ,    x --           append cell to dict
;   HERE ! 1 CELLS ALLOT ;
    head(COMMA,``,'',docolon)
        dw HERE,STORE,lit,1,CELLS,ALLOT,EXIT

;C C,   char --        append char to dict
;   HERE C! 1 CHARS ALLOT ;
    head(CCOMMA,``C,'',docolon)
        dw HERE,CSTORE,lit,1,CHARS,ALLOT,EXIT

; INTERPRETER ===================================
; Note that NFA>LFA, NFA>CFA, IMMED?, and FIND
; are dependent on the structure of the Forth
; header.  This may be common across many CPUs,
; or it may be different.

;C SOURCE   -- adr n    current input buffer
;   'SOURCE 2@ ;        length is at lower adrs
    head(SOURCE,SOURCE,docolon)
        DW TICKSOURCE,TWOFETCH,EXIT

;X /STRING  a u n -- a+n u-n   trim string
;   ROT OVER + ROT ROT - ;
    head(SLASHSTRING,/STRING,docolon)
        DW ROT,OVER,PLUS,ROT,ROT,MINUS,EXIT

;Z >counted  src n dst --     copy to counted str
;   2DUP C! CHAR+ SWAP CMOVE ;
    head(TOCOUNTED,>COUNTED,docolon)
        DW TWODUP,CSTORE,CHARPLUS,SWOP,CMOVE,EXIT

;C WORD   char -- c-addr    word delim'd by char
;   DUP  SOURCE >IN @ /STRING   -- c c adr n
;   DUP >R   ROT SKIP           -- c adr' n'
;   OVER >R  ROT SCAN           -- adr" n"
;   DUP IF CHAR- THEN        skip trailing delim.
;   R> R> ROT -   >IN +!        update >IN offset
;   TUCK -                      -- adr' N
;   HERE >counted               --
;   HERE                        -- a
;   BL OVER COUNT + C! ;    append trailing blank
    head(WORD,WORD,docolon)
        DW DUP,SOURCE,TOIN,FETCH,SLASHSTRING
        DW DUP,TOR,ROT,skip
        DW OVER,TOR,ROT,scan
        DW DUP,qbranch,WORD1,ONEMINUS  ; char-
WORD1:  DW RFROM,RFROM,ROT,MINUS,TOIN,PLUSSTORE
        DW TUCK,MINUS
        DW HERE,TOCOUNTED,HERE
        DW BL,OVER,COUNT,PLUS,CSTORE,EXIT

;Z NFA>LFA   nfa -- lfa    name adr -> link field
;   3 - ;
    head(NFATOLFA,NFA>LFA,docolon)
        DW lit,3,MINUS,EXIT

;Z NFA>CFA   nfa -- cfa    name adr -> code field
;   COUNT 7F AND + ;       mask off 'smudge' bit
    head(NFATOCFA,NFA>CFA,docolon)
        DW COUNT,lit,07FH,AND,PLUS,EXIT

;Z IMMED?    nfa -- f      fetch immediate flag
;   1- C@ ;                     nonzero if ``immed''
    head(IMMEDQ,IMMED?,docolon)
        DW ONEMINUS,CFETCH,EXIT

;C FIND   c-addr -- c-addr 0   if not found
;C                  xt  1      if immediate
;C                  xt -1      if "normal"
;   LATEST @ BEGIN             -- a nfa
;       2DUP OVER C@ CHAR+     -- a nfa a nfa n+1
;       S=                     -- a nfa f
;       DUP IF
;           DROP
;           NFA>LFA @ DUP      -- a link link
;       THEN
;   0= UNTIL                   -- a nfa  OR  a 0
;   DUP IF
;       NIP DUP NFA>CFA        -- nfa xt
;       SWAP IMMED?            -- xt iflag
;       0= 1 OR                -- xt 1/-1
;   THEN ;
    head(FIND,FIND,docolon)
        DW lit,xt_find,FETCH,EXECUTE
        DW EXIT

FIND_8K:
        call docolon
        DW LATEST,FETCH
FIND1:  DW TWODUP,OVER,CFETCH,CHARPLUS
        DW sequal,DUP,qbranch,FIND2
        DW DROP,NFATOLFA,FETCH,DUP
FIND2:  DW ZEROEQUAL,qbranch,FIND1
        DW DUP,qbranch,FIND3
        DW NIP,DUP,NFATOCFA
        DW SWOP,IMMEDQ,ZEROEQUAL,lit,1,OR
FIND3:  DW EXIT

;C LITERAL  x --        append numeric literal
;   STATE @ IF ['] LIT ,XT , THEN ; IMMEDIATE
; This tests STATE so that it can also be used
; interpretively.  (ANSI doesn't require this.)
    immed(LITERAL,LITERAL,docolon)
        DW STATE,FETCH,qbranch,LITER1
        DW lit,lit,COMMAXT,COMMA
LITER1: DW EXIT

;Z DIGIT?   c -- n -1   if c is a valid digit
;Z            -- x  0   otherwise
;   [ HEX ] DUP 39 > 100 AND +     silly looking
;   DUP 140 > 107 AND -   30 -     but it works!
;   DUP BASE @ U< ;
    head(DIGITQ,DIGIT?,docolon)
        DW DUP,lit,39H,GREATER,lit,100H,AND,PLUS
        DW DUP,lit,140H,GREATER,lit,107H,AND
        DW MINUS,lit,30H,MINUS
        DW DUP,BASE,FETCH,ULESS,EXIT

;Z ?SIGN   adr n -- adr' n' f  get optional sign
;Z  advance adr/n if sign; return NZ if negative
;   OVER C@                 -- adr n c
;   2C - DUP ABS 1 = AND    -- +=-1, -=+1, else 0
;   DUP IF 1+               -- +=0, -=+2
;       >R 1 /STRING R>     -- adr' n' f
;   THEN ;
    head(QSIGN,?SIGN,docolon)
        DW OVER,CFETCH,lit,2CH,MINUS,DUP,ABS
        DW lit,1,EQUAL,AND,DUP,qbranch,QSIGN1
        DW ONEPLUS,TOR,lit,1,SLASHSTRING,RFROM
QSIGN1: DW EXIT

;C >NUMBER  ud adr u -- ud' adr' u'
;C                      convert string to number
;   BEGIN
;   DUP WHILE
;       OVER C@ DIGIT?
;       0= IF DROP EXIT THEN
;       >R 2SWAP BASE @ UD*
;       R> M+ 2SWAP
;       1 /STRING
;   REPEAT ;
    head(TONUMBER,>NUMBER,docolon)
TONUM1: DW DUP,qbranch,TONUM3
        DW OVER,CFETCH,DIGITQ
        DW ZEROEQUAL,qbranch,TONUM2,DROP,EXIT
TONUM2: DW TOR,TWOSWAP,BASE,FETCH,UDSTAR
        DW RFROM,MPLUS,TWOSWAP
        DW lit,1,SLASHSTRING,branch,TONUM1
TONUM3: DW EXIT

;Z ?NUMBER  c-addr -- n -1      string->number
;Z                 -- c-addr 0  if convert error
;   DUP  0 0 ROT COUNT      -- ca ud adr n
;   ?SIGN >R  >NUMBER       -- ca ud adr' n'
;   IF   R> 2DROP 2DROP 0   -- ca 0   (error)
;   ELSE 2DROP NIP R>
;       IF NEGATE THEN  -1  -- n -1   (ok)
;   THEN ;
    head(QNUMBER,?NUMBER,docolon)
        DW DUP,lit,0,DUP,ROT,COUNT
        DW QSIGN,TOR,TONUMBER,qbranch,QNUM1
        DW RFROM,TWODROP,TWODROP,lit,0
        DW branch,QNUM3
QNUM1:  DW TWODROP,NIP,RFROM,qbranch,QNUM2,NEGATE
QNUM2:  DW lit,-1
QNUM3:  DW EXIT

; INTERPRET_IHEX   ( src dest len flag  if flag = -1 -- )
;                  (              flag  if flag = 1  -- )
;       -1 = IF
;           STATE @ 0= IF  (IHEX) ELSE IHEX, THEN
;       ELSE
;
;       THEN
INTERPRET_IHEX:
        call docolon
        DW lit,-1,EQUAL,qbranch,INTERPIHEX2
        DW STATE,FETCH,ZEROEQUAL,qbranch,INTERPIHEX1
        DW XIHEX,EXIT
INTERPIHEX1:
        DW IHEXCOMMA,EXIT
INTERPIHEX2:
        DW EXIT



;Z INTERPRET    i*x  -- j*x
;Z                   interpret buffer at 'SOURCE
; This is a common factor of EVALUATE and QUIT.
; ref. dpANS-6, 3.4 The Forth Text Interpreter
;   BEGIN
;   BL WORD DUP C@ WHILE        -- textadr
;       FIND                    -- a 0/1/-1
;       ?DUP IF                 -- xt 1/-1
;           1+ STATE @ 0= OR    `immed' or interp?
;           IF EXECUTE ELSE ,XT THEN
;       ELSE                    -- textadr
;           DUP COUNT IHEX?
;           ?DUP  IF        -- textadr src dest len f
;               INTERPRET_IHEX  -- textadr
;               DROP
;           ELSE
;               ?NUMBER
;               IF POSTPONE LITERAL     converted ok
;               ELSE COUNT TYPE 3F EMIT CR ABORT  err
;               THEN
;           THEN
;       THEN
;   REPEAT DROP ;
    head(INTERPRET,INTERPRET,docolon)
        DW lit,xt_interpret,FETCH,EXECUTE
        DW EXIT

INTERPRET_8K:
        call docolon
INTER1: DW BL,WORD,DUP,CFETCH,qbranch,INTER9
        DW FIND,QDUP,qbranch,INTER4
        DW ONEPLUS,STATE,FETCH,ZEROEQUAL,OR
        DW qbranch,INTER2
        DW EXECUTE,branch,INTER3
INTER2: DW COMMAXT
INTER3: DW branch,INTER8
INTER4: DW DUP,COUNT,IHEXQ
        DW QDUP,qbranch,INTER4a
        DW INTERPRET_IHEX,DROP,branch,INTER6
INTER4a: DW QNUMBER,qbranch,INTER5
        DW LITERAL,branch,INTER6
INTER5: DW COUNT,TYPE,lit,3FH,EMIT,CR,ABORT
INTER6:
INTER8: DW branch,INTER1
INTER9: DW DROP,EXIT

;X (REFILL8K)  -- f  refill input buffer
;     TIB DUP TIBSIZE ACCEPT 'SOURCE 2! 0 >IN ! SPACE -1 ;
XREFILL8K:
        call docolon
        DW TIB,DUP,TIBSIZE,ACCEPT,TICKSOURCE,TWOSTORE
        DW lit,0,TOIN,STORE,SPACE,lit,-1
        DW EXIT

;X REFILL      -- f  refill input buffer
;   REFILLVEC @ EXECUTE   ;
    head(REFILL,REFILL,docolon)
        DW REFILLVEC,FETCH,EXECUTE
        DW EXIT

;C EVALUATE  i*x c-addr u -- j*x  interpret string
;   SOURCE-ID @ R>
;   -1 SOURCE-ID !
;   'SOURCE 2@ >R >R  >IN @ >R
;   'SOURCE 2! 0 >IN !
;   INTERPRET
;   R> >IN !  R> R> 'SOURCE 2! R> SOURCE-ID !;
    head(EVALUATE,EVALUATE,docolon)
        DW SOURCE_ID,FETCH,TOR
        DW lit,65535,SOURCE_ID,STORE
        DW TICKSOURCE,TWOFETCH,TOR,TOR
        DW TOIN,FETCH,TOR
        DW TICKSOURCE,TWOSTORE,lit,0,TOIN,STORE
        DW INTERPRET
        DW RFROM,TOIN,STORE,RFROM,RFROM
        DW TICKSOURCE,TWOSTORE
        DW RFROM,SOURCE_ID,STORE,EXIT

;C CATCH   xt --          ( exception# | 0 ; r: return addr on stack)
;     SP@ >R             ( xt )       \ save data stack pointer
;     HANDLER @ >R       ( xt )       \ and previous handler
;     RP@ HANDLER !      ( xt )       \ set current handler
;     EXECUTE            ( )          \ execute returns if no THROW
;     R> HANDLER !       ( )          \ restore previous handler
;     R> DROP            ( )          \ discard saved stack ptr
;     0   ;              ( 0 )        \ normal completion
    head(CATCH,CATCH,docolon)
        DW SPFETCH,TOR
        DW HANDLER,FETCH,TOR
        DW RPFETCH,HANDLER,STORE
        DW EXECUTE
        DW RFROM,HANDLER,STORE
        DW RFROM,DROP,lit,0,EXIT


;C THROW ( ??? exception# -- ??? exception# )
;    ?DUP IF          ( exc# )     \ 0 THROW is no-op
;      HANDLER @ RP!   ( exc# )     \ restore prev return stack
;      R> HANDLER !    ( exc# )     \ restore prev handler
;      R> SWAP >R      ( saved-sp ) \ exc# on return stack
;      SP! DROP R>     ( exc# )     \ restore stack
;      \ Return to the caller of CATCH because return
;      \ stack is restored to the state that existed
;       \ when CATCH began execution
;    THEN   ;
    head(THROW,THROW,docolon)
        DW QDUP,qbranch,THROW1
        DW HANDLER,FETCH,RPSTORE
        DW RFROM,HANDLER,STORE
        DW RFROM,SWOP,TOR
        DW SPSTORE,DROP,RFROM
THROW1: DW EXIT



;C QUIT     --    R: i*x --    interpret from kbd
;   L0 LP !  R0 RP!   0 STATE ! 0 HANDLER !  0 SOURCE-ID !
;   ['] XREFILL8K REFILLVEC !
;   BEGIN
;     REFILL  IF
;       ['] INTERPRET CATCH
;       CASE
;          0 OF STATE @ 0= IF ."  OK" THEN CR ENDOF
;         -1 OF ."  ABORT" CR ENDOF
;         -2 OF ( abort message given ) CR ENDOF
;       DUP ." EXCEPTION" . CR
;       ENDCASE
;     ELSE
;       0 SOURCE-ID !
;       0 BLK !
;     THEN
;   AGAIN ;
    head(QUIT,QUIT,docolon)
        DW L0,LP,STORE
        DW R0,RPSTORE,lit,0,STATE,STORE
        DW lit,0,HANDLER,STORE
        DW lit,0,SOURCE_ID,STORE
        DW lit,0,BLK,STORE
        DW lit,XREFILL8K,REFILLVEC,STORE

QUIT1:  DW REFILL

        DW qbranch,QUITX
        DW lit,INTERPRET,CATCH

        ; case 0
        DW DUP,lit,0,EQUAL,qbranch,QUIT2
        DW DROP,STATE,FETCH,ZEROEQUAL,qbranch,QUIT1a
        DW XSQUOTE
        DB 3," OK"
        DW TYPE
QUIT1a: DW CR
        DW branch,QUIT1

        ; case -1
QUIT2:  DW DUP,lit,65535,EQUAL,qbranch,QUIT3
        DW DROP,XSQUOTE
        DB 6," ABORT"
        DW TYPE,CR
        DW branch,QUIT1

        ; case -2
QUIT3:  DW DUP,lit,65534,EQUAL,qbranch,QUIT4
        DW DROP,CR
        DW branch,QUIT1

        ; default
QUIT4:
        DW XSQUOTE
        DB 11," EXCEPTION "
        DW TYPE,DOT,CR
        DW branch,QUIT1

QUITX:
        DW lit,0,SOURCE_ID,STORE
        DW lit,0,BLK,STORE
        DW branch,QUIT1

;C ABORT    i*x --   R: j*x --   clear stk & QUIT
;   S0 SP!  QUIT ;
    head(ABORT,ABORT,docolon)
        DW S0,SPSTORE,QUIT   ; QUIT never returns

;Z ?ABORT   f c-addr u --      abort & print msg
;   ROT IF TYPE ABORT THEN 2DROP ;
    head(QABORT,?ABORT,docolon)
        DW ROT,qbranch,QABO1,TYPE,ABORT
QABO1:  DW TWODROP,EXIT

;C ABORT"  i*x 0  -- i*x   R: j*x -- j*x  x1=0
;C         i*x x1 --       R: j*x --      x1<>0
;   POSTPONE S" POSTPONE ?ABORT ; IMMEDIATE
    immed(ABORTQUOTE,ABORT",docolon)
        DW SQUOTE
        DW lit,QABORT,COMMAXT
        DW EXIT

;C '    -- xt           find word in dictionary
;   BL WORD FIND
;   0= ABORT" ?" ;
     head(TICK,',docolon)
        DW BL,WORD,FIND,ZEROEQUAL,XSQUOTE
        DB 1,"?"
        DW QABORT,EXIT

;C CHAR   -- char           parse ASCII character
;   BL WORD 1+ C@ ;
    head(CHAR,CHAR,docolon)
        DW BL,WORD,ONEPLUS,CFETCH,EXIT

;C [CHAR]   --          compile character literal
;   CHAR  ['] LIT ,XT  , ; IMMEDIATE
    immed(BRACCHAR,[CHAR],docolon)
        DW CHAR
        DW lit,lit,COMMAXT
        DW COMMA,EXIT

;C (    --                     skip input until )
;   [ HEX ] 29 WORD DROP ; IMMEDIATE
    immed(PAREN,``('',docolon)
        DW lit,29H,WORD,DROP,EXIT

; COMPILER ======================================

;C CREATE   --      create an empty definition
;   CURRENT @ WID>NFA , 0 C,         link & `immed' field
;   HERE CURRENT @ WID>NFA!           new "latest" link
;   BL WORD C@ 1+ ALLOT         name field
;   docreate ,CF                code field
    head(CREATE,CREATE,docolon)
        DW CURRENT,FETCH,WIDTONFA,COMMA,lit,0,CCOMMA
        DW HERE,CURRENT,FETCH,WIDTONFASTORE
        DW BL,WORD,CFETCH,ONEPLUS,ALLOT
        DW lit,docreate,COMMACF,EXIT
        
;Z (DOES>)  --      run-time action of DOES>
;   R>              adrs of headless DOES> def'n
;   CURRENT @ WID>NFA NFA>CFA    code field to fix up
;   !CF ;
    head(XDOES,(DOES>),docolon)
        DW RFROM,CURRENT,FETCH,WIDTONFA,NFATOCFA,STORECF
        DW EXIT

;C DOES>    --      change action of latest def'n
;   COMPILE (DOES>)
;   dodoes ,CF ; IMMEDIATE
    immed(DOES,DOES>,docolon)
        DW lit,XDOES,COMMAXT
        DW lit,dodoes,COMMACF,EXIT

;C RECURSE  --      recurse current definition
;   CURRENT @ WID>NFA NFA>CFA ,XT ; IMMEDIATE
    immed(RECURSE,RECURSE,docolon)
        DW CURRENT,FETCH,WIDTONFA,NFATOCFA,COMMAXT,EXIT

;C [        --      enter interpretive state
;   0 STATE ! ; IMMEDIATE
    immed(LEFTBRACKET,[,docolon)
        DW lit,0,STATE,STORE,EXIT

;C ]        --      enter compiling state
;   -1 STATE ! ;
    head(RIGHTBRACKET,],docolon)
        DW lit,-1,STATE,STORE,EXIT

;Z HIDE     --      "hide" latest definition
;   CURRENT @ WID>NFA DUP C@ 80 OR SWAP C! ;
    head(HIDE,HIDE,docolon)
        DW CURRENT,FETCH,WIDTONFA,DUP,CFETCH,lit,80H,OR
        DW SWOP,CSTORE,EXIT

;Z REVEAL   --      "reveal" latest definition
;   CURRENT @ WID>NFA DUP C@ 7F AND SWAP C! ;
    head(REVEAL,REVEAL,docolon)
        DW CURRENT,FETCH,WIDTONFA,DUP,CFETCH,lit,7FH,AND
        DW SWOP,CSTORE,EXIT

;C IMMEDIATE   --   make last def'n immediate
;   1 CURRENT @ WID>NFA 1- C! ;   set immediate flag
    head(IMMEDIATE,IMMEDIATE,docolon)
        DW lit,1,CURRENT,FETCH,WIDTONFA,ONEMINUS,CSTORE
        DW EXIT

;C :        --      begin a colon definition
;   CREATE HIDE ] !COLON ;
    head(COLON,:,docode)
        CALL docolon    ; code fwd ref explicitly
        DW CREATE,HIDE,RIGHTBRACKET,STORCOLON
        DW EXIT

;C ;
;   REVEAL  ,EXIT
;   POSTPONE [  ; IMMEDIATE
    immed(SEMICOLON,;,docolon)
        DW REVEAL,CEXIT
        DW LEFTBRACKET,EXIT

dnl ;C [']  --         find word & compile as literal
dnl ;   '  ['] LIT ,XT  , ; IMMEDIATE
dnl ; When encountered in a colon definition, the
dnl ; phrase  ['] xxx  will cause   lit,xxt  to be
dnl ; compiled into the colon definition (where
dnl ; (where xxt is the execution token of word xxx).
dnl ; When the colon definition executes, xxt will
dnl ; be put on the stack.  (All xt's are one cell.)
    immed(BRACTICK,['],docolon)
        DW TICK               ; get xt of 'xxx'
        DW lit,lit,COMMAXT    ; append LIT action
        DW COMMA,EXIT         ; append xt literal

;C POSTPONE  --   postpone compile action of word
;   BL WORD FIND
;   DUP 0= ABORT" ?"
;   0< IF   -- xt  non `immed': add code to current
;                  def'n to compile xt later.
;       ['] LIT ,XT  ,      add "lit,xt,COMMAXT"
;       ['] ,XT ,XT         to current definition
;   ELSE  ,XT      `immed': compile into cur. def'n
;   THEN ; IMMEDIATE
    immed(POSTPONE,POSTPONE,docolon)
        DW lit,xt_postpone,FETCH,EXECUTE
        DW EXIT

POSTPONE_8K:
        call docolon
        DW BL,WORD,FIND,DUP,ZEROEQUAL,XSQUOTE
        DB 1,"?"
        DW QABORT,ZEROLESS,qbranch,POST1
        DW lit,lit,COMMAXT,COMMA
        DW lit,COMMAXT,COMMAXT,branch,POST2
POST1:  DW COMMAXT
POST2:  DW EXIT
               
;Z COMPILE   --   append inline execution token
;   R> DUP CELL+ >R @ ,XT ;
; The phrase ['] xxx ,XT appears so often that
; this word was created to combine the actions
; of LIT and ,XT.  It takes an inline literal
; execution token and appends it to the dict.
;    xxxhead COMPILE,7,COMPILE,docolon
;        DW RFROM,DUP,CELLPLUS,TOR
;        DW FETCH,COMMAXT,EXIT
; N.B.: not used in the current implementation

; CONTROL STRUCTURES ============================

;C IF       -- orig    conditional forward branch
;   ['] qbranch ,BRANCH  HERE DUP ,DEST ;
;   IMMEDIATE
    immed(IF,IF,docolon)
        DW lit,qbranch,COMMABRANCH
        DW HERE,DUP,COMMADEST,EXIT

;C THEN     orig --        resolve forward branch
;   HERE SWAP !DEST ; IMMEDIATE
    immed(THEN,THEN,docolon)
        DW HERE,SWOP,STOREDEST,EXIT

;C ELSE     orig1 -- orig2    branch for IF..ELSE
;   ['] branch ,BRANCH  HERE DUP ,DEST
;   SWAP  POSTPONE THEN ; IMMEDIATE
    immed(ELSE,ELSE,docolon)
        DW lit,branch,COMMABRANCH
        DW HERE,DUP,COMMADEST
        DW SWOP,THEN,EXIT

;C BEGIN    -- dest        target for bwd. branch
;   HERE ; IMMEDIATE
    immed(BEGIN,BEGIN,docode)
        jp HERE

;C UNTIL    dest --   conditional backward branch
;   ['] qbranch ,BRANCH  ,DEST ; IMMEDIATE
;   conditional backward branch
    immed(UNTIL,UNTIL,docolon)
        DW lit,qbranch,COMMABRANCH
        DW COMMADEST,EXIT

;X AGAIN    dest --      uncond'l backward branch
;   ['] branch ,BRANCH  ,DEST ; IMMEDIATE
;   unconditional backward branch
    immed(AGAIN,AGAIN,docolon)
        DW lit,branch,COMMABRANCH
        DW COMMADEST,EXIT

;C WHILE   dest -- orig dest         branch for WHILE loop
;   ['] qbranch ,BRANCH  HERE DUP ,DEST SWAP  ; IMMEDIATE
    immed(WHILE,WHILE,docolon)
        DW lit,qbranch,COMMABRANCH
        DW HERE,DUP,COMMADEST,SWOP
        DW EXIT

;C REPEAT   orig dest --     resolve WHILE loop
;   POSTPONE AGAIN POSTPONE THEN ; IMMEDIATE
    immed(REPEAT,REPEAT,docolon)
        DW AGAIN,THEN,EXIT

;Z >L   x --   L: -- x        move to leave stack
;   CELL LP +!  LP @ ! ;      (L stack grows up)
    head(TOL,>L,docolon)
        DW CELL,LP,PLUSSTORE,LP,FETCH,STORE,EXIT

;Z L>   -- x   L: x --      move from leave stack
;   LP @ @  CELL NEGATE LP +! ;
    head(LFROM,L>,docolon)
        DW LP,FETCH,FETCH
        DW CELL,NEGATE,LP,PLUSSTORE,EXIT

;C DO       -- adrs   L: -- 0
;   ['] xdo ,XT   HERE     target for bwd branch
;   0 >L ; IMMEDIATE           marker for LEAVEs
    immed(DO,DO,docolon)
        DW lit,xdo,COMMAXT,HERE
            DW lit,0,TOL,EXIT

    ;Z ENDLOOP   adrs xt --   L: 0 a1 a2 .. aN --
    ;   ,BRANCH  ,DEST                backward loop
    ;   BEGIN L> ?DUP WHILE POSTPONE THEN REPEAT ;
    ;                                 resolve LEAVEs
    ; This is a common factor of LOOP and +LOOP.
        head(ENDLOOP,ENDLOOP,docolon)
            DW COMMABRANCH,COMMADEST
    LOOP1:  DW LFROM,QDUP,qbranch,LOOP2
            DW THEN,branch,LOOP1
    LOOP2:  DW EXIT

    ;C LOOP    adrs --   L: 0 a1 a2 .. aN --
    ;   ['] xloop ENDLOOP ;  IMMEDIATE
        immed(LOOP,LOOP,docolon)
            DW lit,xloop,ENDLOOP,EXIT

;C +LOOP   adrs --   L: 0 a1 a2 .. aN --
;   ['] xplusloop ENDLOOP ;  IMMEDIATE
    immed(PLUSLOOP,+LOOP,docolon)
        DW lit,xplusloop,ENDLOOP,EXIT

;C LEAVE    --    L: -- adrs
;   ['] UNLOOP ,XT
;   ['] branch ,BRANCH   HERE DUP ,DEST  >L
;   ; IMMEDIATE      unconditional forward branch
    immed(LEAVE,LEAVE,docolon)
        DW lit,UNLOOP,COMMAXT
        DW lit,branch,COMMABRANCH
        DW HERE,DUP,COMMADEST,TOL,EXIT

; OTHER OPERATIONS ==============================

;X WITHIN   n1|u1 n2|u2 n3|u3 -- f   n2<=n1<n3?
;  OVER - >R - R> U< ;          per ANS document
    head(WITHIN,WITHIN,docolon)
        DW OVER,MINUS,TOR,MINUS,RFROM,ULESS,EXIT

;C MOVE    addr1 addr2 u --     smart move
;             VERSION FOR 1 ADDRESS UNIT = 1 CHAR
;  >R 2DUP SWAP DUP R@ +     -- ... dst src src+n
;  WITHIN IF  R> CMOVE>        src <= dst < src+n
;       ELSE  R> CMOVE  THEN ;          otherwise
    head(MOVE,MOVE,docolon)
        DW TOR,TWODUP,SWOP,DUP,RFETCH,PLUS
        DW WITHIN,qbranch,MOVE1
        DW RFROM,CMOVEUP,branch,MOVE2
MOVE1:  DW RFROM,CMOVE
MOVE2:  DW EXIT

;C DEPTH    -- +n        number of items on stack
;   SP@ S0 SWAP - 2/ ;   16-BIT VERSION!
    head(DEPTH,DEPTH,docolon)
        DW SPFETCH,S0,SWOP,MINUS,TWOSLASH,EXIT

;C ENVIRONMENT?  c-addr u -- false   system query
;                         -- i*x true
;   2DROP 0 ;       the minimal definition!
    head(ENVIRONMENTQ,ENVIRONMENT?,docolon)
        DW TWODROP,lit,0,EXIT

; UTILITY WORDS AND STARTUP =====================

;X (WORDS)  wid  --          list all words in wordlist.
;   WID>NFA DUP 0= IF DROP EXIT THEN
;   BEGIN
;       DUP WHILE
;          DUP COUNT
;              ( ignore zero-length names, AKA :NONAME )
;          ?DUP 0= IF DROP ELSE TYPE SPACE THEN
;          NFA>LFA @
;       REPEAT
;   DROP ;
XWORDS:
        call docolon
        DW WIDTONFA
        DW DUP,ZEROEQUAL,qbranch,WDS1
        DW DROP,EXIT
WDS1:   DW DUP,qbranch,WDS4
        DW DUP,COUNT
        DW QDUP,ZEROEQUAL,qbranch,WDS2
        DW DROP
        DW branch,WDS3
WDS2:   DW TYPE,SPACE
WDS3:   DW NFATOLFA,FETCH
        DW branch,WDS1
WDS4:
        DW DROP,EXIT

;Z WORDS_8K    --          list all words in current wordlist.
;   CONTEXT (WORDS) ;
WORDS_8K:
        call docolon
        DW CONTEXT,XWORDS
        DW EXIT

;X WORDS    --          list all words in current wordlist.
;   xt_words @ EXECUTE ;
    head(WORDS,WORDS,docolon)
        DW lit,xt_words,FETCH,EXECUTE
        DW EXIT

;X .S      --           print stack contents
;   SP@ S0 - IF
;       SP@ S0 2 - DO I @ U. -2 +LOOP
;   THEN ;
    head(DOTS,.S,docolon)
        DW SPFETCH,S0,MINUS,qbranch,DOTS2
        DW SPFETCH,S0,lit,2,MINUS,xdo
DOTS1:  DW II,FETCH,UDOT,lit,-2,xplusloop,DOTS1
DOTS2:  DW EXIT

;C (D.)    d -- c-addr +n      d signed to counted string
    head(XDDOT,(D.),docolon)
        DW LESSNUM,DUP,TOR,DABS,NUMS
        DW RFROM,SIGN,NUMGREATER,EXIT

;C D.    d --           display d signed
    head(DDOT,D.,docolon)
        DW XDDOT,TYPE,SPACE,EXIT

;X D+               d1 d2 -- d1+d2              Add double numbers
    head(DPLUS,D+,docode)
        exx
        pop bc          ; BC'=d2lo
        exx
        pop hl          ; HL=d1hi,BC=d2hi
        exx
        pop hl          ; HL'=d1lo
        add hl,bc
        push hl         ; 2OS=d1lo+d2lo
        exx
        adc hl,bc       ; HL=d1hi+d2hi+cy
        ld b,h
        ld c,l
        next

;C 2>R   d d --           2 cells to R
    head(TWOTOR,2>R,docolon)
        DW SWOP,RFROM,SWOP,TOR,SWOP,TOR,TOR,EXIT

;C 2R>   -- d d           fetch 2 cells from R
    head(TWORFROM,2R>,docolon)
        DW RFROM,RFROM,RFROM,SWOP,ROT,TOR,EXIT


TNEGATE:
        call docolon
        DW TOR,TWODUP,OR,DUP,qbranch,TNEG1,DROP,DNEGATE,lit,1
TNEG1:
        DW RFROM,PLUS,NEGATE,EXIT

qtneg:
        call docolon
        DW ZEROLESS,qbranch,qtneg1,TNEGATE
qtneg1:
        DW EXIT

TSTAR:
        call docolon
        DW TWODUP,XOR,TOR
        DW TOR,DABS,RFROM,ABS
        DW TWOTOR
        DW RFETCH,UMSTAR,lit,0
        DW TWORFROM,UMSTAR
        DW DPLUS
        DW RFROM
        DW qtneg
        DW EXIT

TDIV:
        call docolon
        DW OVER,TOR,TOR
        DW DUP,qtneg
        DW RFETCH,UMSLASHMOD
        DW ROT,ROT
        DW RFROM,UMSLASHMOD
        DW NIP,SWOP
        DW RFROM,ZEROLESS,qbranch,tdiv1,DNEGATE
tdiv1:
        DW EXIT

    head(MSTARSLASH,M*/,docolon)
        DW TOR,TSTAR,RFROM,TDIV,EXIT

;Z ?ROM16K    -- f      is it a 16K ROM?
;
    head(ROM16KQ,ROM16K?,docolon)
        DW lit,rom_16k_signature
        DW XSQUOTE
        DB 6,"RC2014"
        DW sequal,ZEROEQUAL
        DW EXIT


;Z COLD     --      cold start Forth system
;   UINIT U0 #INIT CMOVE      init user area
;   80 COUNT INTERPRET       interpret CP/M cmd
;   ." Z80 CamelForth etc."
;   ABORT ;
    head(COLD,COLD,docolon)
        DW UINIT,U0,NINIT,CMOVE
        DW lit,camel_signon
        DW lit,camel_signon_len
        DW TYPE,CR
        DW lit,65535,RAMTOP,STORE
        ;DW lit,0,INTVEC,STORE
        DW ROM16KQ,qbranch,COLD1
        DW lit,65535,lit,flag_rom16k,STORE
        DW SLASH16KROM
        DW lit,FIND_16K,lit,xt_find,STORE
        DW lit,POSTPONE_16K,lit,xt_postpone,STORE
        DW lit,INTERPRET_16K,lit,xt_interpret,STORE
        DW lit,WORDS_16K,lit,xt_words,STORE
        DW ABORT

COLD1:  DW lit,lastword8k,LATEST,STORE
        DW lit,0,lit,flag_rom16k,STORE
        DW lit,FIND_8K,lit,xt_find,STORE
        DW lit,POSTPONE_8K,lit,xt_postpone,STORE
        DW lit,INTERPRET_8K,lit,xt_interpret,STORE
        DW lit,WORDS_8K,lit,xt_words,STORE
        DW ABORT       ; ABORT never returns

;Z WARM     --      warm start Forth system
;   ." Z80 CamelForth etc."
;   ABORT ;
    head(WARM,WARM,docolon)
        DW lit,camel_signon
        DW lit,camel_signon_len
        DW TYPE
        DW XSQUOTE
        DB 7," (warm)"
        DW TYPE,CR
        DW ROM16KQ,qbranch,WARM1
        DW SLASH16KROM
WARM1:
        DW ABORT       ; ABORT never returns


;: WID>NFA ( wid -- nfa )
; Return the address of the first name field in the word list identified by wid.
;       WORDLISTS DUP @ + @    ;
        head(WIDTONFA,WID>NFA,docolon)
            dw lit,flag_rom16k,FETCH,qbranch,WIDTONFA1
            dw FETCH
            dw EXIT
WIDTONFA1:
            dw DROP,LATEST,FETCH
            dw EXIT

;: WID>NFA! ( nfa wid -- )
; Store the address of the first name field in the word list identified by wid.
;       CELLS,WORDLISTS DUP @ CELLS + !    ;
        head(WIDTONFASTORE,WID>NFA!,docolon)
            dw lit,flag_rom16k,FETCH,qbranch,WIDTONFASTOR1
            dw STORE
            dw EXIT
WIDTONFASTOR1:
            dw DROP,LATEST,STORE
            dw EXIT

;: CONTEXT      ( -- wid )
;    STACK_WORDLIST STACK@
        head(CONTEXT,CONTEXT,docolon)
            dw lit,flag_rom16k,FETCH,qbranch,CONTEXT1
            dw lit,STACK_WORDLISTS,STACKFETCH
            dw EXIT
CONTEXT1:
            dw LATEST
            dw EXIT


camel_signon:
        DB "Z80 CamelForth v1.02  25 Jan 1995"
	defc camel_signon_len = 33

SECTION data

flag_rom16k:
        DEFS 2
xt_interpret:
        DEFS 2
xt_postpone:
        DEFS 2
xt_find:
        DEFS 2
xt_words:
        DEFS 2

SECTION code
