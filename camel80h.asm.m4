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

;C BL      -- char            an ASCII space
    head(BL,BL,docon)
        dw 20h

;Z tibsize  -- n         size of TIB
    head(TIBSIZE,TIBSIZE,docon)
        dw 252          ; 2 chars safety zone

;X tib     -- a-addr     Terminal Input Buffer
;  HEX 82 CONSTANT TIB   CP/M systems: 126 bytes
    ;  HEX -80 USER TIB      others: below user area
        head(TIB,TIB,docon)
            dw $8400

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

    ;Z BLK      -- a-addr     block number storage
    ;  20 USER BLK
        head(BLK,BLK,douser)
            dw 20

    ;Z DSK      -- a-addr     disk number storage
    ;  22 USER DSK
        head(DSK,DSK,douser)
            dw 22

    ;Z BLKBUFFER    -- a-addr  1024byte block buffer
    ;  24 USER BLKBUFFER
        head(BLKBUFFER,BLKBUFFER,douser)
            dw 24

    ;Z BLKUPDATE    -- a-addr  block update flag storage
    ;  26 USER BLKUPDATE
        head(BLKUPDATE,BLKUPDATE,douser)
            dw 26

    ;Z BLKREADVEC   -- a-addr  1024byte block buffer
    ;  24 USER BLKREADVEC
        head(BLKREADVEC,BLKREADVEC,douser)
            dw 28

    ;Z BLKWRITEVEC   -- a-addr  1024byte block buffer
    ;  24 USER BLKWRITEVEC
        head(BLKWRITEVEC,BLKWRITEVEC,douser)
            dw 30

    ;Z SCR          -- a-addr  last edited screen number
    ;  28 USER SCR
        head(SCR,SCR,douser)
            dw 32

    ;Z IHXCRC       -- a-addr  location for current HEXLOAD CRC
    ;  30 USER IHXCRC
        head(IHXCRC,IHXCRC,douser)
            dw 34

    ;Z SEED        -- a-addr   seed for random number generator
    ;  32 USER SEED
        head(SEED,SEED,douser)
            dw 36

    ;Z CONTEXT      -- a-addr   context for VOCABULARY
    ;  32 USER CONTEXT
        head(CONTEXT_OLD,CONTEXT_OLD,douser)
            dw 38

    ;Z CURRENT      -- a-addr   context for CURRENT
    ;  32 USER CURRENT
        head(CURRENT,CURRENT,douser)
            dw 40

    ;Z VOCLINK      -- a-addr   context for CURRENT
    ;  32 USER CURRENT
        head(VOCLINK,VOCLINK,douser)
            dw 42

    ;Z CALLSP      -- a-addr   address to set SP during CALL
    ;  32 USER CALLSP
        head(CALLSP,CALLSP,douser)
            dw 44

    ;Z INTVEC      -- a-addr   interrupt vector
    ;  32 USER INTVEC
        head(INTVEC,INTVEC,douser)
            dw 46

    ;Z RST30VEC     -- a-addr   RST30 vector
    ;  32 USER RST30VEC
        head(RST30VEC,RST30VEC,douser)
            dw 48

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
            DW enddict      ; DP
            DW 0,0          ; SOURCE init'd elsewhere
            DW lastword     ; LATEST
            DW 0            ; HP init'd elsewhere
            DW 0            ; LP init'd elsewhere
            DW 65535        ; BLK
            DW 1            ; DSK
            DW 0x8800       ; BLKBUFFER
            DW 0            ; BLKUPDATE
            DW 0            ; BLKREADVEC
            DW 0            ; BLKWRITEVEC
            DW 0            ; SCR
            DW 0            ; IHXCRC
            DW 42           ; SEED
            DW 0            ; CONTEXT
            DW 0            ; CURRENT
            DW vocab_lastword            ; VOC-LINK
            DW 0            ; CALLSP
            DW 0            ; INTVEC
            DW 0            ; RST30VEC


    ;Z #init    -- n    #bytes of user area init data
        head(NINIT,``#INIT'',docon)
            DW 50

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
    ;   DUP >R              save divisor
    ;   SM/REM
    ;   DUP 0< IF           if quotient negative,
    ;       SWAP R> +         add divisor to rem'dr
    ;       SWAP 1-           decrement quotient
    ;   ELSE R> DROP THEN ;
    ; Ref. dpANS-6 section 3.2.2.1.
        head(FMSLASHMOD,FM/MOD,docolon)
            DW DUP,TOR,SMSLASHREM
            DW DUP,ZEROLESS,qbranch,FMMOD1
            DW SWOP,RFROM,PLUS,SWOP,ONEMINUS
            DW branch,FMMOD2
    FMMOD1: DW RFROM,DROP
    FMMOD2: DW EXIT

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

    ;C ACCEPT  c-addr +n -- +n'  get line from term'l
    ;   OVER + 1- OVER      -- sa ea a
    ;   BEGIN KEY           -- sa ea a c
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
ACC1:   DW KEY,DUP,lit,0DH,NOTEQUAL,qbranch,ACC5
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

;C S"       --         compile in-line string
;   COMPILE (S")  [ HEX ]
;   22 WORD C@ 1+ ALIGNED ALLOT ; IMMEDIATE
    immed(SQUOTE,S",docolon)
        DW lit,XSQUOTE,COMMAXT
        DW lit,22H,WORD,CFETCH,ONEPLUS
        DW ALIGNED,ALLOT,EXIT

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

;C WORD   char -- c-addr n   word delim'd by char
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
dnl ;    head(FIND,FIND,docolon)
;        DW LATEST,FETCH
;FIND1:  DW TWODUP,OVER,CFETCH,CHARPLUS
;        DW sequal,DUP,qbranch,FIND2
;        DW DROP,NFATOLFA,FETCH,DUP
;FIND2:  DW ZEROEQUAL,qbranch,FIND1
;        DW DUP,qbranch,FIND3
;        DW NIP,DUP,NFATOCFA
;        DW SWOP,IMMEDQ,ZEROEQUAL,lit,1,OR
;FIND3:  DW EXIT

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

;Z INTERPRET    i*x c-addr u -- j*x
;Z                      interpret given buffer
; This is a common factor of EVALUATE and QUIT.
; ref. dpANS-6, 3.4 The Forth Text Interpreter
;   'SOURCE 2!  0 >IN !
;   BEGIN
;   BL WORD DUP C@ WHILE        -- textadr
;       FIND                    -- a 0/1/-1
;       ?DUP IF                 -- xt 1/-1
;           1+ STATE @ 0= OR    `immed' or interp?
;           IF EXECUTE ELSE ,XT THEN
;       ELSE                    -- textadr
;           ?NUMBER
;           IF POSTPONE LITERAL     converted ok
;           ELSE COUNT TYPE 3F EMIT CR ABORT  err
;           THEN
;       THEN
;   REPEAT DROP ;
    head(INTERPRET,INTERPRET,docolon)
        DW TICKSOURCE,TWOSTORE,lit,0,TOIN,STORE
INTER1: DW BL,WORD,DUP,CFETCH,qbranch,INTER9
        DW FIND,QDUP,qbranch,INTER4
        DW ONEPLUS,STATE,FETCH,ZEROEQUAL,OR
        DW qbranch,INTER2
        DW EXECUTE,branch,INTER3
INTER2: DW COMMAXT
INTER3: DW branch,INTER8
INTER4: DW QNUMBER,qbranch,INTER5
        DW LITERAL,branch,INTER6
INTER5: DW COUNT,TYPE,lit,3FH,EMIT,CR,ABORT
INTER6:
INTER8: DW branch,INTER1
INTER9: DW DROP,EXIT

;C EVALUATE  i*x c-addr u -- j*x  interprt string
;   'SOURCE 2@ >R >R  >IN @ >R
;   INTERPRET
;   R> >IN !  R> R> 'SOURCE 2! ;
    head(EVALUATE,EVALUATE,docolon)
        DW TICKSOURCE,TWOFETCH,TOR,TOR
        DW TOIN,FETCH,TOR,INTERPRET
        DW RFROM,TOIN,STORE,RFROM,RFROM
        DW TICKSOURCE,TWOSTORE,EXIT

;C QUIT     --    R: i*x --    interpret from kbd
;   L0 LP !  R0 RP!   0 STATE !
;   BEGIN
;       TIB DUP TIBSIZE ACCEPT  SPACE
;       INTERPRET
;       STATE @ 0= IF ."  OK" CR THEN
;   AGAIN ;
    head(QUIT,QUIT,docolon)
        DW L0,LP,STORE
        DW R0,RPSTORE,lit,0,STATE,STORE
QUIT1:  DW TIB,DUP,TIBSIZE,ACCEPT,SPACE
        DW INTERPRET
        DW STATE,FETCH,ZEROEQUAL,qbranch,QUIT2
        DW XSQUOTE
        DB 3," OK"
        DW TYPE,CR
QUIT2:  DW branch,QUIT1

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
;   LATEST @ , 0 C,         link & `immed' field
;   HERE LATEST !           new "latest" link
;   BL WORD C@ 1+ ALLOT         name field
;   docreate ,CF                code field
    head(CREATE,CREATE,docolon)
        DW LATEST,FETCH,COMMA,lit,0,CCOMMA
        DW HERE,LATEST,STORE
        DW BL,WORD,CFETCH,ONEPLUS,ALLOT
        DW lit,docreate,COMMACF,EXIT
        
;Z (DOES>)  --      run-time action of DOES>
;   R>              adrs of headless DOES> def'n
;   LATEST @ NFA>CFA    code field to fix up
;   !CF ;
    head(XDOES,(DOES>),docolon)
        DW RFROM,LATEST,FETCH,NFATOCFA,STORECF
        DW EXIT

;C DOES>    --      change action of latest def'n
;   COMPILE (DOES>)
;   dodoes ,CF ; IMMEDIATE
    immed(DOES,DOES>,docolon)
        DW lit,XDOES,COMMAXT
        DW lit,dodoes,COMMACF,EXIT

;C RECURSE  --      recurse current definition
;   LATEST @ NFA>CFA ,XT ; IMMEDIATE
    immed(RECURSE,RECURSE,docolon)
        DW LATEST,FETCH,NFATOCFA,COMMAXT,EXIT

;C [        --      enter interpretive state
;   0 STATE ! ; IMMEDIATE
    immed(LEFTBRACKET,[,docolon)
        DW lit,0,STATE,STORE,EXIT

;C ]        --      enter compiling state
;   -1 STATE ! ;
    head(RIGHTBRACKET,],docolon)
        DW lit,-1,STATE,STORE,EXIT

;Z HIDE     --      "hide" latest definition
;   LATEST @ DUP C@ 80 OR SWAP C! ;
    head(HIDE,HIDE,docolon)
        DW LATEST,FETCH,DUP,CFETCH,lit,80H,OR
        DW SWOP,CSTORE,EXIT

;Z REVEAL   --      "reveal" latest definition
;   LATEST @ DUP C@ 7F AND SWAP C! ;
    head(REVEAL,REVEAL,docolon)
        DW LATEST,FETCH,DUP,CFETCH,lit,7FH,AND
        DW SWOP,CSTORE,EXIT

;C IMMEDIATE   --   make last def'n immediate
;   1 LATEST @ 1- C! ;   set immediate flag
    head(IMMEDIATE,IMMEDIATE,docolon)
        DW lit,1,LATEST,FETCH,ONEMINUS,CSTORE
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

;C IF       -- adrs    conditional forward branch
;   ['] qbranch ,BRANCH  HERE DUP ,DEST ;
;   IMMEDIATE
    immed(IF,IF,docolon)
        DW lit,qbranch,COMMABRANCH
        DW HERE,DUP,COMMADEST,EXIT

;C THEN     adrs --        resolve forward branch
;   HERE SWAP !DEST ; IMMEDIATE
    immed(THEN,THEN,docolon)
        DW HERE,SWOP,STOREDEST,EXIT

;C ELSE     adrs1 -- adrs2    branch for IF..ELSE
;   ['] branch ,BRANCH  HERE DUP ,DEST
;   SWAP  POSTPONE THEN ; IMMEDIATE
    immed(ELSE,ELSE,docolon)
        DW lit,branch,COMMABRANCH
        DW HERE,DUP,COMMADEST
        DW SWOP,THEN,EXIT

;C BEGIN    -- adrs        target for bwd. branch
;   HERE ; IMMEDIATE
    immed(BEGIN,BEGIN,docode)
        jp HERE

;C UNTIL    adrs --   conditional backward branch
;   ['] qbranch ,BRANCH  ,DEST ; IMMEDIATE
;   conditional backward branch
    immed(UNTIL,UNTIL,docolon)
        DW lit,qbranch,COMMABRANCH
        DW COMMADEST,EXIT

;X AGAIN    adrs --      uncond'l backward branch
;   ['] branch ,BRANCH  ,DEST ; IMMEDIATE
;   unconditional backward branch
    immed(AGAIN,AGAIN,docolon)
        DW lit,branch,COMMABRANCH
        DW COMMADEST,EXIT

;C WHILE    -- adrs         branch for WHILE loop
;   POSTPONE IF ; IMMEDIATE
    immed(WHILE,WHILE,docode)
        jp IF

;C REPEAT   adrs1 adrs2 --     resolve WHILE loop
;   SWAP POSTPONE AGAIN POSTPONE THEN ; IMMEDIATE
    immed(REPEAT,REPEAT,docolon)
        DW SWOP,AGAIN,THEN,EXIT

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

;X WORDS  wid  --          list all words in wordlist.
;   @ BEGIN
;       DUP COUNT
;           ( ignore zero-length names, AKA :NONAME )
;       ?DUP 0= IF DROP ELSE TYPE SPACE THEN
;       NFA>LFA @
;   DUP 0= UNTIL
;   DROP ;
    head(XWORDS,(WORDS),docolon)
        DW FETCH
WDS1:   DW DUP,COUNT
        DW QDUP,ZEROEQUAL,qbranch,WDS2
        DW DROP
        DW branch,WDS3
WDS2:   DW TYPE,SPACE
WDS3:   DW NFATOLFA,FETCH
        DW DUP,ZEROEQUAL,qbranch,WDS1
        DW DROP,EXIT

;X WORDS    --          list all words in current wordlist.
;   CONTEXT (WORDS) ;
    head(WORDS,WORDS,docolon)
        DW CONTEXT,XWORDS
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

;C 2>R   d --           2 to R
    head(TWOTOR,2>R,docolon)
        DW SWOP,RFROM,SWOP,TOR,SWOP,TOR,TOR,EXIT

;C 2R>   d --           fetch 2 from R
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
        DW XSQUOTE
        DB 35,"Z80 CamelForth v1.02  25 Jan 1995"
        DB 0dh,0ah
        DW TYPE
        DW ROM16KQ,qbranch,COLD1
        DW XSQUOTE
        DB 10," - 16K ROM"
        DW branch,COLD2
COLD1:  DW lit,lastword8k,LATEST,STORE
        DW XSQUOTE
        DB 9," - 8K ROM"
COLD2:  DW TYPE,CR
        DW lit,lastword,lit,FORTH_WORDLIST_WID,STORE
        DW lit,editor_lastword,lit,EDITOR_WORDLIST_WID,STORE
        DW lit,vocab_lastword,lit,VOCAB_WORDLIST_WID,STORE
        DW FORTH_WORDLIST,lit,1,SET_ORDER
        DW ABORT       ; ABORT never returns

;Z WARM     --      warm start Forth system
;   ." Z80 CamelForth etc."
;   ABORT ;
    head(WARM,WARM,docolon)
        DW XSQUOTE
        DB 47,"Z80 CamelForth v1.02  25 Jan 1995 (warmstart)"
        DB 0dh,0ah
        DW TYPE
        DW lit,lastword,lit,FORTH_WORDLIST_WID,STORE
        DW lit,editor_lastword,lit,EDITOR_WORDLIST_WID,STORE
        DW lit,vocab_lastword,lit,VOCAB_WORDLIST_WID,STORE
        DW FORTH_WORDLIST,lit,1,SET_ORDER
        DW ABORT       ; ABORT never returns

