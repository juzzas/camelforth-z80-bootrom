; RC2014 Extension Words
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

EXTERN cflash_read_block
EXTERN cflash_write_block

;Z CALL       a-addr --    call machine code at address
    head(CALL,CALL,docode)
        ; protect against some stack abuse
        ld (forth_stack_save), sp
        ld sp, forth_state_stack_top
        push ix
        push iy
        push hl
        push de
        ld (forth_state_stack_save), sp

        ; set up user stack
        ld sp, 0    ; end of RAM

        ld hl, call_exit  ; return address
        push hl
        ld h,b
        ld l,c
        jp (hl)

call_exit:
        ld sp, (forth_state_stack_save)
        pop de
        pop hl
        pop iy
        pop ix

        ld sp, (forth_stack_save)
        pop bc   ; DROP the address from TOS and fill BC with new TOS
        next


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; forth_push --  push BC onto the forth stack
PUBLIC forth_push
forth_push:
        ld (user_stack_save), sp
        ld sp, (forth_stack_save)
        push bc
        ld (forth_stack_save), sp
        ld sp, (user_stack_save)
        ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; forth_push --  pop BC from the forth stack
PUBLIC forth_pop
forth_pop:
        ld (user_stack_save), sp
        ld sp, (forth_stack_save)
        pop bc
        ld (forth_stack_save), sp
        ld sp, (user_stack_save)
        ret

SECTION data_user

forth_stack_save:
        DEFW  0

user_stack_save:
        DEFW  0

forth_state_stack:
        DEFS 16
forth_state_stack_top:

forth_state_stack_save:
        DEFW  0

SECTION code_user

; RC2014 EXTENSION misc ======================

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


; RC2014 EXTENSION output ====================

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

;Z RESET  ( -- reset attributes )
;    VT-ESC ." 0m" ;
    head(RESET,RESET,docolon)
        dw VT_ESC, XSQUOTE
        db 2,"0m"
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

;Z INVIS  ( -- make cursor invisible )
;    VT-ESC ." ?25l" ;
    head(INVIS,INVIS,docolon)
        dw VT_ESC,XSQUOTE
        db 4,"?25l"
        dw TYPE
        dw EXIT

;Z VIS  ( -- make cursor visible )
;    ESC ." [?25h" ;
    head(VIS,VIS,docolon)
        dw VT_ESC,XSQUOTE
        db 4,"?25h"
        dw TYPE
        dw EXIT

;Z BELL  ( -- beep )
;    BEL EMIT ;
    head(BELL,BELL,docolon)
        dw lit,7,EMIT
        dw EXIT

; 0 CONSTANT BLACK
    head(BLACK,BLACK,docon)
        dw 0

; 1 CONSTANT RED
    head(RED,RED,docon)
        dw 1

; 2 CONSTANT GREEN
    head(GREEN,GREEN,docon)
        dw 2

; 3 CONSTANT YELLOW
    head(YELLOW,YELLOW,docon)
        dw 3

; 4 CONSTANT BLUE
    head(BLUE,BLUE,docon)
        dw 4

; 5 CONSTANT MAGENTA
    head(MAGENTA,MAGENTA,docon)
        dw 5

; 6 CONSTANT CYAN
    head(CYAN,CYAN,docon)
        dw 6

; 7 CONSTANT WHITE
    head(WHITE,WHITE,docon)
        dw 7

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



; HEXLOAD implementation ==========================

; 0 CONSTANT IHXOK
; 1 CONSTANT IHXEND
; 2 CONSTANT IHXERROR

;Z <ACCEPT  ( c-addr +n -- +n'   get line from term.; no echo )
;    OVER + 1- OVER      \ sa ea a
;    BEGIN KEY           \ sa ea a c
;    DUP 13 <> WHILE
;        OVER C! 1+ OVER UMIN
;    REPEAT              \ sa ea a c
;    DROP NIP SWAP - ;
    head(FROMACCEPT,<ACCEPT,docolon)
        DW OVER,PLUS,ONEMINUS,OVER

FROMACC1:
        DW KEY,DUP,lit,0DH,NOTEQUAL,qbranch,FROMACC2
        DW OVER,CSTORE,ONEPLUS,OVER,UMIN
        DW branch,FROMACC1

FROMACC2:
        DW DROP,NIP,SWOP,MINUS,EXIT

;Z IHXCRC+  ( c -- )
;    IHXCRC @ + FF AND IHXCRC ! ;
    head(IHXCRCPLUS,IHXCRC+,docolon)
        DW IHXCRC,FETCH,PLUS,IHXCRC,STORE
        DW EXIT

;Z ?IHXCRC  ( c -- flag )    ( does the crc match? )
;    IHXCRC @ NEGATE FF AND = ;
    head(QIHXCRC,?IHXCRC,docolon)
        DW IHXCRC,FETCH,NEGATE,lit,255,AND,EQUAL
        DW EXIT

;Z (IHXBYTE)  ( tib-ptr -- u tib-ptr )
;    >R 0 S>D R> 2
;    >NUMBER       ( du tib-ptr u )
;    DROP NIP      ( u tib-ptr )
;    ;
    head(XIHXBYTE,(IHXBYTE),docolon)
        DW TOR,lit,0,STOD,RFROM,lit,2
        DW TONUMBER
        DW DROP,NIP
        DW EXIT

;Z IHXBYTE  ( tib-ptr -- u tib-ptr )
;    >R 0 S>D R> 2
;    >NUMBER       ( du tib-ptr u )
;    DROP NIP      ( u tib-ptr )
;    OVER IHXCRC+  ;
    head(IHXBYTE,IHXBYTE,docolon)
        DW XIHXBYTE
        DW OVER,IHXCRCPLUS
        DW EXIT


;Z IHXWORD  ( tib-ptr -- u tib-ptr )
;    IHXBYTE IHXBYTE       ( u1 u2 tib-ptr )
;    >R                    ( u1 u2 )
;    SWAP 8 LSHIFT +       ( u )
;    R>    ;               ( u tib-ptr )
    head(IHXWORD,IHXWORD,docolon)
        DW IHXBYTE,IHXBYTE
        DW TOR
        DW SWOP,lit,8,LSHIFT,PLUS
        DW RFROM
        DW EXIT

;: IHXREC!   ( count hex-addr tib-ptr -- hex-addr' tib-ptr' )
;    ROT 0 DO
;        IHXBYTE          ( hex-addr hex-byte tib-ptr )
;        >R               ( hex-addr hex-byte )
;        OVER             ( hex-addr hex-byte hex-addr )
;        C!               ( hex-addr )
;        1+               ( hex-addr+1 )
;        R>               ( hex-addr+1 tib-ptr )
;    LOOP ;
    head(IHXRECSTORE,IHXREC!,docolon)
        DW ROT,lit,0,xdo
IHXRECSTORE1:
        DW IHXBYTE
        DW TOR
        DW OVER,CSTORE
        DW ONEPLUS
        DW RFROM
        DW xloop,IHXRECSTORE1
        DW EXIT

;: IHXREC ( c-addr -- 0 if ok, 1 if end, 2 if error  parse hex record )
;    0 IHXCRC !
;    DUP C@ [CHAR] : <> IF  IHXERROR  ( no colon ) EXIT  THEN
;
;    CHAR+
;    IHXBYTE   ( count tib-ptr )
;    IHXWORD   ( count hex-addr tib-ptr )
;    IHXBYTE   ( count hex-addr record-type tib-ptr )
;    SWAP 0=  IF               ( count hex-addr tib-ptr )
;        IHXREC!               ( hex-addr tib-ptr )
;        (IHXBYTE)             ( hex-addr crc tib-ptr )
;        DROP NIP              ( crc )
;        ?IHXCRC IF IHX-OK ELSE IHX-ERROR THEN
;    ELSE
;        DROP DROP DROP
;        IHX-END
;    THEN  ;
    head(IHXREC,IHXREC,docolon)
        DW lit,0,IHXCRC,STORE
        DW DUP,CFETCH,lit,58,NOTEQUAL,qbranch,IHXREC1
        DW lit,2,EXIT

IHXREC1:
        DW CHARPLUS
        DW IHXBYTE,IHXWORD,IHXBYTE

        DW SWOP,ZEROEQUAL,qbranch,IHXREC3
        DW IHXRECSTORE,XIHXBYTE,DROP,NIP

        DW QIHXCRC,qbranch,IHXREC2
        DW lit,0              ; IHXOK
        DW branch,IHXREC4

IHXREC2:
        DW lit,2              ; IHXERROR
        DW branch,IHXREC4

IHXREC3:
        DW DROP,DROP,DROP
        DW lit,1              ; IHXEND
IHXREC4:
        DW EXIT

; \ :07F00000EF7A535F1CE7C922
; \ :00000001FF

; \ :06F00000EF78414FE7C963

;: HEXLOAD
;    ." Waiting for input" CR
;    BASE @ >R HEX
;    [CHAR] : EMIT
;
;    BEGIN
;
;        TIB DUP TIBSIZE ACCEPT
;        DROP                             ( drop count )
;        IHXREC
;
;        DUP IHX-OK = IF
;            [CHAR] # EMIT
;        THEN
;
;    DUP IHX-OK <> UNTIL  ;
;
;    DUP IHX-ERROR = IF
;        ABORT" HEXLOAD ERROR"
;    THEN
;    RFROM BASE ! EXIT ;
    head(HEXLOAD,HEXLOAD,docolon)
        DW XSQUOTE
        DB 17,"Waiting for input"
        DW TYPE,CR
        DW BASE,FETCH,TOR,HEX
        DW lit,58,EMIT

HEXLOAD1:
        DW TIB,DUP,TIBSIZE,FROMACCEPT
        DW DROP
        DW IHXREC

HEXLOAD2:
        DW DUP,lit,0,EQUAL,qbranch,HEXLOAD3
        DW lit,35,EMIT

HEXLOAD3:
        DW DUP,lit,0,NOTEQUAL,qbranch,HEXLOAD1

HEXLOAD4:
        DW DUP,lit,2,EQUAL,qbranch,HEXLOAD5
        DW XSQUOTE
        DB 13,"HEXLOAD ERROR"
        DW TYPE
        DW ABORT

HEXLOAD5:
        DW RFROM,BASE,STORE
        DW EXIT



