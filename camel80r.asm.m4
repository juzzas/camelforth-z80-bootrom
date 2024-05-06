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

EXTERN asm_z80_delay_ms
EXTERN asm_z80_delay_tstate

;Z RAMTOP      -- a-addr   address of first USER reserved byte
;  ramtop_ptr CONSTANT RAMTOP
    head(RAMTOP,RAMTOP,docon)
        dw ramtop_ptr

;Z INTVEC      -- a-addr   pointer to address holding interrupt vector
;  intvec_ptr CONSTANT INTVEC
    head(INTVEC,INTVEC,docon)
        dw intvec_ptr

;Z RST30VEC     -- a-addr   pointer to address holding RST30 vector
;  intvec_ptr CONSTANT RST30VEC
    head(RST30VEC,RST30VEC,docon)
        dw rst30vec_ptr

SECTION data_user
ramtop_ptr:
        DEFW 0

intvec_ptr:
        DEFW 0

rst30vec_ptr:
        DEFW 0

SECTION code_user


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


; HEXLOAD implementation ==========================

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
;    BASE @ >R HEX
;    >R 0 S>D R> 2
;    >NUMBER       ( du tib-ptr u )
;    DROP NIP      ( u tib-ptr )
;    R> BASE !    ;
    head(XIHXBYTE,(IHXBYTE),docolon)
        DW BASE,FETCH,TOR,HEX
        DW TOR,lit,0,STOD,RFROM,lit,2
        DW TONUMBER
        DW DROP,NIP
        DW RFROM,BASE,STORE
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



