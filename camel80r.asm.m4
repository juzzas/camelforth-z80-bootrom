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


SECTION code_user

;Z RAMTOP      -- a-addr   address of first USER reserved byte
;  ramtop_ptr CONSTANT RAMTOP
    head(RAMTOP,RAMTOP,docon)
        dw ramtop_ptr

SECTION data_user

ramtop_ptr:
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
        ld sp, USER_STACK_TOP    ; end of RAM

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
;Z \   (  --     comment to end of line )
;   13 WORD DROP ;
dnl   head(BACKSLASH,``\'',docolon)
    ; macro doesn't like the \ character, so manually build this word
    DW CALL_link
    DB 0
BACKSLASH_link:
    define(`link', `BACKSLASH_link')
    DEFM 1,0x5c
BACKSLASH:
    call docolon
    DW lit,13,WORD,DROP
    DW EXIT

;Z DUMP  ( caddr len -- )
;   OVER + SWAP DO I C@ . LOOP ;
    head(DUMP,DUMP,docolon)
        DW OVER,PLUS,SWOP,xdo
DUMP1:
        DW II,CFETCH,DOT,xloop,DUMP1
        DW EXIT

; HEXLOAD implementation ==========================

;Z IHXCRC+  ( c -- )
;    IHXCRC @ + FF AND IHXCRC ! ;
IHXCRCPLUS:
        call docolon
        DW IHXCRC,FETCH,PLUS,IHXCRC,STORE
        DW EXIT

;Z ?IHXCRC  ( c -- flag )    ( does the crc match? )
;    IHXCRC @ NEGATE FF AND = ;
QIHXCRC:
        call docolon
        DW IHXCRC,FETCH,NEGATE,lit,255,AND,EQUAL
        DW EXIT

;Z (IHXBYTE)  ( tib-ptr -- u tib-ptr )
;    BASE @ >R HEX
;    >R 0 S>D R> 2
;    >NUMBER       ( du tib-ptr u )
;    DROP NIP      ( u tib-ptr )
;    R> BASE !    ;
XIHXBYTE:
        call docolon
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
IHXBYTE:
        call docolon
        DW XIHXBYTE
        DW OVER,IHXCRCPLUS
        DW EXIT


;Z IHXWORD  ( tib-ptr -- u tib-ptr )
;    IHXBYTE IHXBYTE       ( u1 u2 tib-ptr )
;    >R                    ( u1 u2 )
;    SWAP 8 LSHIFT +       ( u )
;    R>    ;               ( u tib-ptr )
IHXWORD:
        call docolon
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
IHXRECSTORE:
        call docolon
        DW ROT,lit,0,xdo
IHXRECSTORE1:
        DW IHXBYTE
        DW TOR
        DW OVER,CSTORE
        DW ONEPLUS
        DW RFROM
        DW xloop,IHXRECSTORE1
        DW EXIT

;Z IHXCRC       -- a-addr  location for current HEXLOAD CRC
;  ihxcrc_ptr CONSTANT IHXCRC
IHXCRC:
        call docon
        dw ihxcrc_ptr

;: IHEX? ( addr len -- src dest n -1   if ok, 0 if not recognised, 1 if end )
;    DROP
;    0 IHXCRC !
;    DUP C@ [CHAR] : <> IF DROP 0  ( no colon ) EXIT  THEN
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
;           1 EXIT   ( flag end of hex )
;        ELSE
;           0 EXIT   ( flag not recognised )
;        THEN
;    THEN
;
;    ?IHXCRC IF
;        PAD CELL+ CELL+        ( src )
;        PAD @                  ( src dest )
;        PAD CELL+ @ -1         ( src dest n -1 )
;    ELSE
;        0   ( flag not recognised )
;    THEN  ;
    head(IHEXQ,IHEX?,docolon)
        DW DROP
        DW lit,0,IHXCRC,STORE
        DW DUP,CFETCH,lit,58,NOTEQUAL,qbranch,IHEXQ1
        DW DROP,lit,0,EXIT

IHEXQ1:
        DW CHARPLUS
        DW IHXBYTE,OVER,PAD,CELLPLUS,STORE
        DW IHXWORD,SWOP,PAD,STORE
        DW PAD,CELLPLUS,CELLPLUS,SWOP
        DW IHXBYTE

        DW OVER,ZEROEQUAL,qbranch,IHEXQ2
        DW NIP,IHXRECSTORE,XIHXBYTE,DROP,NIP
        DW branch,IHEXQ3
IHEXQ2:
        DW DROP,NIP,NIP
        DW lit,1,EQUAL,qbranch,IHEXQ2a
        DW lit,1,EXIT
IHEXQ2a:
        DW lit,0,EXIT

IHEXQ3:
        DW QIHXCRC,qbranch,IHEXQ4
        DW PAD,CELLPLUS,CELLPLUS
        DW PAD,FETCH
        DW PAD,CELLPLUS,FETCH,lit,-1
        DW EXIT
IHEXQ4:
        DW lit,0,EXIT

; (IHEX)                   ( src dest len -- runtime action )
;     IHEX_START @ 0= IF OVER IHEX_START ! THEN
;     2DUP + IHEX_START @ - IHEX_LENGTH !
;     MOVE    ;
XIHEX:
        call docolon
        DW lit,ihex_start,FETCH,ZEROEQUAL,qbranch,XIHEX1
        DW OVER,lit,ihex_start,STORE
XIHEX1:
        DW TWODUP,PLUS,lit,ihex_start,FETCH,MINUS,lit,ihex_length,STORE
        DW MOVE
        DW EXIT

;  NONAME:    ( src dest len --     compile action for ihex )
IHEXCOMMA:
        call docolon
        DW SWOP,LITERAL,SLITERAL
        DW lit,ROT,COMMA
        DW lit,SWOP,COMMA
        DW lit,XIHEX,COMMA
        DW EXIT

;: HEXLOAD
;    0 IHEX_START !
;    0 IHEX_LENGTH !   ;
    head(HEXLOAD,HEXLOAD,docolon)
        DW lit,0,lit,ihex_start,STORE
        DW lit,0,lit,ihex_length,STORE
        DW EXIT

;: ;HEXLOAD     ( -- ihex_start ihex_length )
;    IHEX_START @
;    IHEX_LENGTH @   ;
    head(SEMIHEXLOAD,;HEXLOAD,docolon)
        DW lit,ihex_start,FETCH
        DW lit,ihex_length,FETCH
        DW EXIT

SECTION data_user

ihxcrc_ptr:
        DW 0

ihex_start:
        DW 0

ihex_length:
        DW 0

SECTION code_user



