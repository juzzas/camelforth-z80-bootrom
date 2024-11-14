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


SECTION code

;Z RAMTOP      -- a-addr   address of first USER reserved byte
;  ramtop_ptr CONSTANT RAMTOP
    head(RAMTOP,RAMTOP,docon)
        dw ramtop_ptr

SECTION data

ramtop_ptr:
        DEFS 2


SECTION code
EXTERN jp_hl

;Z CALL       a-addr --    call machine code at address
    head(CALL,CALL,docode)
        ; protect against some stack abuse
        ld (forth_stack_save), sp
        ld sp, forth_state_stack_top
        push ix
        push iy
        push de
        ld (forth_state_stack_save), sp

        ; set up user stack
        ld sp, USER_STACK_TOP    ; end of RAM

        ld hl,bc
        call jp_hl

call_exit:
        ld sp, (forth_state_stack_save)
        pop de
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; forth_callxt --  call Forth XT in HL
PUBLIC forth_callxt
forth_callxt:
        ld (user_stack_save), sp

        ; get snapshot of Forth state
        ld sp, (forth_state_stack_save)
        pop de
        pop iy
        pop ix

        ld sp, (forth_stack_save)
        pop bc   ; fill BC with TOS

        ; set up a new forth thread, xt in hl
        ld de, callxt_exit_xt
        jp (hl)              ; jump to xt

callxt_exit:
        push bc    ; save current TOS
        ld (forth_stack_save), sp

        ld sp, (user_stack_save)
        ret

; simple forth thread to jump back to exit
callxt_exit_xt:
        DEFW  callxt_exit

SECTION data

forth_stack_save:
        DEFS 2

user_stack_save:
        DEFS 2

forth_state_stack:
        DEFS 16
forth_state_stack_top:

forth_state_stack_save:
        DEFS 2

SECTION code

; RC2014 EXTENSION misc ======================
;Z \   (  --     comment to end of line )
;   SOURCE >IN ! DROP ;   IMMEDIATE
dnl   head(BACKSLASH,``\'',docolon)
    ; macro doesn't like the \ character, so manually build this word
    DW CALL_link
    DB 1
BACKSLASH_link:
    define(`link', `BACKSLASH_link')
    DEFM 1,0x5c
BACKSLASH:
    call docolon
    DW SOURCE,TOIN,STORE,DROP
    DW EXIT

;C .(    --                     emit input until )
;   [ HEX ] 29 WORD COUNT TYPE ; IMMEDIATE
    immed(DOTPAREN,``.('',docolon)
        DW lit,29H,WORD,COUNT,TYPE,EXIT

;X SLITERAL    c-addr u --    compile string literal
;    ['] (S") ,XT
;    DUP C,     ( store size )
;    HERE   OVER ALIGNED ALLOT
;    SWAP MOVE
;   ; IMMEDIATE
    immed(SLITERAL,SLITERAL,docolon)
        DW lit,XSQUOTE,COMMAXT
        DW DUP,CCOMMA
        DW HERE,OVER,ALIGNED,ALLOT
        DW SWOP,MOVE
        DW EXIT


;Z DUMP  ( caddr len -- )
;   OVER + SWAP DO I C@ . LOOP ;
    head(DUMP,DUMP,docolon)
        DW OVER,PLUS,SWOP,xdo
DUMP1:
        DW II,CFETCH,DOT,xloop,DUMP1
        DW EXIT

;Z AT-XY  ( x y -- move cursor to x,y )
;    VT-ESC 1+ (.) TYPE ." ;" 1+ (.) TYPE ." H" ;
    head(AT_XY,AT-XY,docolon)
        dw lit,0x1b,EMIT
        dw lit,'[',EMIT
        dw ONEPLUS,XDOT,TYPE
        dw lit,';',EMIT
        dw ONEPLUS,XDOT,TYPE
        dw lit,'H',EMIT
        dw EXIT

;Z PAGE  ( --  clear screen )
;    VT-ESC ." 2J"
    head(PAGE,PAGE,docolon)
        dw XSQUOTE
        db 4,0x1b,"[2J"
        dw TYPE
        dw lit,0,lit,0,AT_XY
        dw EXIT


; HEXLOAD implementation ==========================

;Z IHXCRC+  ( c -- )
;    IHXCRC +! ;
IHXCRCPLUS:
        call docolon
        DW IHXCRC,PLUSSTORE
        DW EXIT

;Z ?IHXCRC  ( c -- flag )    ( does the crc match? )
;    IHXCRC @ NEGATE FF AND = ;
QIHXCRC:
        call docolon
        DW IHXCRC,FETCH,NEGATE,lit,255,AND,EQUAL
        DW EXIT



;Z (IHXBYTE)  ( tib-ptr -- c tib-ptr' )
XIHXBYTE:
        call readnibble ; read the first nibble
        rlca            ; shift it left by 4 bits
        rlca
        rlca
        rlca
        ld l,a          ; temporarily store the first nibble in L
        call readnibble ; get the second (low) nibble
        or l            ; assemble two nibbles into one byte in A
        ld l,a          ; put assembled byte back into L
        ld h,0
        push hl         ; TUCK hl 
        next            ;

    readnibble:
        ld a,(bc)
        inc bc
        sub '0'
        cp 10
        ret C           ; if A<10 just return
        sub 7           ; else subtract 'A'-'0' (17) and add 10
        ret


;Z IHXBYTE  ( tib-ptr -- u tib-ptr )
;    (IHXBYTE)
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
XIHEXQ:
        call docolon
        DW DROP
        DW lit,0,IHXCRC,STORE
        DW DUP,CFETCH,lit,58,NOTEQUAL,qbranch,XIHEXQ1
        DW DROP,lit,0,EXIT

XIHEXQ1:
        DW CHARPLUS
        DW IHXBYTE,OVER,PAD,CELLPLUS,STORE
        DW IHXWORD,SWOP,PAD,STORE
        DW PAD,CELLPLUS,CELLPLUS,SWOP
        DW IHXBYTE

        DW OVER,ZEROEQUAL,qbranch,XIHEXQ2
        DW NIP,IHXRECSTORE,XIHXBYTE,DROP,NIP
        DW branch,XIHEXQ3
XIHEXQ2:
        DW DROP,NIP,NIP
        DW lit,1,EQUAL,qbranch,XIHEXQ2a
        DW lit,1,EXIT
XIHEXQ2a:
        DW lit,0,EXIT

XIHEXQ3:
        DW QIHXCRC,qbranch,XIHEXQ4
        DW PAD,CELLPLUS,CELLPLUS
        DW PAD,FETCH
        DW PAD,CELLPLUS,FETCH,lit,-1
        DW EXIT
XIHEXQ4:
        DW lit,0,EXIT


;Z IHEX?
;    ['] (IHEXQ?) CATCH
;       0<> IF  0  THEN  EXIT  ;

    head(IHEXQ,IHEX?,docolon)
        DW lit,XIHEXQ,CATCH
        DW qbranch,IHEXQ1
        DW lit,0,EXIT

IHEXQ1:
        DW EXIT



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

SECTION data

ihxcrc_ptr:
        DEFS 2

ihex_start:
        DEFS 2

ihex_length:
        DEFS 2




