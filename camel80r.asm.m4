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
;     R> BLOCK ;
    head(LOAD,LOAD,docolon)
        dw BLK,FETCH,TOR
        dw DUP,lit,0,EQUAL,qbranch,LOAD1
        dw BLOCK,DROP,branch,LOAD2
LOAD1:
        dw BLOCK,B_BLK,EVALUATE
LOAD2:
        dw RFROM,BLOCK
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

; HEXLOAD implementation ==========================


;X HEXLOAD           --    call Hexloader
    head(HEXLOAD,HEXLOAD,docolon)
        dw lit,_hexload,CALL
        dw EXIT


