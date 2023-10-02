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

EXTERN _hexload

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

; ===============================================

;X HEXLOAD           --    call Hexloader
    head(HEXLOAD,HEXLOAD,docolon)
        dw lit,_hexload,CALL
        dw EXIT

; RC2014 EXTENSION CONSTANTS ====================

;C C/L      -- n         columns per line
    head(C_L,C/L,docon)
        dw 64

;C B/BLK      -- n       bytes per block
    head(B_BLK,B/BLK,docon)
        dw 1024

; BLOCK implementation ==========================

;Z BLKFIRST      -- a-adrs      address of first block buffer
    head(BLKFIRST,BLKFIRST,docon)
        dw 0x8800

;Z BLOCK-READ  --  Compact Flash read block  BLK and DSK
; Reads the block from the Compact Flash card into memory
; address found at 'adrs'. 'dks' and 'blk' are the disk
; and block numbers respectively
    head(BLOCK_READ,BLOCK-READ,docolon)
        dw DSK,FETCH,BLK,FETCH,BLKBUFFER,FETCH
        dw lit,cflash_read_block,CALL
        dw EXIT

;Z BLOCK-WRITE  --  Compact Flash read write BLK and DSK
; Reads the block from the Compact Flash card into memory
; address found at 'adrs'. 'dks' and 'blk' are the disk
; and block numbers respectively
    head(BLOCK_WRITE,BLOCK-WRITE,docolon)
        dw DSK,FETCH,BLK,FETCH,BLKBUFFER,FETCH
        dw lit,cflash_write_block,CALL
        dw EXIT

;C BUFFER        n -- addr         push buffer address
;     BLKFIRST BLKBUFFER !
;     BLKUPDATE @ IF BLOCK-WRITE  0 BLKUPDATE ! THEN
;     BLK !
;     BLKBUFFER @      ( push buffer address ) ;
    head(BUFFER,BUFFER,docolon)
        dw BLKFIRST,BLKBUFFER,STORE
        dw BLKUPDATE,FETCH,qbranch,BUFF1
        dw BLOCK_WRITE,lit,0,BLKUPDATE,STORE
BUFF1:
        dw BLK,STORE
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

;C FLUSH                    --    flush blocks to disk
;     BLKUPDATE @ IF BLOCK-WRITE  0 BLKUPDATE ! THEN ;
    head(FLUSH,FLUSH,docolon)
        dw BLKUPDATE,FETCH,qbranch,FLUSH1
        dw BLOCK_WRITE,lit,0,BLKUPDATE,STORE
FLUSH1:
        dw EXIT

;C LOAD                  n  --    load block n
;     BLK @ >R
;     BLOCK B/BLK INTERPRET
;     R> BLOCK ;
    head(LOAD,LOAD,docolon)
        dw BLK,FETCH,TOR
        dw BLOCK,B_BLK,EVALUATE
        dw RFROM,BLOCK
        dw EXIT

; RC2014 EXTENSIONS (TERMINAL) ==================

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

;Z .W
;    (.W) TYPE SPACE ;
    head(DOTW,.W,docolon)
        dw XDOTW,TYPE,SPACE
        dw EXIT

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

;Z PRINTABLE?         ( n - flag  is characte printable? )
;    20 7F WITHIN ;
    head(PRINTABLEQ,PRINTABLE?,docolon)
        dw lit,0x20,lit,0x7f,WITHIN
        dw EXIT
