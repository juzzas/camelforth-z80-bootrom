; RC2014 Utility Words
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
; RC2014 Utility Words
; 
;   Forth words are documented as follows:
;*   NAME     stack -- stack    description
;   Word names in upper case are from the ANS
;   Forth Core word set.  Names in lower case are
;   "internal" implementation words & extensions.
; ===============================================

EXTERN _hexload

SECTION code_16k

; RC2014 basic line editor

;Z S    ( n -- )     select screen n
;     DUP SCR ! BLOCK DROP ;
    head_editor(S,S,docolon)
        dw DUP,SCR,STORE,BLOCK,DROP
        dw EXIT

;Z IA   ( column row -- )  insert at column,row
;     (LINE) + >R 13 WORD COUNT R> SWAP MOVE UPDATE ;
    head_editor(IA,IA,docolon)
        dw XLINE,PLUS,TOR
        dw lit,13,WORD,COUNT,RFROM
        dw SWOP,MOVE,UPDATE
        dw EXIT

;Z Y   ( n -- )        yank line into PAD
    head_editor(Y,Y,docolon)
        dw XLINE,PAD,ONEPLUS,C_L,MOVE
        dw C_L,PAD,STORE
        dw EXIT

;Z P   ( n -- )        paste contents of PAD at line n
    head_editor(P,P,docolon)
        dw PAD,COUNT,XLINE,SWOP,MOVE,UPDATE
        dw EXIT

;Z I   ( n -- )            put text at line n
;     0 SWAP IA ;
    head_editor(I,I,docolon)
        dw lit,0,SWOP,IA
        dw EXIT

;Z E   ( n -- )       erase line n
;    (LINE) C/L BL FILL V* ;
    head_editor(E,E,docolon)
        dw XLINE,C_L,BL,FILL,UPDATE
        dw EXIT

;Z B   ( -- )
;     -1 SCR +!
;     SCR @ 0 BLKLIMIT @
;     WITHIN INVERT IF
;        0 SCR !
;     THEN ;
    head_editor(BEE,B,docolon)
        dw lit,-1,SCR,PLUSSTORE
        dw SCR,FETCH,lit,0,BLKLIMIT,FETCH
        dw WITHIN,INVERT,qbranch,B1
        dw lit,0,SCR,STORE
B1:
        dw EXIT

;Z N   ( -- )
;     1 SCR +! ;
;     SCR @ 0 BLKLIMIT @
;     WITHIN INVERT IF
;        BLKLIMIT @ 1- SCR !
;     THEN ;
    head_editor(N,N,docolon)
        dw lit,1,SCR,PLUSSTORE
        dw SCR,FETCH,lit,0,BLKLIMIT,FETCH
        dw WITHIN,INVERT,qbranch,N1
        dw BLKLIMIT,FETCH,ONEMINUS,SCR,STORE
N1:
        dw EXIT

;Z L   ( -- )
;     SCR @ LIST ;
    head_editor(L,L,docolon)
        dw SCR,FETCH,LIST
        dw EXIT


;  RC2014 Memdump =========================

dnl ;Z (MEMDUMP)  ( addr u --    memory dump line )
dnl ;     DUP >R
dnl ;     OVER 4 .W ." : "
dnl ;     0 DO DUP I + C@ 2 .W LOOP
dnl ;     DROP R>
dnl ;     SPACE     ( addr u )
dnl ;     TYPE$  ;
dnl     head(XMEMDUMP,(MEMDUMP),docolon)
dnl         dw DUP,TOR
dnl         dw OVER,lit,4,UDOTW,XSQUOTE
dnl         db 2,": "
dnl         dw TYPE
dnl         dw lit,0,xdo
dnl XMEMDUMP1:
dnl         dw DUP,II,PLUS,CFETCH,lit,2,DOTW
dnl         dw xloop,XMEMDUMP1
dnl         dw DROP,RFROM,SPACE
dnl         dw TYPESTRING,EXIT


dnl ;Z MEMDUMP  ( addr u --    memory dump utility )
dnl ;     BASE @ >R HEX
dnl ;     OVER +               ( addr addr+u )
dnl ;     SWAP                 ( addr+u  addr )
dnl ;     DO I DUP 16 CR (MEMDUMP) 16 +LOOP
dnl ;     R> BASE ! ;
dnl     head(MEMDUMP,MEMDUMP,docolon)
dnl         dw BASE,FETCH,TOR,HEX
dnl         dw OVER,PLUS,SWOP
dnl         dw xdo
dnl MEMDUMP1:
dnl         dw II,DUP,lit,16,CR,XMEMDUMP,lit,16,xplusloop,MEMDUMP1
dnl         dw RFROM,BASE,STORE
dnl         dw EXIT


