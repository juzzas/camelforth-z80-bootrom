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

dnl ;Z S    ( n -- )     select screen n
dnl ;     DUP SCR ! BLOCK DROP ;
dnl     head_editor(S,S,docolon)
dnl         dw DUP,SCR,STORE,BLOCK,DROP
dnl         dw EXIT

dnl ;Z IA   ( column row -- )  insert at column,row
dnl ;     (LINE) + >R 13 WORD COUNT R> SWAP MOVE UPDATE ;
dnl     head_editor(IA,IA,docolon)
dnl         dw XLINE,PLUS,TOR
dnl         dw lit,13,WORD,COUNT,RFROM
dnl         dw SWOP,MOVE,UPDATE
dnl         dw EXIT

dnl ;Z Y   ( n -- )        yank line into PAD
dnl     head_editor(Y,Y,docolon)
dnl         dw XLINE,PAD,ONEPLUS,C_L,MOVE
dnl         dw C_L,PAD,STORE
dnl         dw EXIT

dnl ;Z P   ( n -- )        paste contents of PAD at line n
dnl     head_editor(P,P,docolon)
dnl         dw PAD,COUNT,XLINE,SWOP,MOVE,UPDATE
dnl         dw EXIT

dnl ;Z I   ( n -- )            put text at line n
dnl ;     0 SWAP IA ;
dnl     head_editor(I,I,docolon)
dnl         dw lit,0,SWOP,IA
dnl         dw EXIT

dnl ;Z E   ( n -- )       erase line n
dnl ;    (LINE) C/L BL FILL V* ;
dnl     head_editor(E,E,docolon)
dnl         dw XLINE,C_L,BL,FILL,UPDATE
dnl         dw EXIT

dnl ;Z B   ( -- )
dnl ;     -1 SCR +!
dnl ;     SCR @ 0 BLKLIMIT @
dnl ;     WITHIN INVERT IF
dnl ;        0 SCR !
dnl ;     THEN ;
dnl     head_editor(BEE,B,docolon)
dnl         dw lit,-1,SCR,PLUSSTORE
dnl         dw SCR,FETCH,lit,0,BLKLIMIT,FETCH
dnl         dw WITHIN,INVERT,qbranch,B1
dnl         dw lit,0,SCR,STORE
dnl B1:
dnl         dw EXIT

dnl ;Z N   ( -- )
dnl ;     1 SCR +! ;
dnl ;     SCR @ 0 BLKLIMIT @
dnl ;     WITHIN INVERT IF
dnl ;        BLKLIMIT @ 1- SCR !
dnl ;     THEN ;
dnl     head_editor(N,N,docolon)
dnl         dw lit,1,SCR,PLUSSTORE
dnl         dw SCR,FETCH,lit,0,BLKLIMIT,FETCH
dnl         dw WITHIN,INVERT,qbranch,N1
dnl         dw BLKLIMIT,FETCH,ONEMINUS,SCR,STORE
dnl N1:
dnl         dw EXIT

dnl ;Z L   ( -- )
dnl ;     SCR @ LIST ;
dnl     head_editor(L,L,docolon)
dnl         dw SCR,FETCH,LIST
dnl         dw EXIT


;  RC2014 Memdump =========================

dnl ;Z (MEMDUMP)  ( addr u --    memory dump line )
dnl ;     DUP >R
dnl ;     OVER 4 .W ." : "
dnl ;     0 DO DUP I + C@ 2 .W LOOP
dnl ;     DROP R>
dnl ;     SPACE     ( addr u )
dnl ;     TYPE$  ;
XMEMDUMP:
         call docolon
         dw DUP,TOR
         dw OVER,lit,4,UDOTR,XSQUOTE
         db 2,": "
         dw TYPE
         dw lit,0,xdo
 XMEMDUMP1:
         dw DUP,II,PLUS,CFETCH,lit,3,UDOTR
         dw xloop,XMEMDUMP1
         dw DROP,RFROM,SPACE
         dw TYPESTRING,EXIT


dnl ;Z MEMDUMP  ( addr u --    memory dump utility )
dnl ;     BASE @ >R HEX
dnl ;     OVER +               ( addr addr+u )
dnl ;     SWAP                 ( addr+u  addr )
dnl ;     DO I DUP 16 CR (MEMDUMP) 16 +LOOP
dnl ;     R> BASE ! ;
     head_utils(MEMDUMP,MEMDUMP,docolon)
         dw BASE,FETCH,TOR,HEX
         dw OVER,PLUS,SWOP
         dw xdo
 MEMDUMP1:
         dw II,DUP,lit,16,CR,XMEMDUMP,lit,16,xplusloop,MEMDUMP1
         DW CR
         dw RFROM,BASE,STORE
         dw EXIT


