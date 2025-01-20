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


