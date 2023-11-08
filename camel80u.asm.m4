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

SECTION code_user_16k

;  RC2014 simple editor =========================

;Z .BLOCK  ( --  block status )
;    ." Screen: " SCR @ DUP . UPDATED? 43 + EMIT SPACE ;
    head(DOTBLOCK,.BLOCK,docolon)
        dw XSQUOTE
        db 8,"Screen: "
        dw TYPE
        dw SCR,FETCH,DUP,DOT
        dw UPDATEDQ,lit,43,PLUS,EMIT,SPACE
        dw EXIT

;Z DASHES  ( n -- )    print n dashes
;    0 DO 45 EMIT LOOP ;
    head(DASHES,DASHES,docolon)
        dw lit,0,xdo
DASHES1:
        dw lit,45,EMIT
        dw xloop,DASHES1
        dw EXIT

;Z ---  ( n -- )    print border
;    3 SPACES 64 DASHES CR ;
    head(DASHDASHDASH,---,docolon)
        dw lit,3,SPACES
        dw lit,64,DASHES
        dw CR
        dw EXIT

; Z VB  ( -- )    visual list
;     --- SCR @ BLOCK (LIST) DROP --- ;
    head(VB,VB,docolon)
        dw DASHDASHDASH
        dw SCR,FETCH,BLOCK,XLIST,DROP
        dw DASHDASHDASH
        dw EXIT


;Z .STACK  ( -- )    display stack status
;     ." Stack: " .S ;
    head(DOTSTACK,.STACK,docolon)
        dw XSQUOTE
        db 7,"Stack: "
        dw TYPE
        dw DOTS
        dw EXIT


;Z STATUS  ( -- )    status line
;     .BLOCK .STACK ;
    head(STATUS,STATUS,docolon)
        dw DOTBLOCK
        dw DOTSTACK
        dw EXIT

;Z V    ( -- )      visual list
;     CR VB STATUS ;
    head(VEE,V,docolon)
        dw CLS,lit,0,DUP,AT_XY,VB,STATUS
        dw EXIT

;Z V*   ( -- )      visual list update
;     UPDATE V ;
    head(VSTAR,V*,docolon)
        dw UPDATE,VEE
        dw EXIT

;Z S    ( n -- )     select screen n
;     DUP SCR ! BLOCK DROP V ;
    head(S,S,docolon)
        dw DUP,SCR,STORE,BLOCK,DROP,VEE
        dw EXIT

;Z IA   ( column row -- )  insert at column,row
;     (LINE) + >R 13 WORD COUNT R> SWAP MOVE V* ;
    head(IA,IA,docolon)
        dw XLINE,PLUS,TOR
        dw lit,13,WORD,COUNT,RFROM
        dw SWOP,MOVE,VSTAR
        dw EXIT

;Z P   ( n -- )       place at line n
;     0 SWAP IA ;
    head(P,P,docolon)
        dw lit,0,SWOP,IA
        dw EXIT

;Z D   ( n -- )       delete line n
;    (LINE) C/L BL FILL V* ;
    head(D,D,docolon)
        dw XLINE,C_L,BL,FILL,VSTAR
        dw EXIT

;Z X   ( -- )
;     (BLOCK) L/B C/L * BL FILL V* ;
    head(X,X,docolon)
        dw XBLOCK,L_B,C_L,STAR,BL,FILL,VSTAR
        dw EXIT

;Z B   ( -- )
;     -1 SCR +! V ;
    head(B,B,docolon)
        dw lit,-1,SCR,PLUSSTORE,VEE
        dw EXIT

;Z N   ( -- )
;     1 SCR +! V ;
    head(N,N,docolon)
        dw lit,1,SCR,PLUSSTORE,VEE
        dw EXIT

;: E SCR @ LOAD ;

;  RC2014 Full screen editor =========================

;: !XY ( i -- i ) 1023 AND DUP C/L /MOD 3 2 D+ AT-XY ;
    head(STOREXY,!XY,docolon)
        dw lit,1023,AND,DUP,C_L,SLASHMOD,lit,3,lit,1,DPLUS,AT_XY
        dw EXIT

;: !CH ( c i -- c i ) 2DUP SCR @ BLOCK + C! UPDATE OVER EMIT ;
    head(STORECH,!CH,docolon)
        dw TWODUP,SCR,FETCH,BLOCK,PLUS,CSTORE,UPDATE
        dw OVER,EMIT
        dw EXIT

;: ?CH ( c i -- c i' ( VIM like controls )
;    OVER BL - 95 U< IF !CH 1+ EXIT THEN ( text )
;    OVER 8 = IF 1- THEN ( left ^h )
;    OVER 12 = IF 1+ THEN ( right ^l )
;    OVER 11 = IF C/L - THEN ( up ^k )
;    OVER 10 = IF C/L + THEN ( down ^j )
;    OVER 13 = IF C/L 2DUP MOD - + THEN ( crlf return )
;    OVER 127 = IF 1- THEN ( left delete )
;    OVER 2 = IF B L THEN ( back ^b )
;    OVER 14 = IF N L THEN ( nextscr ^n ) ;
    head(QCH,?CH,docolon)
        dw OVER,BL,MINUS,lit,95,ULESS,qbranch,QCH1
        dw STORECH,ONEPLUS
QCH1:
        dw OVER,lit,8,EQUAL,qbranch,QCH2
        dw ONEMINUS
QCH2:
        dw OVER,lit,12,EQUAL,qbranch,QCH3
        dw ONEPLUS
QCH3:
        dw OVER,lit,11,EQUAL,qbranch,QCH4
        dw C_L,MINUS
QCH4:
        dw OVER,lit,10,EQUAL,qbranch,QCH5
        dw C_L,PLUS
QCH5:
        dw OVER,lit,13,EQUAL,qbranch,QCH6
        dw C_L,TWODUP,MOD,MINUS,PLUS
QCH6:
        dw OVER,lit,127,EQUAL,qbranch,QCH7
        dw ONEMINUS
QCH7:
;        dw OVER,lit,2,EQUAL,qbranch,QCH8
;        dw B,L
;QCH8:
;        dw OVER,lit,14,EQUAL,qbranch,QCH9
;        dw N,L
;QCH9:
        dw EXIT


;: EDIT ( n -- ) CLS 0 DUP AT-XY LIST 0
;    BEGIN !XY KEY SWAP ?CH SWAP 27 = UNTIL DROP L ;
    head(EDIT,EDIT,docolon)
        DW S
        dw lit,0
EDIT1:
        dw STOREXY,KEY,SWOP,QCH,SWOP,lit,27,EQUAL,qbranch,EDIT1
        dw DROP,VEE
        dw EXIT


;  RC2014 Memdump =========================

;Z (MEMDUMP)  ( addr u --    memory dump lin )
;     DUP >R
;     OVER 4 .W ." : "
;     0 DO DUP I + C@ 2 .W LOOP
;     DROP R>
;     SPACE
;     0 DO DUP I + C@
;         DUP PRINTABLE? INVERT IF DROP [CHAR] . THEN EMIT
;     LOOP
;     DROP ;
    head(XMEMDUMP,(MEMDUMP),docolon)
        dw DUP,TOR
        dw OVER,lit,4,UDOTW,XSQUOTE
        db 2,": "
        dw TYPE
        dw lit,0,xdo
XMEMDUMP1:
        dw DUP,II,PLUS,CFETCH,lit,2,DOTW
        dw xloop,XMEMDUMP1
        dw DROP,RFROM,SPACE
        dw lit,0,xdo
XMEMDUMP2:
        dw DUP,II,PLUS,CFETCH
        dw DUP,PRINTABLEQ,INVERT,qbranch,XMEMDUMP3
        dw DROP,lit,46
XMEMDUMP3:
        dw EMIT
        dw xloop,XMEMDUMP2
        dw DROP,EXIT


;Z MEMDUMP  ( addr u --    memory dump utility )
;     BASE @ >R HEX
;     OVER +               ( addr addr+u )
;     SWAP                 ( addr+u  addr )
;     DO I DUP 16 CR (MEMDUMP) 16 +LOOP
;     R> BASE ! ;
    head(MEMDUMP,MEMDUMP,docolon)
        dw BASE,FETCH,TOR,HEX
        dw OVER,PLUS,SWOP
        dw xdo
MEMDUMP1:
        dw II,DUP,lit,16,CR,XMEMDUMP,lit,16,xplusloop,MEMDUMP1
        dw RFROM,BASE,STORE
        dw EXIT


