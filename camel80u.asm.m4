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
    head_editor(DOTBLOCK,.BLOCK,docolon)
        dw XSQUOTE
        db 8,"Screen: "
        dw TYPE
        dw SCR,FETCH,DUP,DOT
        dw UPDATEDQ,lit,43,PLUS,EMIT,SPACE
        dw EXIT

;Z RULER  ( --  print ruler )
;    print ruler
RULER:
        call docolon
        dw lit,3,SPACES
        dw RULER1,RULER1,RULER1,RULER1
        dw CR
        dw EXIT

RULER1:
        call docolon
        dw XSQUOTE
        db 16,"+---:---+---:---"
        dw TYPE,EXIT


; Z VB  ( -- )    visual list
;     --- SCR @ BLOCK (LIST) DROP --- ;
    head_editor(VB,VB,docolon)
        dw RULER
        dw SCR,FETCH,BLOCK,XLIST,DROP
        dw RULER
        dw EXIT


;Z .STACK  ( -- )    display stack status
;     ." Stack: " .S ;
    head_editor(DOTSTACK,.STACK,docolon)
        dw XSQUOTE
        db 7,"Stack: "
        dw TYPE
        dw DOTS
        dw EXIT


;Z STATUS  ( -- )    status line
;     .BLOCK .STACK ;
    head_editor(STATUS,STATUS,docolon)
        dw DOTBLOCK
        dw DOTSTACK
        dw EXIT

;Z V    ( -- )      visual list
;     CR VB STATUS ;
    head_editor(VEE,V,docolon)
        dw CLS,lit,0,DUP,AT_XY,VB,STATUS
        dw EXIT

;Z V*   ( -- )      visual list update
;     UPDATE V ;
    head_editor(VSTAR,V*,docolon)
        dw UPDATE,VEE
        dw EXIT

;Z S    ( n -- )     select screen n
;     DUP SCR ! BLOCK DROP V ;
    head_editor(S,S,docolon)
        dw DUP,SCR,STORE,BLOCK,DROP
        dw EXIT

;Z IA   ( column row -- )  insert at column,row
;     (LINE) + >R 13 WORD COUNT R> SWAP MOVE V* ;
    head_editor(IA,IA,docolon)
        dw XLINE,PLUS,TOR
        dw lit,13,WORD,COUNT,RFROM
        dw SWOP,MOVE,UPDATE
        dw EXIT

;Z P   ( n -- )       place at line n
;     0 SWAP IA ;
    head_editor(P,P,docolon)
        dw lit,0,SWOP,IA
        dw EXIT

;Z D   ( n -- )       delete line n
;    (LINE) C/L BL FILL V* ;
    head_editor(D,D,docolon)
        dw XLINE,C_L,BL,FILL,UPDATE
        dw EXIT

;Z X   ( -- )
;     (BLOCK) L/B C/L * BL FILL V* ;
    head_editor(X,X,docolon)
        dw XBLOCK,L_B,C_L,STAR,BL,FILL,UPDATE
        dw EXIT

;Z B   ( -- )
;     -1 SCR +! V ;
    head_editor(BEE,B,docolon)
        dw lit,-1,SCR,PLUSSTORE
        dw EXIT

;Z N   ( -- )
;     1 SCR +! ;
    head_editor(N,N,docolon)
        dw lit,1,SCR,PLUSSTORE
        dw EXIT

;Z L   ( -- )
;     SCR @ LIST ;
    head_editor(L,L,docolon)
        dw SCR,FETCH,LIST
        dw EXIT

;: E SCR @ LOAD ;

;  RC2014 Full screen editor =========================

;: !XY ( i -- i ) 1023 AND DUP C/L /MOD 3 2 D+ AT-XY ;
    head_editor(STOREXY,!XY,docolon)
        dw lit,1023,AND,DUP,C_L,SLASHMOD,lit,3,lit,1,DPLUS,AT_XY
        dw EXIT

;: !CH ( c i -- c i ) 2DUP SCR @ BLOCK + C! UPDATE OVER EMIT ;
    head_editor(STORECH,!CH,docolon)
        dw TWODUP,SCR,FETCH,BLOCK,PLUS,CSTORE,UPDATE
        dw OVER,EMIT
        dw EXIT

;: 'I    ( i -- c-addr )
;    SCR,FETCH,BLOCK,PLUS  ;
    head_editor(TICKI,'I,docolon)
        dw SCR,FETCH,BLOCK,PLUS
        dw EXIT

; INSERT_CHAR    ( i -- i )
;    1023 AND DUP             ( i i )
;    'I DUP 1+          ( i c-addr c-addr+1 )
;    DUP C/L MOD C/L SWAP -  ( i c-addr c-addr+1 n  )
;    MOVE           ( i )
;    BL OVER !CH 2DROP   ( i )
;    DUP C/L /      ( i l )
;    13 EMIT  DUP 2 DOTR SPACE LL     ( i )
;    !XY   ;
INSERT_CHAR:
        call docolon
        dw lit,1023,AND,DUP
        dw TICKI,DUP,ONEPLUS
        dw DUP,C_L,MOD,C_L,SWOP,MINUS
        dw MOVE
        dw BL,OVER,STORECH,TWODROP
        dw DUP,C_L,SLASH
        dw lit,13,EMIT,DUP,lit,2,DOTR,SPACE,LL
        dw STOREXY
        dw EXIT

; DELETE_CHAR  ( i -- i )
;    1023 AND DUP             ( i i )
;    'I DUP 1+ SWAP         ( i c-addr+1 c-addr )
;    OVER C/L MOD C/L SWAP -  ( i c-addr+1 c-addr n  )
;    MOVE           ( i )
;    BL OVER C/L 1- OR !XY !CH 2DROP   ( i )
;    DUP C/L /      ( i l )
;    13 EMIT  DUP 2 DOTR SPACE LL     ( i )
;    !XY   ;
DELETE_CHAR:
        call docolon
        dw lit,1023,AND,DUP
        dw TICKI,DUP,ONEPLUS,SWOP
        dw OVER,C_L,MOD,C_L,SWOP,MINUS
        dw MOVE
        dw BL,OVER,C_L,ONEMINUS,OR,STOREXY,STORECH,TWODROP
        dw DUP,C_L,SLASH
        dw lit,13,EMIT,DUP,lit,2,DOTR,SPACE,LL
        dw STOREXY
        dw EXIT


; DELETE_LINE  ( i -- i )
;    1023 AND DUP 0xffc0 AND 'I ( i 'i-sol )
;    DUP C/L +                ( i 'i-sol 'i-nl )
;    SWAP                     ( i 'i-nl 'i-sol )
;    OVER 1023 AND B/BLK SWAP -        ( i 'i-nl 'i-sol #n )
;    MOVE
;    B/BLK C/L - 'I C/L BL FILL  ;   ( i )
DELETE_LINE:
        call docolon
        dw lit,1023,AND,DUP,lit,0xffc0,AND,TICKI
        dw DUP,C_L,PLUS
        dw SWOP
        dw OVER,lit,1023,AND,B_BLK,SWOP,MINUS
        dw MOVE
        dw B_BLK,C_L,MINUS,TICKI,C_L,BL,FILL
        dw EXIT

; INSERT_LINE  ( i -- i )
;    1023 AND DUP 0xffc0 AND 'I  ( i 'i-sol )
;    DUP >R
;    DUP C/L +                ( i 'i-sol 'i-nl )
;    OVER 1023 AND B/BLK SWAP -        ( i 'i-nl 'i-sol #n )
;    MOVE
;    R> C/L BL FILL  ;   ( i )
INSERT_LINE:
        call docolon
        dw lit,1023,AND,DUP,lit,0xffc0,AND,TICKI
        dw DUP,TOR
        dw DUP,C_L,PLUS
        dw OVER,lit,1023,AND,B_BLK,SWOP,MINUS
        dw MOVE
        dw RFROM,C_L,BL,FILL
        dw EXIT

;: ?CH ( c i -- c i' ( VIM like controls )
;    OVER BL - 95 U< IF !CH 1+ EXIT THEN ( text )
;    OVER 8 = IF 1- THEN ( left ^h )
;    OVER 19 = IF 1- THEN ( left ^s )
;    OVER 4 = IF 1+ THEN ( right ^d )
;    OVER 5 = IF C/L - THEN ( up ^e )
;    OVER 24 = IF C/L + THEN ( down ^x )
;    OVER 13 = IF C/L 2DUP MOD - + THEN ( crlf return )
;    OVER 127 = IF 1- THEN ( left delete )
;    OVER 18 = IF B  >R >R  V  R> R>  THEN ( back ^r )
;    OVER 3 = IF N  >R >R  V  R> R>  THEN ( nextscr ^c ) ;
;    OVER 22 = IF insert_char THEN ( insert space ^v )
;    OVER 7 = IF delete_char THEN ( delete char ^g )
;    OVER 25 = IF cut_line THEN ( cut line, shift up ^y ) ;
;    OVER 15 = IF insert_line THEN ( shift down, empty line ^o ) ;
;    OVER 16 = IF paste_line THEN ( shift down, paste line ^p ) ;
    head_editor(QCH,?CH,docolon)
        dw OVER,BL,MINUS,lit,95,ULESS,qbranch,QCH1
        dw STORECH,ONEPLUS
QCH1:
        dw OVER,lit,8,EQUAL,qbranch,QCH2
        dw ONEMINUS,BL,OVER,STOREXY,STORECH,TWODROP
QCH2:
        dw OVER,lit,19,EQUAL,qbranch,QCH3
        dw ONEMINUS
QCH3:
        dw OVER,lit,4,EQUAL,qbranch,QCH4
        dw ONEPLUS
QCH4:
        dw OVER,lit,5,EQUAL,qbranch,QCH5
        dw C_L,MINUS
QCH5:
        dw OVER,lit,24,EQUAL,qbranch,QCH6
        dw C_L,PLUS
QCH6:
        dw OVER,lit,13,EQUAL,qbranch,QCH7
        dw C_L,TWODUP,MOD,MINUS,PLUS
QCH7:
        dw OVER,lit,127,EQUAL,qbranch,QCH8
        dw ONEMINUS
QCH8:
        dw OVER,lit,18,EQUAL,qbranch,QCH9
        dw BEE,TOR,TOR,VEE,RFROM,RFROM
QCH9:
        dw OVER,lit,3,EQUAL,qbranch,QCH10
        dw N,TOR,TOR,VEE,RFROM,RFROM
QCH10:
        dw OVER,lit,22,EQUAL,qbranch,QCH11
        dw INSERT_CHAR
QCH11:
        dw OVER,lit,7,EQUAL,qbranch,QCH12
        dw DELETE_CHAR
QCH12:
        dw OVER,lit,25,EQUAL,qbranch,QCH13
        dw DELETE_LINE,TOR,TOR,VEE,RFROM,RFROM
QCH13:
        dw OVER,lit,15,EQUAL,qbranch,QCH14
        dw INSERT_LINE,TOR,TOR,VEE,RFROM,RFROM
QCH14:
        dw EXIT


;: EDIT ( n -- ) CLS 0 DUP AT-XY LIST 0
;    BEGIN !XY KEY SWAP ?CH SWAP 27 = UNTIL DROP L ;
    head_editor(EDIT,EDIT,docolon)
        DW S,VEE
        dw lit,0
EDIT1:
        dw STOREXY,KEY,SWOP,QCH,SWOP,lit,27,EQUAL,qbranch,EDIT1
        dw DROP,VEE
        dw EXIT


;  RC2014 Memdump =========================

;Z (MEMDUMP)  ( addr u --    memory dump line )
;     DUP >R
;     OVER 4 .W ." : "
;     0 DO DUP I + C@ 2 .W LOOP
;     DROP R>
;     SPACE     ( addr u )
;     TYPE$  ;
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
        dw TYPESTRING,EXIT


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


