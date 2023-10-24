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
        dw CR,VB,STATUS
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

;  RC2014 HEXLOAD ===============================

; VARIABLE IHXCRC
; HEX

; 0 CONSTANT IHXOK
; 1 CONSTANT IHXEND
; 2 CONSTANT IHXERROR

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
;    >R 0 S>D R> 2
;    >NUMBER       ( du tib-ptr u )
;    DROP NIP      ( u tib-ptr )
;    ;
    head(XIHXBYTE,(IHXBYTE),docolon)
        DW TOR,lit,0,STOD,RFROM,lit,2
        DW TONUMBER
        DW DROP,NIP
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

;: IHXREC ( c-addr -- 0 if ok, 1 if end, 2 if error  parse hex record )
;    0 IHXCRC !
;    DUP C@ [CHAR] : <> IF  IHXERROR  ( no colon ) EXIT  THEN
;
;    CHAR+
;    IHXBYTE   ( count tib-ptr )
;    IHXWORD   ( count hex-addr tib-ptr )
;    IHXBYTE   ( count hex-addr record-type tib-ptr )
;    SWAP 0=  IF               ( count hex-addr tib-ptr )
;        IHXREC!               ( hex-addr tib-ptr )
;        (IHXBYTE)             ( hex-addr crc tib-ptr )
;        DROP NIP              ( crc )
;        ?IHXCRC IF IHX-OK ELSE IHX-ERROR THEN
;    ELSE
;        DROP DROP DROP
;        IHX-END
;    THEN  ;
    head(IHXREC,IHXREC,docolon)
        DW lit,0,IHXCRC,STORE
        DW DUP,CFETCH,lit,58,NOTEQUAL,qbranch,IHXREC1
        DW lit,2,EXIT

IHXREC1:
        DW CHARPLUS
        DW IHXBYTE,IHXWORD,IHXBYTE

        DW SWOP,ZEROEQUAL,qbranch,IHXREC3
        DW IHXRECSTORE,XIHXBYTE,DROP,NIP

        DW QIHXCRC,qbranch,IHXREC2
        DW lit,0              ; IHXOK
        DW branch,IHXREC4

IHXREC2:
        DW lit,2              ; IHXERROR
        DW branch,IHXREC4

IHXREC3:
        DW DROP,DROP,DROP
        DW lit,1              ; IHXEND
IHXREC4:
        DW EXIT

; \ :07F00000EF7A535F1CE7C922
; \ :00000001FF

; \ :06F00000EF78414FE7C963

;: HEXLOAD
;    ." HEXLOAD test" CR
;    [CHAR] : EMIT
;
;    BEGIN
;
;        TIB DUP TIBSIZE ACCEPT
;        DROP                             ( drop count )
;        IHXREC
;
;        DUP IHX-OK = IF
;            [CHAR] # EMIT
;        THEN
;
;    DUP IHX-OK <> UNTIL  ;
;
;    DUP IHX-ERROR = IF
;        ABORT" HEXLOAD ERROR"
;    THEN
    head(HLOAD,HLOAD,docolon)
        DW XSQUOTE
        DB 12,"HEXLOAD test"
        DW TYPE,CR
        DW lit,58,EMIT

HEXLOAD1:
        DW TIB,DUP,TIBSIZE,ACCEPT
        DW DROP
        DW IHXREC

HEXLOAD2:
        DW DUP,lit,0,EQUAL,qbranch,HEXLOAD3
        DW lit,35,EMIT

HEXLOAD3:
        DW DUP,lit,0,NOTEQUAL,qbranch,HEXLOAD1

HEXLOAD4:
        DW DUP,lit,2,EQUAL,qbranch,HEXLOAD5
        DW XSQUOTE
        DB 13,"HEXLOAD ERROR"
        DW TYPE
        DW ABORT

HEXLOAD5:
        DW EXIT


