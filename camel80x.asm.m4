; RC2014 Extension Words - 16K
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


SECTION code_user_16k

; RC2014 EXTENSION (MISC) =======================

;C ERASE       ( a-addr u --   fill with 0's )
;    0 FILL
;    ;
    head(ERASE,ERASE,docolon)
        dw lit,0,FILL
        dw EXIT

;C BLANKS       ( a-addr u --   fill with spaces )
;    BL FILL
;    ;
    head(BLANKS,BLANKS,docolon)
        dw lit,32,FILL
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

;Z .W           ( n width --   width with leading 0's )
;    (.W) TYPE SPACE ;
    head(DOTW,.W,docolon)
        dw XDOTW,TYPE,SPACE
        dw EXIT

;Z (U.W)        ( u width --   width with leading 0's )
;    0 SWAP (D.W) ;
    head(XUDOTW,(U.W),docolon)
        dw lit,0,SWOP,XDDOTW
        dw EXIT

;Z U.W         ( u width --   width with leading 0's )
;    (U.W) TYPE SPACE ;
    head(UDOTW,U.W,docolon)
        dw XUDOTW,TYPE,SPACE
        dw EXIT

;Z PRINTABLE?         ( n - flag  is characte printable? )
;    20 7F WITHIN ;
    head(PRINTABLEQ,PRINTABLE?,docolon)
        dw lit,0x20,lit,0x7f,WITHIN
        dw EXIT

;Z ESC  ( --  emit escape character )
;    27 EMIT ;
    head(ESC,ESC,docolon)
        dw lit,0x1b,EMIT
        dw EXIT

;Z CLS  ( --  clear screen )
;    ESC ." [2J"
    head(CLS,CLS,docolon)
        dw ESC, XSQUOTE
        db 3,"[2J"
        dw TYPE
        dw EXIT

;Z RESET  ( -- reset attributes )
;    ESC ." [0m" ;
    head(RESET,RESET,docolon)
        dw ESC, XSQUOTE
        db 3,"[0m"
        dw TYPE
        dw EXIT

;Z AT-XY  ( x y -- move cursor to x,y )
;    ESC ." [" 1+ (.) TYPE ." ;" 1+ (.) TYPE ." H" ;
    head(AT_XY,AT-XY,docolon)
        dw ESC, XSQUOTE
        dw lit,'[',EMIT
        dw ONEPLUS,XDOT,TYPE
        dw lit,';',EMIT
        dw ONEPLUS,XDOT,TYPE
        dw lit,'H',EMIT
        dw EXIT

dnl ;Z :NONAME       ( -- xt      define anonymous xt )
dnl ;    LATEST @ , 0 C,    ( last link + immed flag )
dnl ;    HERE LATEST !      ( new "latest" )
dnl ;    0 C,          ( empty NFA )
dnl ;    HERE               ( push xt to stack             )
dnl ;    HIDE ] !COLON  ;   ( start compiling as a docolon )
    head(NONAME,:NONAME,docolon)
        dw LATEST,FETCH,COMMA,lit,0,CCOMMA
        dw HERE,LATEST,STORE
        dw lit,0,CCOMMA
        dw HERE
        dw HIDE,RIGHTBRACKET,lit,docolon,COMMACF
        dw EXIT


