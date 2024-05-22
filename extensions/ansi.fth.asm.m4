;Z BELL  ( -- beep )
;    BEL EMIT ;
    head(BELL,BELL,docolon)
        dw lit,7,EMIT
        dw EXIT


;Z RESET  ( -- reset attributes )
;    VT-ESC ." 0m" ;
    head(RESET,RESET,docolon)
        dw VT_ESC, XSQUOTE
        db 2,"0m"
        dw TYPE
        dw EXIT

;Z INVIS  ( -- make cursor invisible )
;    VT-ESC ." ?25l" ;
    head(INVIS,INVIS,docolon)
        dw VT_ESC,XSQUOTE
        db 4,"?25l"
        dw TYPE
        dw EXIT

;Z VIS  ( -- make cursor visible )
;    ESC ." [?25h" ;
    head(VIS,VIS,docolon)
        dw VT_ESC,XSQUOTE
        db 4,"?25h"
        dw TYPE
        dw EXIT

; 8 CONSTANT #COLOURS
    head(NCOLOURS,``#COLOURS'',docon)
        dw 8

;: INK  ( n -- change fg to n 0-7 )
;    DUP 0 #COLOURS WITHIN IF
;        VT-ESC
;        30 + (.) TYPE ." m"
;    THEN ;
    head(INK,INK,docolon)
        dw DUP,lit,0,NCOLOURS,WITHIN,qbranch,INK1
        dw VT_ESC
        dw lit,30,PLUS,XDOT,TYPE
        dw lit,'m',EMIT
INK1:
        dw EXIT

;: BRIGHT.INK  ( n -- change fg to n 0-7 )
;    DUP 0 #COLOURS WITHIN IF
;        VT-ESC
;        90 + (.) TYPE ." m"
;    THEN ;
    head(BRIGHTINK,BRIGHT.INK,docolon)
        dw DUP,lit,0,NCOLOURS,WITHIN,qbranch,BRINK1
        dw VT_ESC
        dw lit,90,PLUS,XDOT,TYPE
        dw lit,'m',EMIT
BRINK1:
        dw EXIT

;: PAPER  ( n -- change bg to n 0-7 )
;    DUP 0 #COLOURS WITHIN IF
;        VT-ESC
;        40 + (.) TYPE ." m"
;    THEN ;
    head(PAPER,PAPER,docolon)
        dw DUP,lit,0,NCOLOURS,WITHIN,qbranch,PAPER1
        dw VT_ESC
        dw lit,40,PLUS,XDOT,TYPE
        dw lit,'m',EMIT
PAPER1:
        dw EXIT

;: BRIGHT.PAPER  ( n -- change bg to n 0-7 )
;    DUP 0 #COLOURS WITHIN IF
;        VT-ESC
;        100 + (.) TYPE ." m"
;    THEN ;
    head(BRIGHTPAPER,BRIGHT.PAPER,docolon)
        dw DUP,lit,0,NCOLOURS,WITHIN,qbranch,BRPAPER1
        dw VT_ESC
        dw lit,100,PLUS,XDOT,TYPE
        dw lit,'m',EMIT
BRPAPER1:
        dw EXIT

;Z REVERSE  ( -- reverse attributes )
;    VT-ESC ." 7m" ;
    head(REVERSE,REVERSE,docolon)
        dw VT_ESC, XSQUOTE
        db 2,"7m"
        dw TYPE
        dw EXIT


