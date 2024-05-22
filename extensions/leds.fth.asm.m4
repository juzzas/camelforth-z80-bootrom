; DEBUG LED implementation ========================

;Z /LED ( -- )                       initialise LED
;      0 led_state C!
;      0 0 PC!   ;
    head(SLASHLED,/LED,docolon)
        DW lit,0,lit,led_state,CSTORE
        DW lit,0,lit,0,PCSTORE
        DW EXIT

;Z @LED ( -- c )                    fetch LED value
;      led_state C@ ;
    head(LEDFETCH,@LED,docolon)
        DW led_state,CFETCH
        DW EXIT

;Z !LED ( -- c )                    store LED value
;      DUP led_state C! 0 PC! ;
    head(LEDSTORE,!LED,docolon)
        DW DUP,lit,led_state,CSTORE
        DW lit,0,PCSTORE
        DW EXIT

;Z +LED ( n -- )                       enable LED n
;       DUP 0 8 WITHIN IF
;           1 SWAP LSHIFT LED@ OR LED!
;       ELSE
;           DROP
;       THEN ;
    head(PLUSLED,+LED,docolon)
        DW DUP,lit,0,lit,8,WITHIN,qbranch,PLUSLED1
        DW lit,1,SWOP,LSHIFT,LEDFETCH,OR,LEDSTORE
        DW EXIT
PLUSLED1:
        DW DROP,EXIT

;Z -LED ( n -- )                      disable LED n
;       DUP 0 8 WITHIN IF
;           1 SWAP LSHIFT INVERT LED@ AND LED!
;       ELSE
;           DROP
;       THEN ;
    head(MINUSLED,-LED,docolon)
        DW DUP,lit,0,lit,8,WITHIN,qbranch,MINUSLED1
        DW lit,1,SWOP,LSHIFT,INVERT,LEDFETCH,AND,LEDSTORE
        DW EXIT
MINUSLED1:
        DW DROP,EXIT

SECTION data_user

led_state:
        DEFB 0

SECTION code_user_16k

