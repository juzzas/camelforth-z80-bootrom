( DEBUG LED implementation                               1 / 2 )

VARIABLE LED-STATE     VARIABLE LED-PORT

: /LED ( port --                    initialise LED )
   LED-PORT !
   0 LED-STATE C!
   0 LED-PORT @  PC!   ;

: LED@ ( -- c                     fetch LED value )
   LED_STATE C@ ;

: LED! ( -- c                     store LED value )
   DUP LED-STATE C!   LED-PORT @ PC! ;


( DEBUG LED implementation                               2 / 2 )
: +LED ( n --                        enable LED n )
   DUP 0 8 WITHIN IF
       1 SWAP LSHIFT LED@ OR LED!
   ELSE
       DROP
   THEN ;

: -LED ( n --                       disable LED n )
   DUP 0 8 WITHIN IF
       1 SWAP LSHIFT INVERT LED@ AND LED!
   ELSE
       DROP
   THEN ;

