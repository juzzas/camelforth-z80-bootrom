CREATE PATTERN  24 C, 36 C, 66 C, 129 C,    ( 4 byte LED pattern to display )

: DELAY   ( --   delay 10000 cycles )
    10000 0 DO LOOP ;

: LEDOUT  ( n --   display n on LEDs )
    0 PC! ;    ( output to port 0 )

: LED-PATTERN  ( addr n --   display pattern on LEDs )
    0 DO                ( loop 0 to n-1. stack = addr )
        DUP             ( stack = addr addr )
        C@              ( fetch byte.  stack = addr n )
        LEDOUT          ( display byte on LEDs. on stack: addr )
        DELAY           ( short delay )
        1+              ( increment addr. stack = addr+1 )
    LOOP
    DROP ;              ( drop remaining addr from stack )

: RUN    ( --   repeat 10 times )
    10 0 DO
        PATTERN 4 LED-PATTERN  ( display pattern at address PATTERN for 4 bytes )
    LOOP ;
