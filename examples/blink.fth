CREATE PATTERN  24 C, 36 C, 66 C, 129 C,

: DELAY 10000 0 DO LOOP ;

: DOLOOP 4 0 DO PATTERN I + C@ 0 PC! DELAY LOOP ;

: RUN 10 0 DO DOLOOP LOOP ;
