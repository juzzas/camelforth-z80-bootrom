: /CALLXT   ( xt -- )
  HEXLOAD
  :09F00000EF6069F701FFFFE7C9A9
  :00000001FF ;

: TEST
    ." Hello world!" CR ;

:  RUN
    /CALLXT
    ['] TEST    [ HEX ] F000 CALL ." yay!"  CR .S  ;



