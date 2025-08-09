
: TMSINT+ ( -- )
   TmsCtrl1Reg TMSREG@
   TmsIntEnableBit OR
   TmsCtrl1Reg TMSREG!   ;

: TMSINT- ( -- )
   TmsCtrl1Reg TMSREG@
   TmsIntEnableBit INVERT AND
   TmsCtrl1Reg TMSREG!   ;


VARIABLE INTCOUNT  0 INTCOUNT !
VARIABLE SECCOUNT  0 SECCOUNT !

: IC+
  1 INTCOUNT +!
  INTCOUNT @ 32 > IF
    0 INTCOUNT !
    1 SECCOUNT +! 
  THEN ;

: INTHANDLER
  TMSREG PC@ AND 80 IF IC+ THEN  ;
  

( output count to screen )
: T
  1 1 TMSAT-XY
  SECCOUNT @ 
  (.) TMSTYPE ;

\  ' INTHANDLER 'INT !

