\ TMS TEXT word set
CR .( Loading TMS TEXT wordset )
ALSO TMS  DEFINITIONS

1 4 +THRU

PREVIOUS DEFINITIONS









HEX

: /TMSTEXT  ( -- )
   TMSRESET
   TmsNameAddr 400 / TmsNameReg  TMSREG!
   TmsColorAddr 40 / TmsColorTableReg TMSREG!
   TmsPatternAddr 800 / TmsPatternReg TMSREG!
   TmsSpritePatternAddr 800 / TmsSpritePatternReg TMSREG!
   TmsSpriteAttrAddr  80 / TmsSpriteAttrReg TMSREG!
   00  TmsCtrl0Reg TMSREG!
   D0  TmsCtrl1Reg TMSREG!
   F5  TmsColorReg TMSREG!
;

: TMSAT-XY ( x y -- )
    $28 *  +    TmsNameAddr +  TMSWRADDR ;

: TMSEMIT   ( c -- )
    TMS!            ;

: TMSTYPE   ( c-addr u -- )
    0 DO
        DUP C@ TMSEMIT 1+
    LOOP  DROP  ;

28 CONSTANT TmsTextLine%   \ line length
18 CONSTANT TmsTextLine#   \ number of lines
TmsTextNameLen TmsTextLine% - CONSTANT TmsTextScrollRegion%

CREATE textbuf  TmsTextNameLen ALLOT

: TMS-CLS   ( -- )
   TmsNameAddr TmsTextNameLen BL  TMSFILL
   0 0 TMSAT-XY  ;

: TMS-SCROLLUP ( -- )
   TmsNameAddr TmsTextLine% +   textbuf
         TmsTextScrollRegion%   TMSREAD

   textbuf  TmsNameAddr TmsTextScrollRegion%  TMSWRITE

   TmsNameAddr TmsTextScrollRegion% +   TmsTextLine%
         BL  TMSFILL
;

DECIMAL

