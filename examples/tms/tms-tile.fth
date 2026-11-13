\ TMS TILE word set
CR .( Loading TMS TILE wordset )
ALSO TMS  DEFINITIONS

1 4 +THRU

PREVIOUS DEFINITIONS









HEX

\ Tile mode =========================

: /TMSTILE ( -- )
   TMSRESET
   TmsNameAddr 400 / TmsNameReg  TMSREG!
   TmsColorAddr 40 / TmsColorTableReg TMSREG!
   TmsPatternAddr 800 / TmsPatternReg TMSREG!
   TmsSpritePatternAddr 800 / TmsSpritePatternReg TMSREG!
   TmsSpriteAttrAddr  80 / TmsSpriteAttrReg TMSREG!
   00  TmsCtrl0Reg TMSREG!
   C0  TmsCtrl1Reg TMSREG!
   F5  TmsColorReg TMSREG!
;

: TMSTILE-XY  ( x y --  )
    5 LSHIFT   +    TmsNameAddr  +  TMSWRADDR  ;

: TMSEMIT   ( c -- )
    TMS!            ;

: TMSTYPE   ( c-addr u -- )
    0 ?DO
        DUP C@ TMSEMIT 1+
    LOOP  DROP  ;

20 CONSTANT TmsTileLine%   \ line length
18 CONSTANT TmsTileLine#   \ number of lines
TmsTileNameLen TmsTileLine% - CONSTANT TmsTextScrollRegion%

: TMS-CLS   ( -- )
   TmsNameAddr TmsTileNameLen BL  TMSFILL
   TmsColorAddr TmsTileColorLen TmsColorReg TMSREG@ TMSFILL
   0 0 TMSTILE-XY  ;

DECIMAL
