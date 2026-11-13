\ TMS9918 BASE

VOCABULARY TMS ( --- )
ALSO TMS DEFINITIONS
CR .( Loading TMS9918... )
1 8 +THRU

PREVIOUS DEFINITIONS

1635 CONSTANT TMS-TEXTING
1650 CONSTANT TMS-TILING
1665 CONSTANT TMS-BITMAPPING
1680 CONSTANT TMS-PATTERNING
1695 CONSTANT TMS-SPRITING


\ TMS9918 BASE
HEX

\ IO port
98 CONSTANT TMSRAM
TMSRAM 1+ CONSTANT TMSREG

\ name table address (multiples of 400H)
3800 CONSTANT TmsNameAddr

\ color table address (multiples of 40H)
2000 CONSTANT TmsColorAddr

\ pattern table (multiples of 800H)
0 CONSTANT TmsPatternAddr

\ sprite attribute table (multiples of 800H)
1800 CONSTANT TmsSpritePatternAddr

\ sprite pattern table (multiples of 80H)
3B00 CONSTANT TmsSpriteAttrAddr

80 CONSTANT TmsCtrl0Reg           81 CONSTANT TmsCtrl1Reg
82 CONSTANT TmsNameReg            83 CONSTANT TmsColorTableReg
84 CONSTANT TmsPatternReg         85 CONSTANT TmsSpriteAttrReg
86 CONSTANT TmsSpritePatternReg   87 CONSTANT TmsColorReg

\ Control Register 0 Bits
2   CONSTANT TmsM3                1   CONSTANT TmsExtVideo

\ Control Register 1 Bits
80 CONSTANT Tms16k                40 CONSTANT TmsDisplayEnable
20 CONSTANT TmsIntEnableBit       10 CONSTANT TmsM1
8  CONSTANT TmsM2                 2  CONSTANT TmsSprite32
1  CONSTANT TmsSpriteMag

\ table lengths
300  CONSTANT TmsTileNameLen
3C0  CONSTANT TmsTextNameLen

20   CONSTANT TmsTileColorLen
1800  CONSTANT TmsBitmapColorLen

800  CONSTANT TmsTilePatternLen
800  CONSTANT TmsTextPatternLen
600  CONSTANT TmsMulticolorPatternLen
1800 CONSTANT TmsBitmapPatternLen

\ color constants
00 CONSTANT TmsTransparent    01 CONSTANT TmsBlack
02 CONSTANT TmsMediumGreen    03 CONSTANT TmsLightGreen
04 CONSTANT TmsDarkBlue       05 CONSTANT TmsLightBlue
06 CONSTANT TmsDarkRed        07 CONSTANT TmsCyan
08 CONSTANT TmsMediumRed      09 CONSTANT TmsLightRed
0A CONSTANT TmsDarkYellow     0B CONSTANT TmsLightYellow
0C CONSTANT TmsDarkGreen      0D CONSTANT TmsMagenta
0E CONSTANT TmsGray           0F CONSTANT TmsWhite

CREATE TmsRegs  8 ALLOT

: TMSWRADDR   ( vram-addr -- )
    DUP 00FF AND TMSREG PC!
    3F00 AND 8 RSHIFT 40 + TMSREG PC! ;
: TMSRDADDR   ( vram-addr -- )
    DUP 00FF AND TMSREG PC!
    3F00 AND 8 RSHIFT TMSREG PC! ;

: TMS!  ( c -- )  TMSRAM PC! ;
: TMS@  ( -- c )  TMSRAM PC@ ;

: TMSREG! ( val reg -- )
   2DUP 7F AND TmsRegs + C!
   SWAP
   TMSREG PC!  TMSREG PC!  ;

: TMSREG@ ( reg -- val )
   7F AND TmsRegs + C@  ;

: TMSPUTS ( c-addr u -- )
   0 ?DO DUP C@ TMS! 1+ LOOP  ;

: TMSWRITE ( c-addr vram-addr u -- )
   SWAP TMSWRADDR  TMSPUTS ;

: TMSGETS ( c-addr u -- )
   0 ?DO TMS@ OVER  C! 1+ LOOP  ;

: TMSREAD ( vram-addr c-addr u -- )
   ROT TMSRDADDR  TMSGETS ;

: TMSFILL ( vram-addr u c -- )
   ROT TMSWRADDR
   SWAP  0 ?DO DUP TMS! LOOP  DROP  ;

: TMSRESET
   0 TmsCtrl0Reg TMSREG!
   80 ( Tms16k )    TmsCtrl1Reg TMSREG!
   0 TmsColorReg TMSREG!   ;

: TMSDISPLAY+ ( -- )
   TmsCtrl1Reg TMSREG@
   TmsDisplayEnable OR
   TmsCtrl1Reg TMSREG!   ;

: TMSDISPLAY- ( -- )
   TmsCtrl1Reg TMSREG@
   TmsDisplayEnable INVERT AND
   TmsCtrl1Reg TMSREG!   ;

: TMSBG     ( c -- )
   0F AND
   TmsColorReg TMSREG@  F0 AND   OR
   TmsColorReg TMSREG!  ;

: TMSFG     ( c -- )
   0F AND   4 LSHIFT
   TmsColorReg TMSREG@  0F AND   OR
   TmsColorReg TMSREG!  ;

: TMSPATTERN-OFFSET ( n -- vram-addr )
   3 LSHIFT  TmsPatternAddr + ;

DECIMAL

