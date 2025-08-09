\ TMS9918 

ONLY FORTH
VOCABULARY TMS

ALSO TMS   DEFINITIONS

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
3BC0 CONSTANT TmsSpriteAttrAddr

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

: TMSWRADDR   ( addr -- ) 
    DUP 00FF AND TMSREG PC!
    3F00 AND 8 RSHIFT 40 + TMSREG PC! ;
: TMSRDADDR   ( addr -- )
    DUP 00FF AND TMSREG PC!
    3F00 AND 8 RSHIFT TMSREG PC! ;

: TMS!  ( val -- )  TMSRAM PC! ;
: TMS@  ( val -- )  TMSRAM PC@ ;

: TMSREG! ( val reg -- )
    2DUP 7F AND TmsRegs + C!
    SWAP
    TMSREG PC!  TMSREG PC!  ;
: TMSREG@ ( reg -- val )
    7F AND TmsRegs + C@  ;

: TMSWRITE ( src dest count -- )
  SWAP TMSWRADDR
  0 DO DUP C@ TMS! 1+ LOOP  ;

: TMSFILL ( dest count c -- )
   ROT TMSWRADDR
   SWAP  0 DO DUP TMS! LOOP  DROP  ;

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



\ Text mode ======================

: /TMSTEXT  ( -- )
   TMSRESET
   TmsNameAddr 400 / TmsNameReg  TMSREG!
   TmsColorAddr 40 / TmsColorReg TMSREG!
   TmsPatternAddr 800 / TmsPatternReg TMSREG!
   TmsSpritePatternAddr 800 / TmsSpritePatternReg TMSREG!
   TmsSpriteAttrAddr  80 / TmsSpriteAttrReg TMSREG!
   00  TmsCtrl0Reg TMSREG!
   D0  TmsCtrl1Reg TMSREG!
;

: TMSAT-XY ( x y -- )
    $28 *  +    TmsNameAddr +  TMSWRADDR ;

: TMSEMIT   ( c -- )
    TMS!            ;

: TMSTYPE   ( c-addr u -- )
    0 DO
        DUP C@ TMSEMIT 1+
    LOOP  DROP  ;

\ Tile mode =========================

: /TMSTILE ( -- )
   TMSRESET
   TmsNameAddr 400 / TmsNameReg  TMSREG!
   TmsColorAddr 40 / TmsColorReg TMSREG!
   TmsPatternAddr 800 / TmsPatternReg TMSREG!
   TmsSpritePatternAddr 800 / TmsSpritePatternReg TMSREG!
   TmsSpriteAttrAddr  80 / TmsSpriteAttrReg TMSREG!
   00  TmsCtrl0Reg TMSREG!
   C0  TmsCtrl1Reg TMSREG!
;

: TMSTILE-XY  ( x y -- offset )
    5 LSHIFT   +    TmsNameAddr  +    ;

\ Bitmap mode ======================

: /TMSBITMAP ( -- )
   TMSRESET
   FF TmsColorTableReg TMSREG!   ( 2000H )
   03 TmsPatternReg TMSREG!      ( 0000H )
   TmsNameAddr 400 / TmsNameReg  TMSREG!
   TmsSpritePatternAddr 800 / TmsSpritePatternReg TMSREG!
   TmsSpriteAttrAddr  80 / TmsSpriteAttrReg TMSREG!
   TmsNameAddr TMSWRADDR
   3 0 DO
      100 0 DO
         I TMS!
      LOOP
   LOOP
   02  TmsCtrl0Reg TMSREG!
   C0  TmsCtrl1Reg TMSREG!
;


\ byte address = 8(INT(X/8)) + 256(INT(Y/8)) + R(Y/8)
\    where R(Y/8) is equal to the remainder of (Y/8)
: tmsxy-offset     ( x y -- offset )
   8 /MOD   8 LSHIFT   >R       ( x y%8  r: y/8*256 )
   SWAP   F8  AND   R> + +  ;   (  y%8 + x&F8 + y/8*256 )

CREATE masklut 80 C, 40 C, 20 C, 10 C, 8 C, 4 C, 2 C, 1 C,

: TMSPLOT-XY ( x y -- )
   OVER SWAP  tmsxy-offset      ( x offset )
   TmsPatternAddr + >R           ( x  r: addr )
   7 AND  masklut + C@          ( bitmask  r: addr )
   R@ TMSRDADDR  TMS@
   OR
   R>  TMSWRADDR  TMS!  ;

: TMSCLR-XY ( x y -- )

                ;
CR .( loaded TMS )
DECIMAL
TMSREG PC@ .
0 0 tmsxy-offset .
8 0 tmsxy-offset .
0 191 tmsxy-offset .
255 191 tmsxy-offset .

CR .( started bitmap mode )
/TMSBITMAP
TmsPatternAddr TmsBitmapPatternLen 0 TMSFILL
TmsColorAddr TmsBitmapColorLen $1F TMSFILL

HEX 5 87 TMSREG!
