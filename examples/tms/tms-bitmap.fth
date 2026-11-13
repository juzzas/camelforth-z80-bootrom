\ TMS BITMAP word set
CR .( Loading TMS BITMAP wordset )
ALSO TMS  DEFINITIONS

1 5 +THRU

PREVIOUS DEFINITIONS









HEX

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
   OVER SWAP  tmsxy-offset      ( x offset )
   TmsPatternAddr + >R           ( x  r: addr )
   7 AND  masklut + C@  INVERT   ( bitmask  r: addr )
   R@ TMSRDADDR  TMS@
   AND
   R>  TMSWRADDR  TMS!  ;


VARIABLE x0   VARIABLE y0
VARIABLE x1   VARIABLE y1
VARIABLE dx   VARIABLE sx
VARIABLE dy   VARIABLE sy
VARIABLE e2

: TMSPLOT-LINE  ( x1 y1 x2 y2 -- )
   y1 !   x1 !   y0 !   x0 !
   x1 @  x0 @  -  ABS         dx !
   x1 @  x0 @  < 2* 1+        sx !
   y0 @  y1 @  - ABS NEGATE   dy !
   y1 @  y0 @  < 2* 1+        sy !
   dx @  dy @  +
   BEGIN
      x0 @  y0 @  TMSPLOT-XY
      x0 @  x1 @  =   y0 @  y1 @  =   AND 0=
   WHILE
      DUP 2*  DUP >R
         dy @ < 0=  IF  dy @ +   sx @ x0 +!  THEN
      R> dx @ > 0=  IF  dx @ +   sy @ y0 +!  THEN
   REPEAT  DROP
;



DECIMAL
