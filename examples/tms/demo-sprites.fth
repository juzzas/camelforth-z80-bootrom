CR .( Loading TMS 9918 Sprite demo )

1620 LOAD     TMS-TILING LOAD
TMS-PATTERNING LOAD    TMS-SPRITING LOAD

1 12 +THRU

demo-sprites

CR .( done )






ALSO TMS

CR .( starting sprite mode )
/TMSTILE  TMSSPRITE-32+

CR .( loading font data )
1710 BLOCK TmsPatternAddr 256 +  768 TMSWRITE

CR .( starting sprite demo )







\ SIN lookuptable for 0-90 degrees, to 4 implied decimal places
CREATE tab-sin 
     0 ,    175 ,    349 ,    523 ,    698 ,    872 ,   1045 , 
  1219 ,   1392 ,   1564 ,   1736 ,   1908 ,   2079 ,   2250 , 
  2419 ,   2588 ,   2756 ,   2924 ,   3090 ,   3256 ,   3420 , 
  3584 ,   3746 ,   3907 ,   4067 ,   4226 ,   4384 ,   4540 , 
  4695 ,   4848 ,   5000 ,   5150 ,   5299 ,   5446 ,   5592 , 
  5736 ,   5878 ,   6018 ,   6157 ,   6293 ,   6428 ,   6561 , 
  6691 ,   6820 ,   6947 ,   7071 ,   7193 ,   7314 ,   7431 , 
  7547 ,   7660 ,   7771 ,   7880 ,   7986 ,   8090 ,   8192 , 
  8290 ,   8387 ,   8480 ,   8572 ,   8660 ,   8746 ,   8829 , 
  8910 ,   8988 ,   9063 ,   9135 ,   9205 ,   9272 ,   9336 , 
  9397 ,   9455 ,   9511 ,   9563 ,   9613 ,   9659 ,   9703 , 
  9744 ,   9781 ,   9816 ,   9848 ,   9877 ,   9903 ,   9925 , 
  9945 ,   9962 ,   9976 ,   9986 ,   9994 ,   9998 ,  10000 , 

\ sine of 0 - 180 
: (sin180) ( deg -- n ) 
   DUP 90 >  ( 91 - 180 degrees )
   IF 180 SWAP - THEN  ( reflect )
   CELLS tab-sin + @ ;

: SIN ( deg -- n ) \ sine to 4 implied decimal places
   360 MOD
   DUP 0< IF 360 + THEN
   DUP 180 >  IF    180 -  (sin180)   NEGATE 
              ELSE   (sin180)   THEN ;

: COS ( deg -- n ) \ cosine to 4 implied decimal places
   360 MOD 90 + SIN ;


\ Point at distance len from origin x,y at angle deg.
\ Note: 0 degs points right, 90 deg points down.
: POLAR>XY  ( x y deg len -- x' y' )
   2DUP SWAP SIN 10000 */ >R
   SWAP COS 10000 */
   ROT +  SWAP R> + ;










HEX   \ Rotating Planet Stage 1, 2, 3
CREATE Planet1 PATTERN[ 00 03 07 0F 07 A3 F1 F0
                        F0 E0 80 1C 0C 02 00 00
                        00 00 80 98 0C 41 23 03
                        07 5F 3F 1E 3E 1C 08 00 ]

CREATE Planet2 PATTERN[ 00 00 01 03 01 68 FC FC
                        FC F8 E0 07 03 00 00 00
                        00 C0 E0 E6 C2 D0 48 00
                        01 17 0F 06 0E 84 00 00 ]

CREATE Planet3 PATTERN[ 00 00 00 00 40 1A 3F 3F
                        7E FE F8 60 61 00 00 00
                        00 30 78 F8 70 34 12 00
                        00 05 03 00 C2 C0 20 00 ]

\ Rotating Planet Stage 4, 5, 6
CREATE Planet4 PATTERN[ 00 00 00 60 30 06 8F 0F
                        1F 7F FE 78 78 30 00 00
                        00 08 1C 3E 1C 8D C4 C0
                        C1 81 00 00 70 30 08 00 ]

CREATE Planet5 PATTERN[ 00 00 00 18 0C 41 23 03
                        07 5F 3F 1E 3E 1C 08 00
                        00 00 04 0E 06 A3 F1 F0
                        F0 E0 80 00 1C 0C 00 00 ]

CREATE Planet6 PATTERN[ 00 00 20 66 43 D0 48 00
                        01 17 0F 07 0F 07 02 00
                        00 00 00 02 00 68 FC FC
                        FC F8 E0 80 86 00 00 00 ]

\ Rotating Planet Stage 7, 8 and overlay
CREATE Planet7 PATTERN[ 00 10 38 79 70 34 12 00
                        00 05 03 01 03 01 00 00
                        00 00 00 80 C0 1A 3F 3F
                        7F FE F8 E0 E0 C0 80 00 ]

CREATE Planet8 PATTERN[ 00 0C 1E 3E 1C 8D C4 C0
                        30 81 00 00 70 30 08 00
                        00 00 00 60 30 06 8F 0F
                        1F 7F FE 78 F8 70 20 00 ]

CREATE PlanetOverlay PATTERN[ 07 1F 3F 7F 7F FF FF FF
                              FF FF FF 7F 7F 3F 1F 07
                              E0 F8 FC FE FE FF FF FF
                              FF FF FF FE FE FC F8 E0 ]

\ Sprite definitions
DECIMAL
PlanetOverlay 8 TMSSPRITE-PATTERN-OFFSET  32  TMSWRITE
Planet1  12 TMSSPRITE-PATTERN-OFFSET  32  TMSWRITE
Planet2  16 TMSSPRITE-PATTERN-OFFSET  32  TMSWRITE
Planet3  20 TMSSPRITE-PATTERN-OFFSET  32  TMSWRITE
Planet4  24 TMSSPRITE-PATTERN-OFFSET  32  TMSWRITE
Planet5  28 TMSSPRITE-PATTERN-OFFSET  32  TMSWRITE
Planet6  32 TMSSPRITE-PATTERN-OFFSET  32  TMSWRITE
Planet7  36 TMSSPRITE-PATTERN-OFFSET  32  TMSWRITE
Planet8  40 TMSSPRITE-PATTERN-OFFSET  32  TMSWRITE


0 CONSTANT SPRITE_PLANET
1 CONSTANT SPRITE_PLANET_OVRLY
2 CONSTANT SPRITE_EOF

12 VALUE planet-frame

: planet-cycle   ( -- )
   planet-frame 4 +  TO planet-frame

   planet-frame 40 > IF
      12 TO planet-frame
   THEN

   planet-frame  SPRITE_PLANET TMSSPRITE-PATTERN!
;

: planet-move   ( x y -- )
   2DUP
   SPRITE_PLANET_OVRLY  TMSSPRITE-XY
   SPRITE_PLANET        TMSSPRITE-XY
;

: planet-plot   ( deg -- x y )
   >R 120 76 R> 80 POLAR>XY ;

: demo-sprites
   0 0 TMSTILE-XY
   TmsNameAddr TmsTileNameLen  BL  TMSFILL
   TmsColorAddr TmsTileColorLen $F5 TMSFILL

   13 10 TMSTILE-XY
   S" RC2014" TMSTYPE

   8  SPRITE_PLANET_OVRLY TMSSPRITE-PATTERN!
   TmsDarkBlue SPRITE_PLANET_OVRLY TMSSPRITE-COLOR!

   12 TO planet-frame
   planet-frame  SPRITE_PLANET TMSSPRITE-PATTERN!
   TmsDarkGreen SPRITE_PLANET TMSSPRITE-COLOR!

   SPRITE_EOF  TMSSPRITE-END!

   10 10 planet-move

   1440 0 DO
      10 MS  I 5 MOD 0= IF  planet-cycle THEN
      I  planet-plot  planet-move
   LOOP
;

PREVIOUS







