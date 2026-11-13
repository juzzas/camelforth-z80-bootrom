CR .( Loading TMS 9918 Bitmap demo )

1620 LOAD     TMS-BITMAPPING LOAD

ALSO TMS

1 2 +THRU









TMSREG PC@ .
0 0 tmsxy-offset .      8 0 tmsxy-offset .
0 191 tmsxy-offset .    255 191 tmsxy-offset .

CR .( started bitmap mode )
/TMSBITMAP
TmsPatternAddr TmsBitmapPatternLen 2/ 0 TMSFILL
TmsColorAddr TmsBitmapColorLen $1F TMSFILL

: line  ( -- )
   25 0 DO I I TMSPLOT-XY  LOOP  ;

: line2  ( -- )
   15 5 DO I I TMSCLR-XY  LOOP  ;

line
line2

0 0 255 191  TMSPLOT-LINE

5 TmsColorReg TMSREG!
CR .( started bitmap mode )
