CR .( Loading TMS 9918 Tile demo )

1620 LOAD     TMS-TILING LOAD

1 3 +THRU

demo-tile

CR .( done )







ALSO TMS

CR .( starting tile mode )
/TMSTILE

CR .( loading font data )
1716 BLOCK TmsPatternAddr 256 +  768 TMSWRITE

CR .( starting tile demo )







: demo-tile
   TMS-CLS

   10 1 TMSTILE-XY
   S" Hello, RC2014!" TMSTYPE

   0 5 TMSTILE-XY
   256 0 DO I TMSEMIT LOOP
;

PREVIOUS






















