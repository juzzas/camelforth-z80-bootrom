CR .( Loading TMS 9918 Text demo )

1620 LOAD     TMS-TEXTING LOAD

1 2 +THRU

demo-text

CR .( done )







ALSO TMS

CR .( starting text mode )
/TMSTEXT

CR .( loading font data )
1701 BLOCK TmsPatternAddr 1024 TMSWRITE
1702 BLOCK TmsPatternAddr 1024 +  1024 TMSWRITE

CR .( starting text demo )






: demo-text
   TMS-CLS
   10 1 TMSAT-XY
   S" Hello, RC2014!" TMSTYPE

   0 5 TMSAT-XY
   256 0 DO I TMSEMIT LOOP

   4 0 DO
      1000 MS TMS-SCROLLUP
   LOOP
;

PREVIOUS






















