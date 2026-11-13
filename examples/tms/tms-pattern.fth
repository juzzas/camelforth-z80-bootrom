\ TMS Pattern definition helpers

1 3 +THRU













ALSO TMS DEFINITIONS


\ 768byte fonts

1707  CONSTANT  MsxFontBlock
1710  CONSTANT  ZxFontBlock
1713  CONSTANT  SamFontBlock
1716  CONSTANT  VgaFontBlock

: PATTERN[   ( -- )
   BEGIN
      BL WORD COUNT DUP 0= IF
         2DROP REFILL 0= IF  EXIT  THEN
      ELSE
         OVER C@ [CHAR] ] = IF
            2DROP EXIT
         ELSE
            0 0 2SWAP >NUMBER
            2DROP DROP C,
         THEN
      THEN
   AGAIN
;


: PATTERN>VRAM   ( c -- vram-addr )
   3 LSHIFT   TmsPatternAddr +
;


PREVIOUS DEFINITIONS
