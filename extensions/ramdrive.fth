\ CamelForth BootROM RC2014 32k Paged RAM drive

\ Requires pagedram wordset

ONLY FORTH ALSO SYSTEM DEFINITIONS

1 2 +THRU


: /RAMDRIVE CR  ." RAMDRIVE OK (" 
        RAM-SLICE-ID SLICE>LIMIT @ .
        ." blocks)" ;
/RAMDRIVE

PREVIOUS DEFINITIONS

\ CamelForth BootROM RC2014 32k Paged RAM drive
\   Fetch RAM drive capacity (1024byte blocks)
: RAM-CAPACITY  ( -- ud )
    32. ;

\ Paged RAM read 1024byte block number LBN
: RAM-BLOCK-READ  ( lbn-l lbn-h adrs -- ior )
   >R  DROP 1024 *   R>  1024  PRMOVE 0 ;

\ Paged RAM write 1024byte block number LBN
: RAM-BLOCK-WRITE  ( lbn-l lbn-h adrs -- ior )
   >R  DROP 1024 *   R>  SWAP  1024  PRMOVE 0 ;

\ Create RAM-DRIVE context
' RAM-BLOCK-READ  ' RAM-BLOCK-WRITE  ' RAM-CAPACITY
    DRIVE: RAM-DRIVE-ID

\ Create SLICE context for RAM drive
RAM-DRIVE-ID 0 SLICE: RAM-SLICE-ID

