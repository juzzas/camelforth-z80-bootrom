\ CamelForth BootROM RC2014 32k Paged RAM drive

\ Requires pagedram wordset

ONLY FORTH ALSO UTILS DEFINITIONS

1 2 +THRU


: /RAMDRIVE CR  ." RAMDRIVE OK (" 
        RAM-SLICE-ID SLICE>LIMIT @ .
        ." blocks)" ;
/RAMDRIVE

PREVIOUS DEFINITIONS

\ CamelForth BootROM RC2014 32k Paged RAM drive
\   Fetch Compact Flash capacity (sectors)
: RAM-CAPACITY  ( d -- )
    64. ;

\ Paged RAM read sector at LBA
: RAM-SECTOR-READ  ( lba-l lba-h adrs -- ior )
   >R  DROP 512 *   R>  512  PRMOVE 0 ;

\ Paged RAM  write sector at LBA
: RAM-SECTOR-WRITE  ( lba-l lba-h adrs -- ior )
   >R  DROP 512 *   R>  SWAP  512  PRMOVE 0 ;

' RAM-SECTOR-READ  ' RAM-SECTOR-WRITE  ' RAM-CAPACITY
    DRIVE: RAM-DRIVE-ID
RAM-DRIVE-ID 0 SLICE: RAM-SLICE-ID

