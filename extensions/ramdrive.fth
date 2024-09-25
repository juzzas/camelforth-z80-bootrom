\ RAM drive extension for CamelForth 

\ Conveniently, the RC2014 Mini II has a spare 32kilobytes in
\ low memory, that CamelForth BootROM doesn't use. Let's use it
\ as a volatile 32-block storage device!




ASSEMBLING LOAD
1 4 +THRU







   \ RAM drive extension for CamelForth - page in / out
HEX  ALSO ASSEMBLER DEFINITIONS

: PAGERAM
   1 #  38 #  OUT,
;  IMMEDIATE








   \ RAM drive extension for CamelForth - sector read
CODE: RAM-SECTOR-READ  ( lba-l lba-h adrs -- )  \ Compact Flash read sector at LBA 
   PAGERAM
   # DE LD,
   DROP  7 LSHIFT  # HL LD,
   200 # BC LD,
   LDIR
   PAGERAM
;CODE







   \ RAM drive extension for CamelForth - sector write
CODE: RAM-SECTOR-WRITE  ( lba-l lba-h adrs -- )  \ Compact Flash write sector at LBA
   PAGERAM
   # HL LD,
   DROP  7 LSHIFT  # DE LD,
   200 # BC LD,
   LDIR
   PAGERAM
;CODE







   \ RAM drive extension for CamelForth - setup
64 2CONSTANT RAM-CAPACITY    ( 32 blocks = 64 sectors )

CREATE RAMDRIVE  DRIVE% ALLOT
' RAM-SECTOR-READ   RAMDRIVE DRIVE>READ !
' RAM-SECTOR-WRITE  RAMDRIVE DRIVE>WRITE !
' RAM-CAPCITY       RAMDRIVE DRIVE>CAPACITY !


: /RAMDRIVE   ( -- drive-id )  ( initialise the RAM Drive drive )
    RAMDRIVE   ;











