HEX
3F CONSTANT CHPORT_CMD
3E CONSTANT CHPORT_DATA

1 CONSTANT CHCMD_GET_IC_VERSION
6 CONSTANT CHCMD_CHECK_EXISTS
8 CONSTANT CHCMD_SET_LBA
15 CONSTANT CHCMD_SET_MODE
22 CONSTANT CHCMD_STATUS
51 CONSTANT CHCMD_DISK_INIT
53 CONSTANT CHCMD_DISK_SIZE
28 CONSTANT CHCMD_RD
2B CONSTANT CHCMD_WR
54 CONSTANT CHCMD_DSKRD
55 CONSTANT CHCMD_DSKRDGO
56 CONSTANT CHCMD_DSKWR
57 CONSTANT CHCMD_DSKWRGO
58 CONSTANT CHCMD_DISK_INQ

51 CONSTANT CHCMD_RET_SUCCESS
5F CONSTANT CHCMD_RET_ABORT

: CHCMD! ." CMD: " DUP . CHPORT_CMD PC! CR ;
: CHCMD@ CHPORT_CMD PC@ ;
: CHDATA! CHPORT_DATA PC!  ;
: CHDATA@ CHPORT_DATA PC@  ;

: CHPOLL  ( -- status )
   BEGIN
      CHPORT_CMD PC@ 80 AND UNTIL
   CHCMD_STATUS CHCMD!
   CHDATA@ ;

: CHSTATUS
   CHCMD_STATUS CHCMD!
   CHDATA@ ;
   

: CHRESET
   CHCMD_SET_MODE CHCMD!
   7 CHDATA! 
   1 MS
   CHDATA@ ." result = " . 
   1 MS

   CHCMD_SET_MODE CHCMD!
   6 CHDATA! 
   1 MS
   CHDATA@ ." usb result = " . 
   1 MS

   CHCMD_DISK_INIT CHCMD!
   CHPOLL ." disk result = " .  ( 14 = success. TODO fix "no media" currently hangs )
;

CREATE CHBUFFER 65 CHARS ALLOT
: /CHBUFFER ( -- ) 0 CHBUFFER C! ;
: CHBUFFER+ ( c -- )
   CHBUFFER C@ 1+ CHBUFFER C! 
   CHBUFFER DUP C@ + C!  ;

: CHRD  ( -- buffer' count )
   /CHBUFFER
   CHCMD_RD CHCMD!
   CHDATA@   ( buffer c )
   0 DO
      CHDATA@ CHBUFFER+   [CHAR] . EMIT
   LOOP ; 

: CHWR ( -- )
   CHBUFFER C@
   0 DO
      CHBUFFER I 1+ + C@ CHDATA!
   LOOP  ;


( set up LBA, 2 sector read )
: LBA>CHBUFFER   ( LBA-L LBA-H -- ) 
   /CHBUFFER
   DUP FF AND 8 RSHIFT CHBUFFER+
   FF AND  CHBUFFER+
   DUP FF AND 8 RSHIFT CHBUFFER+
   FF AND  CHBUFFER+  ;

( read 64 byte chunk )
: (CHRDBLK)  ( buffer -- buffer' ) 
   CHPOLL ." rd result = " .
   ( CHRD . . )
    ;

( read multiple 64 byte chunks )
: CHRDBLK ( LBA-L LBA-H buffer -- )
   CHRESET
   >R 
   CHCMD_DSKRD CHCMD! 1 MS
   LBA>CHBUFFER CHWR
   1 CHDATA! 
   CHPOLL 1D <> ABORT" READ ERROR"
     CHRD

   CHCMD_DSKRDGO CHCMD! 1 MS
   CHPOLL 1D <> ABORT" READGO ERROR"
     CHRD
   
   R>
   7 0 DO  
      CHCMD_DSKRDGO CHCMD!
      CHPOLL ." rd result = "  DUP .
      1D = IF
        CHCMD_RD CHCMD!
        CHRD
      THEN

   LOOP  ;
     

: /CHUSB
   CHCMD_CHECK_EXISTS CHCMD!
   1 MS
   AA CHDATA!
   1 MS
   CHDATA@ ." result = " . 

   CHRESET

;

: CHDISKSIZE
   CHCMD_DISK_SIZE CHCMD!
   CHPOLL ." size result = " .  ( 14 = success. TODO fix "no media" currently hangs )

   ( read size byte -- should be 8 -- read 8 bytes )
   CHRD

 ;

: CHDISKINQ
   CHCMD_DISK_INQ CHCMD!
   CHPOLL ." size result = " .  ( 14 = success. TODO fix "no media" currently hangs )

   ( read size byte -- should be 36 -- read 36 bytes )
   CHRD

 ;


/CHUSB
1 0 A000 CHRDBLK


