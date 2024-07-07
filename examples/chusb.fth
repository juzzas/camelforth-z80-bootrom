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

VARIABLE blkptr 0 blkptr !

: CHPOLL  ( -- status )
   BEGIN  ( 7999 0 DO )
      CHPORT_CMD PC@ 80 AND DUP IF
        1 MS
        CHCMD_STATUS CHPORT_CMD PC!
        1 MS
        CHPORT_DATA PC@
        ( UNLOOP ) EXIT
      THEN  UNTIL ( LOOP )
   ." timeout"  0 ;

: CHRESET
   BEGIN
   CHCMD_SET_MODE CHPORT_CMD PC!
   7 CHPORT_DATA PC!
   1 MS
   CHPORT_DATA PC@ DROP  ( ." result = " . )
   1 MS

   CHCMD_SET_MODE CHPORT_CMD PC!
   6 CHPORT_DATA PC!
   1 MS
   CHPORT_DATA PC@ DROP  ( ." usb result = " . )
   1 MS

   BEGIN
   ( 20 MS )
   CHCMD_DISK_INIT CHPORT_CMD PC!
   90 TDELAY  ( 20 MS )
   CHPOLL
   ( ." disk result = "   DUP .    [CHAR] ? EMIT )
   ( 14 = success. TODO fix "no media" currently hangs )
   14 = UNTIL
   1 MS
;

CREATE CHBUFFER 65 CHARS ALLOT
: /CHBUFFER ( -- ) 0 CHBUFFER C! ;
: CHBUFFER+ ( c -- )
   CHBUFFER C@ 1+ CHBUFFER C! 
   CHBUFFER DUP C@ + C!  ;

: CHRD  ( -- buffer' count )
   /CHBUFFER
   CHPORT_DATA PC@   ( buffer c )
   0 DO
      CHPORT_DATA PC@ CHBUFFER+
   LOOP   ( [CHAR] . EMIT ) ;

: CHUSBRD
   CHCMD_RD CHPORT_CMD PC!  90 TDELAY  CHRD  ;

: CHWR ( -- )
   CHBUFFER C@
   0 DO
      CHBUFFER I 1+ + C@   CHPORT_DATA PC!
   LOOP
   ( [CHAR] + EMIT )
  ;

: CHUSBWR  ( -- )
   CHCMD_WR CHPORT_CMD PC!  90 TDELAY
   blkptr @
   40 CHPORT_DATA PC!
   40 0 DO
      DUP  C@  CHPORT_DATA PC!  1+
   LOOP
   [CHAR] + EMIT  ;

( set up LBA, 2 sector read )
: LBA>CHBUFFER   ( LBA-L LBA-H -- ) 
   SWAP
   DUP FF AND         CHPORT_DATA PC!
            8 RSHIFT  CHPORT_DATA PC!
   DUP FF AND         CHPORT_DATA PC!
            8 RSHIFT  CHPORT_DATA PC!
   1                  CHPORT_DATA PC!  ( 1 sector )  ;


( read multiple 64 byte chunks )
: CH-SECTOR-READ ( LBA-L LBA-H buffer -- )
   blkptr !     ( save buffer )
   CHRESET
   90 TDELAY

   CHCMD_DSKRD CHPORT_CMD PC!
   90 TDELAY
   LBA>CHBUFFER

   CHPOLL ( DUP . ) 1D  <> ABORT" READ ERROR"
     CHUSBRD
     CHBUFFER  COUNT blkptr @ SWAP  MOVE
     40 blkptr +!

   7 0 DO
     CHCMD_DSKRDGO   CHPORT_CMD PC!
     CHPOLL 1D <> ABORT" READGO ERROR"
       CHUSBRD
       CHBUFFER COUNT blkptr @ SWAP  MOVE
       40 blkptr +!
   LOOP   ;

: CH-SECTOR-WRITE ( LBA-L LBA-H buffer -- )
   blkptr !     ( save buffer )
   CHRESET  90 TDELAY

   CHCMD_DSKWR CHPORT_CMD PC!
   90 TDELAY
   LBA>CHBUFFER

   CHPOLL  DUP .  1E  <> ABORT" WRITE ERROR"
   ." done 1.5" CR
   7 0 DO
       CHUSBWR
       CHCMD_DSKWRGO   CHPORT_CMD PC!
     CHPOLL 1E <> ABORT" WRITEGO ERROR"
       40 blkptr +!
   LOOP   ;

: CHCHECK?   ( -- f )
   CHCMD_CHECK_EXISTS CHPORT_CMD PC!
   90 TDELAY
   A5 CHPORT_DATA PC!
   90 TDELAY
   CHPORT_DATA PC@   5A =   ;

: /CHUSB
   CHCHECK? IF
     CHRESET
     ['] CH-SECTOR-READ  SECTRDVEC !
     ['] CH-SECTOR-WRITE SECTWRVEC !
     ." CHUSB OK" CR
   ELSE
    ." NO CHUSB" CR EXIT
   THEN  ;

: CHDISKSIZE
   CHCMD_DISK_SIZE CHPORT_CMD PC!
   90 TDELAY
   CHPOLL  14 <>  IF ." CHDISKSIZE failed = " . EXIT THEN   ( 14 = success. TODO fix "no media" currently hangs )
   ( read size byte -- should be 8 -- read 8 bytes )
   CHUSBRD
 ;

: CHDISKINQ
   CHCMD_DISK_INQ CHPORT_CMD PC!
   CHPOLL  14 <>  IF ." CHDISKINQ failed = " . EXIT THEN   ( 14 = success. TODO fix "no media" currently hangs )

   ( read size byte -- should be 36 -- read 36 bytes )
   CHUSBRD
 ;


.S CR
/CHUSB
.S CR
CHDISKSIZE   CHBUFFER 10 MEMDUMP
CHDISKINQ    CHBUFFER 40 MEMDUMP

CHRESET
0 DSK !
1 LIST

