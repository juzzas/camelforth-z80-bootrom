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

: CHPOLL  ( -- status )
   BEGIN
      CHPORT_CMD PC@ 80 AND UNTIL
   1 MS
   CHCMD_STATUS CHPORT_CMD PC!
   1 MS
   CHPORT_DATA PC@ ;


: CHRESET
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
   20 MS
   CHCMD_DISK_INIT CHPORT_CMD PC!
   20 MS
   CHPOLL ( ." disk result = " DUP . )  [CHAR] ? EMIT  14 = UNTIL ( 14 = success. TODO fix "no media" currently hangs )
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
      CHPORT_DATA PC@ CHBUFFER+   [CHAR] . EMIT
   LOOP ;

: CHUSBRD
   CHCMD_RD CHPORT_CMD PC!  1 MS  CHRD  ;

: CHWR ( -- )
   CHBUFFER C@
   0 DO
      CHBUFFER I 1+ + C@   CHPORT_DATA PC!
      [CHAR] + EMIT
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

VARIABLE blkptr 0 blkptr !

( read multiple 64 byte chunks )
: CHRDBLK ( LBA-L LBA-H buffer -- )
   CHRESET
   blkptr !     ( save buffer )
   1 MS
   LBA>CHBUFFER
   1 CHBUFFER+  ( 1 sector )
   CHCMD_DSKRD CHPORT_CMD PC!
     CHWR
   CHPOLL DUP . 1D  <> ABORT" READ ERROR"
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


: CHCHECK?   ( -- f )
   CHCMD_CHECK_EXISTS CHPORT_CMD PC!
   1 MS  A5   CHPORT_DATA PC!
   CHPORT_DATA PC@   5A =   ;

: /CHUSB
   CHCHECK? IF
     CHRESET
     ." CHUSB OK" CR
   ELSE
    ." NO CHUSB" CR EXIT
   THEN  ;

: CHDISKSIZE
   CHCMD_DISK_SIZE CHPORT_CMD PC!
   1 MS
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


/CHUSB
CHDISKSIZE   CHBUFFER 10 MEMDUMP
CHDISKINQ    CHBUFFER 40 MEMDUMP
  400 MS
1 0 A000 CHRDBLK


