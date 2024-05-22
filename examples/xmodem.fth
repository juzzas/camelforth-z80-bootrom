\ XMODEM
\ - inspired by https://github.com/GrantMeStrength/Forth/blob/master/xmodem
\ ** References
\
\http://web.mit.edu/6.115/www/amulet/xmodem.htm
\https://pythonhosted.org/xmodem/xmodem.html
\https://gist.github.com/zonque/0ae2dc8cedbcdbd9b933
\https://www.complang.tuwien.ac.at/forth/ftp.dei.isep.ipp.pt/pub/forth/ibm/xmodem.fth

\ ** Dictionary

0 VARIABLE CHKSUM
0 VARIABLE REX#

1 CONSTANT SOH
4 CONSTANT EOT
6 CONSTANT ACK
21 CONSTANT NAK

DEFER XM-SEND    ' EMIT IS XM-SEND
DEFER XM-GET     ' KEY IS XM-GET
DEFER XM-GET?    ' KEY? IS XM-GET?

: WAIT ( -- )    ( wait for NAK )
    BEGIN XM-GET NAK = UNTIL ;

: CALCSUM       ( calc the checksum )
    0 SWAP 128 OVER +
    SWAP
    DO
      I C@ +
    LOOP
    255 AND CHKSUM ! ;

: INCREX#     ( increase block number )
  REX# @ 1+ 255 AND
  REX# ! ;

: SENDBLOCK  (  address - )    ( send 128 bytes with checksum )
   DUP CALCSUM SOH XM-SEND REX#
   @ DUP XM-SEND 255 SWAP
   - XM-SEND 128 0
   DO
  	DUP C@ XM-SEND 1+
   LOOP
   CHKSUM @ XM-SEND 5 0
   DO
  	XM-GET ACK =
  	 IF
  	.” OK “
  	0 CHKSUM ! LEAVE
   	ELSE
  	-1 CHKSUM !
   	THEN
   LOOP
   CHKSUM @ -1 =
   IF .” Error”  THEN
  ;


: TRANSMIT    ( start address, length -- )
  128 / WAIT CR .” Sending.. “
  1 REX# ! 0
  DO
  	CR REX# @ .” Block: “
  	. SENDBLOCK INCREX#
  LOOP
  CR .” sent..”
  DROP EOT XM-SEND 5 0
  	DO
  		XM-GET ACK =
  		IF
  		.” done.”
  		LEAVE
  		THEN
  	LOOP
   CR ;

: XM-SYNC    ( -- )
   ;

: RECEIVE    ( start address -- )
  128 / WAIT CR .” Sending.. “
  1 REX# ! 0
  DO
  	CR REX# @ .” Block: “
  	. SENDBLOCK INCREX#
  LOOP
  CR .” sent..”
  DROP EOT XM-SEND 5 0
  	DO
  		XM-GET ACK =
  		IF
  		.” done.”
  		LEAVE
  		THEN
  	LOOP
   CR ;
