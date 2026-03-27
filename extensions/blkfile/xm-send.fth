\ xmodem-send.fs
\ ANS Forth - Xmodem 128-byte packet sender
\ Stores data in standard disk blocks (1024 bytes, 8 packets per block)

\ --- Protocol Constants ---
1 CONSTANT SOH
4 CONSTANT EOT
6 CONSTANT ACK
21 CONSTANT NAK
24 CONSTANT CAN

\ --- Transmission Buffers and Counters ---
CREATE block-buf 128 ALLOT        \ Temporary packet buffer
VARIABLE send-block               \ Current 1K disk block
VARIABLE send-sub                 \ Packet offset (0..7) in block
VARIABLE block-count              \ Total number of packets to send
VARIABLE blk#                     \ Current packet number (0..block-count)
VARIABLE retry-count              \ Retry counter per packet

\ --- Utility: Calculate simple checksum ---
: checksum ( addr -- sum )
  0 128 0 DO DUP I + C@ + LOOP DROP 255 AND ;

\ --- Read current packet into temporary buffer ---
: fetch-packet ( addr -- )
  send-block @ BLOCK
  send-sub @ 128 * + SWAP 128 MOVE ;

\ --- Advance send-block and subindex after successful send ---
: advance-send
  send-sub @ 1+ DUP 8 =
  IF DROP
    0 send-sub !
    send-block @ 1+ send-block !
  ELSE
    send-sub !
  THEN ;

\ --- Send one Xmodem packet from addr and blk# ---
: send-packet ( addr blk# -- )
  SOH EMIT
  DUP EMIT
  255 XOR EMIT
  OVER 128 TYPE
  OVER checksum EMIT
  2DROP ;

\ --- Abort transmission cleanly ---
: cancel-transfer
  CAN EMIT CAN EMIT
  ." Sender cancelled." CR ;

\ --- Wait for receiver NAK to begin ---
: wait-for-nak
  BEGIN
    KEY? IF KEY NAK = ELSE FALSE THEN
  UNTIL ;

\ --- Main sender entry point ---
: send-file ( start-block num-packets -- )
  block-count !
  0 send-sub !
  send-block !
  0 blk# !
  wait-for-nak
  BEGIN
    blk# @ block-count @ < WHILE
      block-buf fetch-packet
      0 retry-count !
      BEGIN
        block-buf blk# @ send-packet
        1000 timed-key
        IF
          DUP ACK = IF
            DROP
            blk# @ 1+ blk# !
            advance-send
            LEAVE
          ELSE DROP
            retry-count @ 1+ DUP retry-count !
            5 > IF cancel-transfer ABORT THEN
          THEN
        ELSE
          ." Timeout—resending." CR
          retry-count @ 1+ DUP retry-count !
          5 > IF cancel-transfer ABORT THEN
        THEN
      AGAIN
  REPEAT
  EOT EMIT ." File sent." CR ;