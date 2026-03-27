\ xmodem-recv.fs - Receive side of Xmodem protocol

1 CONSTANT SOH
4 CONSTANT EOT
6 CONSTANT ACK
21 CONSTANT NAK
24 CONSTANT CAN

CREATE block-buf 128 ALLOT

VARIABLE current-block
VARIABLE sub-index
VARIABLE packet-counter
VARIABLE retry-count

: timed-key ( ms -- c true | false )
  0 DO
    KEY? IF KEY TRUE UNLOOP EXIT THEN
    1 MS
  LOOP FALSE ;

: checksum ( addr -- sum )
  0 128 0 DO DUP I + C@ + LOOP DROP 255 AND ;

: recv-packet ( addr -- ok? )
  500 timed-key
  IF
    DUP CAN = IF DROP FALSE ABORT" Sender cancelled." THEN
    SOH <> IF DROP FALSE EXIT THEN
    timed-key DUP >R
    timed-key DUP R@ XOR 255 <> IF DROP DROP R> DROP FALSE EXIT THEN
    OVER
    128 0 DO
      timed-key 0= IF DROP FALSE EXIT THEN
      DUP CAN = IF DROP FALSE ABORT" Sender cancelled." THEN
      OVER I + C!
    LOOP DROP
    timed-key DUP >R
    OVER checksum R> = IF ACK EMIT TRUE ELSE NAK EMIT FALSE THEN
    R> DROP
  ELSE FALSE THEN ;

: save-packet ( addr -- )
  current-block @ BLOCK
  sub-index @ 128 * + SWAP 128 MOVE
  sub-index @ 1+ DUP 8 =
  IF
    DROP 0 sub-index !
    current-block @ UPDATE
    current-block @ 1+ current-block !
  ELSE sub-index ! THEN ;

: cancel-transfer CAN EMIT CAN EMIT ." Transfer cancelled." CR ;

: reset-receiver
  0 sub-index !
  0 packet-counter !
  current-block ! ;

: receive-file ( start-block -- )
  reset-receiver
  NAK EMIT
  0 retry-count !
  BEGIN
    block-buf recv-packet
    IF
      block-buf save-packet
      packet-counter @ 1+ packet-counter !
      0 retry-count !
    ELSE
      retry-count @ 1+ DUP retry-count !
      5 > IF cancel-transfer ABORT THEN
      NAK EMIT 300 MS
    THEN
    KEY? IF KEY EOT = ELSE FALSE THEN
  UNTIL
  sub-index @ 0 > IF current-block @ UPDATE THEN
  ." Received " packet-counter @ . ." packets." CR ;
