CR .( Starting blkfile_test1 )

1 8 +THRU














TESTING CREATE
T{ CREATE blk-id  BLKF% ALLOT -> }T
T{ test-block blk-id /BLKF -> }T
T{ blk-id BLKF.BLK @  -> test-block }T
T{ blk-id BLKF.OFFSET @  -> 0 }T
T{ blk-id BLKF.ORIGIN @  -> test-block }T


TESTING BLKF-GETCHARS
64 BUFFER: read-buffer
read-block blk-id /BLKF
read-buffer 64 0 FILL
T{ read-buffer 4 blk-id BLKF-GETCHARS  -> 4 }T
T{ read-buffer C@  -> 'T' }T
T{ read-buffer 1 + C@  -> 'H' }T
T{ read-buffer 2 + C@  -> 'I' }T
T{ read-buffer 3 + C@  -> 'S' }T
T{ read-buffer 4 + C@  -> 0 }T

TESTING BLKF-GETCHARS >1024 bytes
2048 BUFFER: read-2048
read-block blk-id BLKF.BLK !
0 blk-id BLKF.OFFSET !
read-2048 2048 0 FILL
T{ read-2048 1028 blk-id BLKF-GETCHARS  -> 1028 }T
T{ read-2048 C@  -> 'T' }T
T{ read-2048 1 + C@  -> 'H' }T
T{ read-2048 2 + C@  -> 'I' }T
T{ read-2048 3 + C@  -> 'S' }T
T{ read-2048 4 + C@  -> 32 }T
T{ read-2048 1024 + C@  -> 'B' }T
T{ read-2048 1025 + C@  -> 'L' }T
T{ read-2048 1026 + C@  -> 'K' }T
T{ read-2048 1027 + C@  -> '2' }T
T{ read-2048 1028 + C@  -> 0 }T


TESTING BLKF-PUTCHARS
CR .( Should say "writing test" on top line of screen )
 test-block U. CR
: str1   S" writing test" ;
test-block WIPE FLUSH

T{ test-block blk-id /BLKF -> }T
T{ str1 blk-id BLKF-PUTCHARS  -> 12 }T

BLKF-FLUSH
test-block LIST

CR .( Should say "writing testTHIS IS A TEST PUT" on top line )
CR .( of screen ) test-block U.
T{ S" THIS IS A TEST PUT"  blk-id BLKF-PUTCHARS  -> 18 }T
BLKF-FLUSH
test-block LIST

TESTING SOURCE context initialisation
VARIABLE sc1-a   FALSE sc1-a !
VARIABLE sc1-b   FALSE sc1-b !
VARIABLE sc1-c   FALSE sc1-c !

: sc1-refill  ( -- f ) 
   CR ." called refill"
   TRUE sc1-a !  TRUE  ;

: sc1-refetch  ( -- )
   CR ." called refetch"
   TRUE sc1-b !  ;

T{ ' sc1-refill  ' sc1-refetch  SOURCE:  sc1  -> }T

T{ sc1 SOURCE.REFILL @  -> ' sc1-refill }T
T{ sc1 SOURCE.REFETCH @  -> ' sc1-refetch }T

TESTING SOURCE context REFILL
: test-sc1  ( source-id -- )
   SAVE-INPUT N>R
   SET-SOURCE
   REFILL  sc1-c !
   NR>  RESTORE-INPUT DROP  ;

T{ sc1 test-sc1 -> }T
T{ sc1-a @ -> TRUE }T
T{ sc1-b @ -> FALSE }T
T{ sc1-c @ -> TRUE }T


TESTING SOURCE context refetch
: test-sc2  ( source-id -- )
   SAVE-INPUT N>R
   SET-SOURCE
   S" TRUE sc1-c !"  EVALUATE
   NR>  RESTORE-INPUT  DROP ;

FALSE sc1-a !
FALSE sc1-b !
FALSE sc1-c !

T{ sc1 test-sc2 -> }T
T{ sc1-a @ -> FALSE }T
T{ sc1-b @ -> TRUE }T
T{ sc1-c @ -> TRUE }T

CR .( Finished blkfile_test1 )

