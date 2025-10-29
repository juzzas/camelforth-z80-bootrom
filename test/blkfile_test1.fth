.( Testing clearing blkf context )   CR

CREATE blk-id   BLKF% ALLOT
T{ test-block blk-id /BLKF -> }T
T{ blk-id BLKF.BLK @  -> test-block }T
T{ blk-id BLKF.OFFSET @  -> 0 }T
T{ blk-id BLKF.ORIGIN @  -> test-block }T


.( Testing BLKF-GETCHARS ) CR
64 BUFFER: read-buffer
read-block blk-id BLKF.BLK !
0 blk-id BLKF.OFFSET !
read-buffer 64 0 FILL
T{ read-buffer 4 blk-id BLKF-GETCHARS  -> 4 }T
T{ read-buffer C@  -> 'T' }T
T{ read-buffer 1 + C@  -> 'H' }T
T{ read-buffer 2 + C@  -> 'I' }T
T{ read-buffer 3 + C@  -> 'S' }T
T{ read-buffer 4 + C@  -> 0 }T

.( Testing BLKF-GETCHARS >1024 bytes ) CR
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


.( Testing BLKF-PUTCHAR ) CR

T{ 'H' blk-id BLKF-PUTCHAR  -> }T
T{ 'I' blk-id BLKF-PUTCHAR  -> }T

test-block LIST

