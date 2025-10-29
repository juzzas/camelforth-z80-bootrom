.( Testing clearing blkf context )   CR

CREATE blk-id   BLKF% ALLOT
T{ test-block blk-id /BLKF -> }T
T{ blk-id BLKF.BLK @  -> test-block }T
T{ blk-id BLKF.OFFSET @  -> 0 }T
T{ blk-id BLKF.ORIGIN @  -> test-block }T


.( Testing blkf>offset+ ) CR
64 BUFFER: read-buffer
T{ read-block blk-id /BLKF -> }T
T{ 16 blk-id blkf>offset+ -> }T
T{ blk-id BLKF.BLK @  -> read-block }T
T{ blk-id BLKF.OFFSET @  -> 16 }T

T{ 1 blk-id blkf>offset+ -> }T
T{ blk-id BLKF.BLK @  -> read-block }T
T{ blk-id BLKF.OFFSET @  -> 17 }T

T{ 1024 blk-id blkf>offset+ -> }T
T{ blk-id BLKF.BLK @  -> read-block 1+ }T
T{ blk-id BLKF.OFFSET @  -> 17 }T

.( testing blkf>offset+ wrap ) CR
read-block blk-id BLKF.BLK !
1022 blk-id BLKF.OFFSET !

T{ 1 blk-id blkf>offset+ -> }T
T{ blk-id BLKF.BLK @  -> read-block }T
T{ blk-id BLKF.OFFSET @  -> 1023 }T

T{ 1 blk-id blkf>offset+ -> }T
T{ blk-id BLKF.BLK @  -> read-block 1+ }T
T{ blk-id BLKF.OFFSET @  -> 0 }T


.( Testing bytes-to-read ) CR
read-block blk-id BLKF.BLK !

.( Testing bytes-to-read at 0 ) CR
0 blk-id BLKF.OFFSET !
T{ 1 blk-id bytes-to-read -> 1 }T
T{ 2 blk-id bytes-to-read -> 2 }T
T{ 3 blk-id bytes-to-read -> 3 }T
T{ 1024 blk-id bytes-to-read -> 1024 }T

.( Testing bytes-to-read at 1022 ) CR
1022 blk-id BLKF.OFFSET !
T{ 1 blk-id bytes-to-read -> 1 }T
T{ 2 blk-id bytes-to-read -> 2 }T
T{ 3 blk-id bytes-to-read -> 2 }T
T{ 1024 blk-id bytes-to-read -> 2 }T

.( Testing incr-index ) CR
T{ 8000 10 1 incr-index -> 8001 9 }T
T{ 8000 10 9 incr-index -> 8009 1 }T
T{ 8000 10 10 incr-index -> 8010 0 }T

.( Testing blkf>bufferidx ) CR
read-block blk-id BLKF.BLK !
1022 blk-id BLKF.OFFSET !

blk-id  blkf>bufferidx   U. CR
0 blk-id BLKF.OFFSET !
blk-id  blkf>bufferidx   U. CR

1023 blk-id BLKF.OFFSET !
blk-id  blkf>bufferidx   U. CR

read-block 1+  blk-id BLKF.BLK !
1023 blk-id BLKF.OFFSET !
blk-id  blkf>bufferidx   U. CR

.( Testing XXBLKF-GETCHARS ) CR
read-block blk-id BLKF.BLK !
0 blk-id BLKF.OFFSET !
read-buffer 64 0 FILL
T{ read-buffer 4 blk-id ((BLKF-GETCHARS))  -> read-buffer 4 +  0 }T
T{ read-buffer C@  -> 'T' }T
T{ read-buffer 1 + C@  -> 'H' }T
T{ read-buffer 2 + C@  -> 'I' }T
T{ read-buffer 3 + C@  -> 'S' }T
T{ read-buffer 4 + C@  -> 0 }T

.( Testing XBLKF-GETCHARS ) CR
read-block blk-id BLKF.BLK !
0 blk-id BLKF.OFFSET !
read-buffer 64 0 FILL
T{ read-buffer 4 blk-id (BLKF-GETCHARS)  -> read-buffer 4 +  0 }T
T{ read-buffer C@  -> 'T' }T
T{ read-buffer 1 + C@  -> 'H' }T
T{ read-buffer 2 + C@  -> 'I' }T
T{ read-buffer 3 + C@  -> 'S' }T
T{ read-buffer 4 + C@  -> 0 }T


.( Testing BLKF-GETCHARS ) CR
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

