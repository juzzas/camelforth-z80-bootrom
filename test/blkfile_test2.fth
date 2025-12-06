.( Testing blkfidpool ) CR

blkfile-private-wid >ORDER

T{ blkfidpool-get VALUE fid1 -> }T
T{ blkfidpool-get VALUE fid2 -> }T
T{ fid1 0 = -> FALSE }T
T{ fid2 0 = -> FALSE }T
T{ fid1 fid2 = -> FALSE }T

.( Testing blkfidpool reuse ) CR
T{ fid1 blkfidpool-free -> }T
T{ blkfidpool-get VALUE newfid1 -> }T
T{ fid1 newfid1 = -> TRUE }T
T{ fid2 newfid1 = -> FALSE }T

.( Testing blkfidpool allocation ) CR
T{ blkfidpool-get VALUE fid3 -> }T
T{ fid3 0 = -> FALSE }T
T{ fid1 fid3 = -> FALSE }T
T{ fid2 fid3 = -> FALSE }T

64 BUFFER: read-buffer


.( Testing OPEN-BLKFILE READ-FILE and CLOSE-BLKFILE )  CR   .S
read-buffer 64 0 FILL
T{ read-block R/O OPEN-BLKFILE TO fid1 -> }T
T{ read-buffer 4 fid1 READ-FILE .S -> 4 0 }T
T{ fid1 CLOSE-FILE -> 0 }T
T{ read-buffer C@  -> 'T' }T
T{ read-buffer 1 + C@  -> 'H' }T
T{ read-buffer 2 + C@  -> 'I' }T
T{ read-buffer 3 + C@  -> 'S' }T
T{ read-buffer 4 + C@  -> 0 }T

.( Testing OPEN-LIMIT-BLKFILE READ-FILE and CLOSE-BLKFILE )  CR
read-buffer 64 0 FILL
T{ read-block 1 R/O OPEN-LIMIT-BLKFILE TO fid1 -> }T
T{ fid1 0 = -> FALSE }T
T{ read-buffer 4 fid1 READ-FILE .S -> 4 0 }T
fid1 .BLKFILE
fid1 BLKF.SLICE  .SLICE
T{ fid1 CLOSE-FILE -> 0 }T
T{ read-buffer C@  -> 'T' }T
T{ read-buffer 1 + C@  -> 'H' }T
T{ read-buffer 2 + C@  -> 'I' }T
T{ read-buffer 3 + C@  -> 'S' }T
T{ read-buffer 4 + C@  -> 0 }T
