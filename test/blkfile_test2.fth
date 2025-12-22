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

128 BUFFER: read-buffer


.( Testing OPEN-BLKFILE READ-FILE and CLOSE-BLKFILE )  CR   .S
read-buffer 128 0 FILL
T{ read-block R/O OPEN-BLKFILE TO fid1 -> }T
T{ read-buffer 4 fid1 READ-FILE -> 4 0 }T
T{ fid1 CLOSE-BLKFILE -> 0 }T
T{ read-buffer C@  -> 'T' }T
T{ read-buffer 1 + C@  -> 'H' }T
T{ read-buffer 2 + C@  -> 'I' }T
T{ read-buffer 3 + C@  -> 'S' }T
T{ read-buffer 4 + C@  -> 0 }T

.( Testing OPEN-LIMIT-BLKFILE READ-FILE and CLOSE-BLKFILE )  CR
read-buffer 128 0 FILL
T{ read-block 1 R/O OPEN-LIMIT-BLKFILE TO fid1 -> }T
T{ fid1 0 = -> FALSE }T
T{ read-buffer 4 fid1 READ-FILE -> 4 0 }T
fid1 .BLKFILE
fid1 BLKF.SLICE  .SLICE
T{ fid1 CLOSE-BLKFILE -> 0 }T
T{ read-buffer C@  -> 'T' }T
T{ read-buffer 1 + C@  -> 'H' }T
T{ read-buffer 2 + C@  -> 'I' }T
T{ read-buffer 3 + C@  -> 'S' }T
T{ read-buffer 4 + C@  -> 0 }T

.( Testing OPEN-FENCE-BLKFILE READ-LINE and CLOSE-BLKFILE )  CR
read-buffer 128 0 FILL
T{ ipsum-file ipsum-file 1+  R/O OPEN-FENCE-BLKFILE TO fid1 -> }T
T{ fid1 0 = -> FALSE }T
T{ read-buffer 128 fid1 READ-LINE -> 11 -1 0 }T
T{ fid1 FILE-POSITION -> 12 S>D 0 }T
T{ read-buffer 128 fid1 READ-LINE -> 0  -1 0 }T
T{ fid1 FILE-POSITION -> 13 S>D 0 }T
T{ read-buffer 128 fid1 READ-LINE -> 77 -1 0 }T
T{ fid1 FILE-POSITION -> 91 S>D 0 }T
T{ read-buffer 128 fid1 READ-LINE -> 75 -1 0 }T
T{ fid1 FILE-POSITION -> 167 S>D 0 }T
T{ read-buffer 128 fid1 READ-LINE -> 66 -1 0 }T
T{ fid1 FILE-POSITION .S -> 234 S>D 0 }T
T{ read-buffer 128 fid1 READ-LINE -> 0   0 0 }T
T{ fid1 FILE-POSITION .S -> 234 S>D 0 }T
T{ read-buffer 128 fid1 READ-LINE -> 0   0 0 }T
T{ fid1 FILE-POSITION .S -> 234 S>D 0 }T
T{ 6 S>D fid1 REPOSITION-FILE -> 0 }T
T{ fid1 FILE-POSITION .S -> 6 S>D 0 }T
T{ read-buffer 128 fid1 READ-LINE -> 5  -1 0 }T
read-buffer 128 MEMDUMP
fid1 .BLKFILE
fid1 BLKF.SLICE  .SLICE
T{ fid1 CLOSE-BLKFILE -> 0 }T


.( Testing OPEN-BLKFILE WRITE-LINE and CLOSE-BLKFILE )  CR
test-block WIPE
T{ test-block  R/W OPEN-BLKFILE TO fid1 -> }T
T{ S" Write line 1"  fid1 WRITE-LINE  .S -> 0 }T
T{ fid1 FILE-POSITION .S -> 13 S>D 0 }T
T{ S" Write line 2"  fid1 WRITE-LINE  .S -> 0 }T
T{ fid1 FILE-POSITION .S -> 26 S>D 0 }T
fid1 .BLKFILE
fid1 BLKF.SLICE  .SLICE
T{ fid1 CLOSE-BLKFILE -> 0 }T

test-block LIST

.( Testing OPEN-BLKFILE READ-LINE short buffers )  CR
read-buffer 128 0 FILL
T{ ipsum-file R/O OPEN-BLKFILE TO fid1 -> }T
T{ fid1 0 = -> FALSE }T
T{ read-buffer 3 fid1 READ-LINE  .S  -> 3 -1 0 }T
T{ fid1 FILE-POSITION  .S  -> 3 S>D 0 }T
fid1 .BLKFILE
fid1 BLKF.SLICE  .SLICE
read-buffer 32 MEMDUMP
T{ read-buffer 0 fid1 READ-LINE  .S  -> 0  -1 0 }T
T{ fid1 FILE-POSITION  .S  -> 3 S>D 0 }T
fid1 .BLKFILE
fid1 BLKF.SLICE  .SLICE
T{ fid1 CLOSE-BLKFILE -> 0 }T
