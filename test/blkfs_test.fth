.( Testing blkfs ) CR
ONLY FORTH ALSO SYSTEM
blkfile-private-wid >ORDER

.( Testing MKFS ROOT! CWD ) CR
T{ test-blkfs test-blkfs% MKFS -> }T
T{ test-blkfs ROOT! -> }T
T{ root @  ->  test-blkfs }T
T{ cwd @   ->  test-blkfs }T

TESTING CREATE-FILE CLOSE-FILE

: FN1 S" fatest1.txt" ;
2VARIABLE fidblk
: FN2 S" fatest2.txt" ;
2VARIABLE fidblk
VARIABLE FID1

T{ 10 FN1 $creat fidblk 2!  -> }T
T{ fidblk 2@ DROP -> test-blkfs 1 + }T
T{ fidblk 2@ NIP -> test-blkfs 11 + }T
T{ fidblk 2@  W/O OPEN-FENCE-BLKFILE FID1 ! -> }T
T{ FID1 @  0=  -> FALSE  }T
T{ S" Hello!" FID1 @ WRITE-LINE  .S -> 0 }T
FID1 @ .BLKFILE
T{ FID1 @  CLOSE-BLKFILE  -> 0 }T

VARIABLE FID2
100 BUFFER: buf

T{ FN1 R/O  OPEN-FILE  SWAP FID2 ! -> 0 }T
FID2 @ .BLKFILE
T{ buf 100 FID2 @ READ-LINE  .S -> 6 TRUE 0 }T
FID2 @ .BLKFILE
T{ FID2 @  CLOSE-FILE  -> 0 }T


T{ FN2 R/W CREATE-FILE SWAP FID1 ! -> 0 }T
FID1 @ .BLKFILE
T{ FID1 @ CLOSE-FILE -> 0 }T

LS

T{ FN2 R/W OPEN-FILE SWAP FID1 ! -> 0 }T
T{ S" Hello!" FID1 @ WRITE-LINE  .S -> 0 }T
FID1 @ .BLKFILE
T{ FID1 @ CLOSE-FILE -> 0 }T

LS

T{ FN2 R/O  OPEN-FILE  SWAP FID2 ! -> 0 }T
T{ buf 100 FID2 @ READ-LINE  .S -> 6 TRUE 0 }T
T{ buf 100 FID2 @ READ-LINE  .S -> 0 FALSE 0 }T
FID2 @ .BLKFILE
T{ FID2 @  CLOSE-FILE  -> 0 }T

