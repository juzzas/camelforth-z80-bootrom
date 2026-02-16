CR .( Starting blkfile_test3 )
\ basic blkflie source tests

1 8 +THRU












TESTING sourcepool

blkfile-private-wid >ORDER

T{ sourcepool-get VALUE sid1 -> }T
T{ sourcepool-get VALUE sid2 -> }T
T{ sid1 0 = -> FALSE }T
T{ sid2 0 = -> FALSE }T
T{ sid1 sid2 = -> FALSE }T

TESTING sourcepool reuse
T{ sid1 sourcepool-free -> }T
T{ sourcepool-get VALUE newsid1 -> }T
T{ sid1 newsid1 = -> TRUE }T
T{ sid2 newsid1 = -> FALSE }T

TESTING sourcepool allocation
T{ sourcepool-get VALUE sid3 -> }T
T{ sid3 0 = -> FALSE }T
T{ sid1 sid3 = -> FALSE }T
T{ sid2 sid3 = -> FALSE }T

TESTING SET-SOURCE with SOURCE-ID
: bf1  ( -- f )
   SAVE-INPUT N>R
   sourcepool-get  DUP  SET-SOURCE
   SOURCE-ID  =
   NR>  RESTORE-INPUT DROP ;

T{ bf1  -> TRUE }T

TESTING source calls refill and refetch
VARIABLE bf2-a   FALSE bf2-a !
VARIABLE bf2-b   FALSE bf2-b !
VARIABLE bf2-c   FALSE bf2-c !

: bf2-refill  ( -- f )  TRUE bf2-a !  TRUE  ;
: bf2-refetch  ( -- )  TRUE bf2-b !  ;

T{ sourcepool-get TO sid1  -> }T
T{ ' bf2-refill  sid1 source.source SOURCE.REFILL !  -> }T
T{ ' bf2-refetch  sid1 source.source SOURCE.REFETCH !  -> }T

: bf2  ( source-id -- )
   SAVE-INPUT N>R
   sid1 SET-SOURCE
   REFILL  bf2-c !
   S" CR .( called evaluate ) "  EVALUATE
   NR>  RESTORE-INPUT  DROP  ;

T{ bf2 -> }T
T{ bf2-a @ -> TRUE }T
T{ bf2-b @ -> TRUE }T
T{ bf2-c @ -> TRUE }T

T{ sid1 sourcepool-free -> }T

CR .( before TLIST )
3660 TLIST
CR .( after TLIST )


TESTING include INCLUDE-BLKFILE
T{ test-blkfile-inc INCLUDE-BLKFILE -> }T
T{ ' blkfile-included  0=  -> FALSE }T
T{ blkfile-included  -> TRUE }T
T{ ' blkfile-nested  0=  -> FALSE }T
T{ blkfile-nested  -> TRUE }T

CR .( Finished blkfile_test3 )
