303 LOAD     \ load ttester

.( Starting stack test ) CR
ALSO UTILS
1 6 +THRU
PREVIOUS
CR .( Finished stack test ) CR









4 STACK: test

: m1 1  ;
: m2 2  ;
: m3 3  ;
: m4 4  ;

: s1 1 0 ; \ 0 means continue with STACK-UNTIL
: s2 2 0 ;
: s3 3 0 ;
: s4 4 -1 ; \ -1 means premature exit from STACK-UNTIL





TESTING set and get methods
T{ -1 test ' STACK-SET CATCH -> -1 test -4 }T
T{ 0 test STACK-SET -> }T
T{ test STACK-GET -> 0 }T
T{ ' s1 1 test STACK-SET -> }T
T{ test STACK-GET -> ' s1 1 }T
T{ 1 2 4 3 test STACK-SET -> }T
T{ test STACK-GET -> 1 2 4 3 }T
T{ ' s1 ' s2 ' s3 3 test STACK-SET -> }T
T{ test STACK-GET -> ' s1 ' s2 ' s3 3 }T

TESTING testing stack-map
\ the whole stack is used for execute
T{ ' m1 ' m2 ' m3 3 test STACK-SET -> }T
T{ ' EXECUTE test STACK-MAP -> 3 2 1 }T

TESTING testing stack-until
\ the whole stack is used for execute until flag aborts
T{ ' s1 ' s2 ' s4 3 test STACK-SET -> }T
\ only the 1st element is executed
T{ ' EXECUTE test STACK-UNTIL -> 4 -1 }T

TESTING append and prepend methods
T{ ' s1 1 test STACK-SET -> }T
T{ ' s2 test >S -> }T
T{ test STACK-GET -> ' s1 ' s2 2 }T
T{ test S> -> ' s2 }T
T{ test STACK-GET -> ' s1 1 }T

TESTING depth
T{ 2 1 0 3 test STACK-SET -> }T
T{ test SDEPTH -> 3 }T

TESTING bounds
: do-bounds   STACK-BOUNDS ?DO @ CELL +LOOP ;
T{ test do-bounds -> }T

TESTING fetch and store
T{ 2 1 0 3 test STACK-SET -> }T
T{ test S@ -> 0 }T
T{ 4 test S! -> }T
T{ test S@ -> 4 }T
T{ test STACK-GET -> 2 1 4 3 }T

TESTING pick
T{ 2 1 0 3 test STACK-SET -> }T
T{ 0 test SPICK -> 0 }T
T{ 1 test SPICK -> 1 }T
\ T{ -1 test ' SPICK CATCH -> -1 test -9 }T
\ T{ 5 test ' SPICK CATCH -> 5 test -9 }T
