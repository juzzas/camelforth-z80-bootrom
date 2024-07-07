( A simple random number generator                  1 / 1 )

: (XORSHIFT) ( n -- n   xorshift random number generator )
    DUP 7 LSHIFT XOR
    DUP 9 RSHIFT XOR
    DUP 8 LSHIFT XOR ;

VARIABLE SEED     42 SEED !
: RND  ( -- n   generate random 16bit value from seed )
    SEED @
    (XORSHIFT)
    DUP SEED ! ;

: RANDOM (  n -- n  generate random value between 0 and TOS )
    ( WARNING: Not evenly distributed but should be good )
    RND SWAP MOD ABS ;