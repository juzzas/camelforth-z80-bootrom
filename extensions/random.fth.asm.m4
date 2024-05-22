;Z (XORSHIFT) ( n -- n   xorshift random number generator )
;    DUP 7 LSHIFT XOR
;    DUP 9 RSHIFT XOR
;    DUP 8 LSHIFT XOR ;
    head(XXORSHIFT,(XORSHIFT),docolon)
        DW DUP,lit,7,LSHIFT,XOR
        DW DUP,lit,9,RSHIFT,XOR
        DW DUP,lit,8,LSHIFT,XOR
        DW EXIT

;: RND  ( -- n   generate random 16bit value from seed )
;    SEED @
;    (XORSHIFT)
;    DUP SEED ! ;
    head(RND,RND,docolon)
        DW SEED,FETCH
        DW XXORSHIFT
        DW DUP,SEED,STORE
        DW EXIT

;: RANDOM (  n -- n  generate random value between 0 and value on stack )
;    ( WARNING: Not evenly distributed but should be good )
;    RND SWAP MOD ABS ;
    head(RANDOM,RANDOM,docolon)
        DW RND,SWOP,MOD,ABS
        DW EXIT

;Z SEED        -- a-addr   address of seed for random number generator
;  VARIABLE SEED
    head(SEED,SEED,docon)
        dw seed_ptr

SECTION data_user

seed_ptr:
    DEFW 0

SECTION code_user_16k
