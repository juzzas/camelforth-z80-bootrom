\ Additional double number definitions
.( Loading additional double number definitions... ) CR
1 +LOAD













   \ Additional double number definitions
: D>  2SWAP D< ;
: D>S  DROP ;  ( d -- s )
: DROT  2>R 2SWAP 2R> 2SWAP ;  ( d1 d2 d3 -- d2 d3 d1 )
: D0<   NIP 32768 AND 0<> ;


: DLSHIFT BEGIN ?DUP WHILE >R D2* R> 1- REPEAT ;
: DRSHIFT BEGIN ?DUP WHILE >R D2/ R> 1- REPEAT ; ( d u -- d )

: DAND ROT AND >R AND R> ; ( d d -- d )
: DOR ROT OR >R OR R> ; ( d d -- d )
: DXOR ROT XOR >R XOR R> ; ( d d -- d )

: 2VARIABLE  ( --      define a Forth double variable )
   CREATE 2 CELLS ALLOT ;

: 2CONSTANT  ( x1 x2  -      define a Forth constant )
;   CREATE , , DOES> 2@  ;

