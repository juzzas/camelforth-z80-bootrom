\ Additional double number definitions
.( Loading additional double number definitions... ) CR
1 3 +THRU













\ Additional double number definitions

: 2VALUE CREATE , ,
   DOES> TO-STATE @ IF 2!  FALSE TO-STATE ! ELSE 2@ THEN  ;

GET-CURRENT   ENVIRONMENT-WORDLIST SET-CURRENT
  4294967295. 2CONSTANT MAX-UD    \ largest usable unsigned 
  2147483647. 2CONSTANT MAX-D     \ largest usable signed
SET-CURRENT



   \ Additional double number definitions
: D>  2SWAP D< ;
: D>S  DROP ;  ( d -- s )
: DROT  2>R 2SWAP 2R> 2SWAP ;  ( d1 d2 d3 -- d2 d3 d1 )
: D0<   NIP 32768 AND 0<> ;
: DU<       \ ud1 ud2 -- flag
  ROT SWAP 2DUP U< IF
    2DROP 2DROP TRUE
  ELSE
    <> IF  2DROP FALSE  ELSE  U<  THEN
  THEN ;


: DLSHIFT BEGIN ?DUP WHILE >R D2* R> 1- REPEAT ;
: DRSHIFT BEGIN ?DUP WHILE >R D2/ R> 1- REPEAT ; ( d u -- d )

: DAND ROT AND >R AND R> ; ( d d -- d )
: DOR ROT OR >R OR R> ; ( d d -- d )
: DXOR ROT XOR >R XOR R> ; ( d d -- d )

