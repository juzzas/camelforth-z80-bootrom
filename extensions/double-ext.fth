\ Additional double number definitions
.( Loading double number extensions... ) CR
1 2 +THRU

GET-CURRENT   ENVIRONMENT-WORDLIST SET-CURRENT
  4294967295. 2CONSTANT MAX-UD    \ largest usable unsigned 
  2147483647. 2CONSTANT MAX-D     \ largest usable signed
SET-CURRENT








\ Additional double number definitions

: 2VALUE CREATE , ,
   DOES> TO-STATE @ IF 2!  FALSE TO-STATE ! ELSE 2@ THEN  ;

: D>  2SWAP D< ;
: D>S  DROP ;  ( d -- s )
: 2ROT  2>R 2SWAP 2R> 2SWAP ;  ( d1 d2 d3 -- d2 d3 d1 )
: D0<   NIP 32768 AND 0<> ;
: DU<       \ ud1 ud2 -- flag
  ROT SWAP 2DUP U< IF
    2DROP 2DROP TRUE
  ELSE
    <> IF  2DROP FALSE  ELSE  U<  THEN
  THEN ;

