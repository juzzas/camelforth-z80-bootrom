\ Additional double number definitions
CR .( Loading double number extensions... )
1 2 +THRU

GET-CURRENT   ENVIRONMENT-WORDLIST SET-CURRENT
  4294967295. 2CONSTANT MAX-UD    \ largest usable unsigned 
  2147483647. 2CONSTANT MAX-D     \ largest usable signed
SET-CURRENT








\ Additional double number definitions

: 2VALUE CREATE , ,
   DOES> TO-STATE @ IF 2!  FALSE TO-STATE ! ELSE 2@ THEN  ;

: 2ROT  5 ROLL 5 ROLL ;  ( d1 d2 d3 -- d2 d3 d1 )

: DU<       \ ud1 ud2 -- flag
  ROT SWAP 2DUP U< IF
    2DROP 2DROP TRUE
  ELSE
    <> IF  2DROP FALSE  ELSE  U<  THEN
  THEN ;

