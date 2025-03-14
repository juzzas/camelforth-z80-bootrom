\ Additional double number definitions
CR .( Loading additional double number definitions... )
1 5 +THRU













\ Additional double number definitions
: UDD* ( ud1 ud2 -- ud3 ) \ all numbers unsigned doubles
   ROT >R OVER >R >R OVER >R \ put c b a d on return stack
   UM* \ b*d = part of 32 bit answer
   2R> * 2R> * + +   \ a*d+b*c= addition to top 16 bits
;

: DD* ( d1 d2 -- d3 ) \ all numbers signed doubles
   DUP >R DABS 2SWAP DUP >R DABS \ #s +ve, keep info to work out final sign
   UDD* \ get 32 bit answer. 
   2R> XOR ?DNEGATE \ work out and apply final sign
;

: D* ( d n -- d )  S>D DD* ;

: 4DUP 2OVER 2OVER ;


\ Additional double number definitions
: T* ( ud un -- ut ) \ Unsigned double * unsigned single = unsigned triple
   DUP ROT UM* 2>R \ high-part of answer to return stack
   UM* 0 2R> D+ \ get low-part,offset 16 bits,add high-part
;

: T/ ( ut un -- ud ) \ Unsigned triple / unsigned single = unsigned double
   >R R@ UM/MOD SWAP   \ divisor > r, divide top 16 bits, rem to top
   ROT 0 R@ UM/MOD SWAP   \ combine with next 16, divide these by divisor
   ROT R> UM/MOD SWAP DROP   \ repeat for last 16 bits, lose final remainder
   0 2SWAP SWAP D+   \ combine parts of answer to for final answer
;




\ Additional double number definitions
: U*/ ( ud un1 un2 -- ud2 ) \ ud * un1 / un2, triple intermediate product
   >R T* R> T/
;

: UDD/ ( U1 U0 V1V0 -- A1 A0 ) \ Unsigned 32 bit by 32 bit divide. No remainder
   DUP 0=     \ top 16 bits of divisor = 0?
   IF SWAP T/   \ simple case, make it a triple, do /
   ELSE       \ more involved case
      DUP 65536. ROT 1+ UM/MOD >R    \ work out scaling factor,copy to r
      DROP R@ T* DROP 2>R    \ scale denominator, move to return stack
      DUP 0 2R@ U*/ D-    \ calculate (U-U0*W1/W0)
      2R> R> -ROT NIP U*/     \ multiply by (D/W0)
      NIP 0     \ /2^16, make answer double
   THEN  ;

\ Additional double number definitions
: DD/MOD ( dn1 dn2 -- drem dquot ) \ Divide two signed double numbers
   2 PICK OVER XOR >R    \ work out sign of answer
   DABS 2SWAP DABS 2SWAP    \ convert numbers to positive
   4DUP UDD/ 2DUP 2>R    \ do the division, save copy of quotient
   UDD* D-    \ calculate the remainder
   2R> R> ?DNEGATE    \ retrieve answer,apply final sign
;

: DD/ ( dn1 dn2 -- dquot ) \ Divide two signed doubles, no remainder
   2 PICK OVER XOR >R     \ work out sign of answer
   DABS 2SWAP DABS 2SWAP     \ convert numbers to positive
   UDD/     \ do the division
   R> ?DNEGATE    \ retrieve answer,apply final sign
;

\ Additional double number definitions
: D*/  ( d1 d2 d3 -- d )
    2>R DD* 2R> DD/  ;

: DLSHIFT BEGIN DUP WHILE >R D2* R> 1- REPEAT DROP ;
: DRSHIFT BEGIN DUP WHILE >R D2/ R> 1- REPEAT DROP ; ( d u -- d )

: DAND ROT AND >R AND R> ; ( d d -- d )
: DOR ROT OR >R OR R> ; ( d d -- d )
: DXOR ROT XOR >R XOR R> ; ( d d -- d )


