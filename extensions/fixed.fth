\ 32-bit fixed point maths                           0 / 5

\ These were taken from Real Time Forth by Tim Hendtlass
\ These require the "double" routines LOADed
\ By default, the default is scaled to 3 decimal places.

CR .( Loading fixed point decimal definitions... )

FORTH DEFINITIONS
DECIMAL
1 6 +THRU





  \ 32-bit fixed point maths                         1 / 5
VARIABLE FDPL \ holds number of implied decimal places
VARIABLE FSCL \ holds the scaling factor we are using

: FPLACES ( -- n ) FDPL @ ; \ return number of implied decimal places
: FSCALE ( -- n ) FSCL @ ; \ return the scaling factor we are using
: FIXED ( n -- )
   0 MAX 3 MIN FDPL ! \ clip to between 0 and 3 decimal places
   1 FDPL @ 0 ?DO 10 * LOOP FSCL ! \ store # places, initialise scaling factor
;

 3 FIXED \ default to three decimal places




  \ 32-bit fixed point maths                         2 / 5
\ Outputting numbers
: (F.) ( fn -- adr len ) \ prepare fixed point # ready to output
   TUCK   \ keep copy of top byte so we know sign
   DABS   \ convert to positive number
   <# BL HOLD   \ start conversion with a leading blank
   FDPL @ 0 ?DO # LOOP   \ convert places after decimal point
   [CHAR] . HOLD   \ put a decimal point in place
   #S \ convert integer part
   ROT SIGN #>  ;   \ put sign in place, tidy stack
: F. ( fn -- ) (F.) TYPE ;   \ print fixed point number
: F.R ( fn p -- ) \ print right justified in a field of p places
   >R (F.)   \ convert
   R> OVER - 0 ?DO BL EMIT LOOP   \ pad with blanks as needed
   TYPE  ;    \ then print

  \ 32-bit fixed point maths                         3 / 5
\  Divide unsigned double by a single, leaving a remainder and quotient.
: MU/MOD        ( ud# un1 -- rem d#quot )
  >R  0  R@  UM/MOD  R>  SWAP  >R  UM/MOD  R>   ;

\ Inputting numbers
: D10* ( d1 -- 10*d1)   \ multiply a 32 bit number by 10
   D2* 2DUP D2* D2* D+   \ 8*d+2*d=10*d
;
: FIX ( dn -- fn )
     FPLACES 0 ?DO D10* LOOP  \ scale the number up
;
: SFIX ( n -- fn )
     S>D FIX ;
;

  \ 32-bit fixed point maths                         4 / 5
\ Multiply two fixed point numbers producing a fixed point result.
: FIX* ( f1 f2 -- f1*f2 )
   ROT 2DUP XOR >R   \ sign of answer to return stack
   -ROT DABS 2SWAP DABS   \ make both numbers positive
   DUP >R ROT DUP >R >R OVER >R   \ put a c c b on return stack
   >R SWAP DUP >R   \ put a d onto return stack
   UM*   \ b*d
   0 2R> UM* D+ 2R> UM* D+   \ offset 16 bits, add on a*d+b*c
   2R> * +   \ add on low byte of a*c
   FSCALE MU/MOD   \ divide ms32 bits, ans to R.
   0<> ABORT" Fixed * Overflow" >R   \ unless overflow quotient to R....
   FSCALE MU/MOD ROT DROP   \ divide remainder and last 16 bits
   R> + R> ?DNEGATE   \ assemble final answer, negate if required
;

  \ 32-bit fixed point maths                         5 / 5
\ Divide two fixed point numbers producing a fixed point result.
: FIX/ ( f1 f2 -- fquot=f1/f2 )  \ Divide two fixed point numbers
   2 PICK OVER XOR >R   \ work out sign of answer and save
   DABS 2SWAP DABS 2SWAP   \ make all numbers positive
   2DUP >R >R   \ keep copy of divisor
   DD/MOD FSCALE 0 DD*   \ scale integer part of answer
   2SWAP FSCALE 0 DD*   \ and then scale remainder
   R> R> DD/   \ divide remainder by divisor
   D+   \ add fractional part of ans
   R> ?DNEGATE   \ put on final sign
;

