\ FIXED.SCR                                  hhh 12:30 02/09/97


.( Loading fixed point decimal definitions... )

FORTH DEFINITIONS
DECIMAL
1 6 +THRU








   \ FIXED.SCR    --- readme                 hhh 12:30 02/09/97
\  written using LMI PC/FORTH 3.2
\  Heinrich Hohl, Lucent Technologies
\ 
\ This package facilitates the use of fixed point double length
\ numbers (fd). These are double length numbers containing an
\ implied decimal point at a known, fixed position. The 
\ following words allow easy handling of fixed point numbers:
\ PLACES ................ determine position of decimal point
\ (FD.) FD. FD.R ........ used to display fd numbers
\ FIXED ................. convert any d number to fd number
\ D+ D- D* D/ ........... calculate with d or fd numbers
\ T* T/ TU* TU/ TU// .... basic triple length number operators
\ D*/ DU*/ DU*// ........ scale d or fd numbers; triple length
\                         intermediate results are used
   \ FIXED.SCR    --- variables              hhh 12:30 02/09/97
VARIABLE places        ( number of digits behind decimal point )














   \ FIXED.SCR    --- number input           hhh 12:30 02/09/97
( specify number of places behind the decimal point )
: PLACES ( n -- )  0 MAX  places ! ;

( decimal left shift (n<0 shifts |n| digits to the right )
: DSHIFT ( d n -- d')
  DUP 0<
  IF    NEGATE
        ?DUP IF 0 DO 10 D/  LOOP THEN
  ELSE  ?DUP IF 0 DO 10 UD*  LOOP  THEN   THEN;

( convert double length number to fd considering DPL )
: FIXED ( d -- fd)
  places @  DPL @ 0 MAX -  DSHIFT ;


   \ FIXED.SCR    --- formatted output       hhh 12:30 02/09/97
( convert fixed point double length number to formatted string )
: (FD.) ( fd -- addr len)
  TUCK DABS
  <#  places @  ?DUP IF 0 DO # LOOP THEN  ASCII .
      HOLD  #S  ROT SIGN  #> ;

( display fixed point double length number )
: FD. ( fd -- )  (FD.) TYPE SPACE ;

( display number right justified in a field of specified width )
: FD.R ( fd width -- )  >R  (FD.)  R> OVER - SPACES  TYPE ;




   \ FIXED.SCR    --- extended arithmetics   hhh 12:30 02/09/97
: UM/ ( ud u -- u')  UM/MOD NIP ;

( multiply or divide signed long number by 0 <= v <= 7FFF )
: T* ( d v -- t)  TUCK M* >R >R UM* 0 R> R> D+ ;
: T/ ( t v -- d)  DUP >R M/MOD -ROT R> UM/ SWAP ;

( multiply or divide unsigned long numbers by unsigned number )
: TU* ( ud u -- ut)  TUCK UM* >R >R UM* 0 R> R> D+ ;
: TU/ ( ut u -- ud)  DUP >R UM/MOD -ROT R> UM/ SWAP ;

( divide ut number by ud number )
: TU// ( ut ud -- u)
  DUP 1+
  DUP >R UM/ R> SWAP >R TU/ R> UM/ ;

   \ FIXED.SCR    --- scaling                hhh 12:30 02/09/97
( scale double length number d according to the unsigned )
( numbers v1 and v2 of range 0 <= v <= 7FFF: (d*v1)/v2 = d' )
: D*/ ( d v1 v2 -- d')  >R T* R> T/ ;

( scale unsigned double length number ud according to the )
( unsigned numbers u1 and u2: (ud*u1)/u2 = ud' )
: DU*/ ( ud u1 u2 -- ud')  >R TU* R> TU/ ;

( scale unsigned number u according to the unsigned double )
( length numbers ud1 and ud2: (u*ud1)/ud2 = u' )
: DU*// ( u ud1 ud2 -- u')  >R >R ROT TU* R> R> TU// ;

( tip: avoid fussy stack operations by using basic triple )
( length arithmetic operators for scaling instead of the  )
( scaling operators shown in this screen                  )
