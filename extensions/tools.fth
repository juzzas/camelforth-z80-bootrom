\ CamelForth tools                                    jps  0 / 4
.( Loading CamelForth tools definitions... ) CR
FORTH DEFINITIONS
1 4 +THRU












   \ CamelForth tools - environment                   jps  1 / 4
.( creating environment )
WORDLIST CONSTANT ENVIRONMENT-WORDLIST
GET-CURRENT    ENVIRONMENT-WORDLIST SET-CURRENT
  256 CONSTANT /COUNTED-STRING    \ maximum size of a counted string, in characters
  44 CONSTANT /HOLD               \ size of the pictured numeric output string buffer, in characters
  88 CONSTANT /PAD                \ size of the scratch area pointed to by PAD, in characters
  16 CONSTANT ADDRESS-UNIT-BITS   \ size of one address unit, in bits
  FALSE CONSTANT FLOORED          \ true if floored division is the default
  127 CONSTANT MAX-CHAR           \ maximum value of any character in the implementation-defined character set
  32767 CONSTANT MAX-N            \ largest usable signed integer
  65535 CONSTANT MAX-U            \ largest usable unsigned integer
  128 CONSTANT RETURN-STACK-CELLS \ maximum size of the return stack, in cells
  128 CONSTANT STACK-CELLS        \ maximum size of the data stack, in cells
SET-CURRENT

   \ CamelForth tools - environment                   jps  2 / 4
: ?   ( addr -- ) @ U.  ;

VARIABLE ^see

: see@    ( -- u )  ^see @ @ ;
: seeC@    ( -- c )  ^see @ C@ ;
: see@++  ( -- u )  see@  CELL ^see +! ;
: see+!   ( n -- )  ^see +! ;
: seedump ( -- )   ^see @ DUP  COUNT
   DUP 2 U.R ."  bytes: "
   BOUNDS ?DO I C@ 2 U.R SPACE LOOP C@ 1+ see+! ;
: (see-step)   ( -- f   where -1 is "continue" )
   CR ^see @   4 U.R  ." : " 
   see@++  CASE
      ['] EXIT  OF ." EXIT"  FALSE ENDOF
      ['] (S")  OF ['] (S") .ID SPACE seedump TRUE ENDOF
      ['] ?branch  OF ." ?branch --> " see@++ 4 U.R TRUE ENDOF
      ['] branch  OF ." branch --> "  see@++ 4 U.R TRUE ENDOF
      ['] LIT  OF ." LIT "  see@++ .ID TRUE ENDOF
      .ID  TRUE
   ESAC ;

: OF-CREATED? ( xt -- xt f )  DUP ['] ^see  3  STRCMP 0= ;
: OF-COLON?   ( xt -- xt f )  DUP ['] OF-CREATED? 3 STRCMP 0= ;
: OF-USER?    ( xt --xt f )   DUP ['] DP 3 STRCMP 0= ;
: OF-CONSTANT?  ( xt -- xt f )  DUP ['] BL 3 STRCMP 0= ;
\ : OF-DODOES?  ( xt -- xt f )  DUP ['] BL 3 STRCMP 0= ;

: (see-loop)  BEGIN (see-step)  0= UNTIL ;
: (see)  ( xt -- )
    DUP 3 + ^see !  DUP ." WORD: " .ID
    CASE
       OF-COLON? ?OF ."  docolon" (see-loop) ENDOF
       OF-CREATED? ?OF ."  docreate" ENDOF
       OF-USER? ?OF ."  douser" ENDOF
       OF-CONSTANT? ?OF ."  docon" ENDOF
       ."  (CODE)"
    ENDCASE    CR
   ;

: SEE  ( "<spaces>name" -- )
   BASE @ >R HEX
    '  (see)
   R> BASE ! ;
