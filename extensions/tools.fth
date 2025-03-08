\ CamelForth tools                                    jps  0 / 4
CR .( Loading CamelForth tools definitions... )
FORTH DEFINITIONS
1 5 +THRU












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
VARIABLE see-xt  \ current xt for SEE
: see@    ( -- u )  ^see @ @ ;
: seeC@    ( -- c )  ^see @ C@ ;
: see@++  ( -- u )  see@  CELL ^see +! ;
: see!   ( n -- )  ^see ! ;
: see+!   ( n -- )  ^see +! ;
: seedump ( -- )   ^see @ DUP  COUNT
   DUP 2 U.R ."  bytes: "
   BOUNDS ?DO I C@ 2 U.R SPACE LOOP C@ 1+ see+! ;
: (see-step)   ( -- f   where -1 is "continue" )
   CR ^see @   4 U.R  ." : " 
   see@++ DUP .ID CASE
      ['] EXIT  OF FALSE ENDOF
      ['] (S")  OF seedump TRUE ENDOF
      ['] ?branch  OF  see@++ 4 U.R TRUE ENDOF
      ['] branch  OF ."  --> "  see@++ 4 U.R TRUE ENDOF
      ['] LIT  OF  see@++ .ID TRUE ENDOF
      ['] (DOES>)  OF 3 see+! TRUE ENDOF
      DROP  TRUE
   ESAC ;

: OF-STRCMP:   ( xt -- )    CREATE ,
   DOES>  ( xt -- xt f )     >R DUP R> @  3 STRCMP 0= ;
' ^see  OF-STRCMP: OF-DOCREATE?
' see@  OF-STRCMP: OF-DOCOLON?
' DP OF-STRCMP: OF-DOUSER?
' BL OF-STRCMP: OF-DOCON?
' OF-DOCOLON? 1+ @ 1+ @  CONSTANT DODOES
: OF-DODOES?  DUP 1+ @ 1+ @ DODOES = ;
: (see-loop)  BEGIN (see-step)  0= UNTIL ;
: (see)  ( xt -- )
    DUP see-xt !
    DUP 3 + ^see !  DUP ." WORD: " .ID
    CASE
       OF-DOCOLON? ?OF ."  docolon" (see-loop) ENDOF
       OF-DOCREATE? ?OF ."  docreate" ENDOF
       OF-DOUSER? ?OF ."  douser" ENDOF
       OF-DOCON? ?OF ."  docon" ENDOF
       OF-DODOES? ?OF ."  dodoes" 
            see-xt @ 1+ @ 3 + see! (see-loop) ENDOF
       ."  (CODE)"
    ENDCASE    CR
   ;

: SEE  ( "<spaces>name" -- )
   BASE @ >R HEX
    '  (see)
   R> BASE ! ;
