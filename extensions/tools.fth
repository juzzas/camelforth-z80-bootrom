\ CamelForth tools                                    jps  0 / 4
.( Loading CamelForth tools definitions... ) CR
FORTH DEFINITIONS
1 4 +THRU












   \ CamelForth tools - envornment                   jps  1 / 4
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

