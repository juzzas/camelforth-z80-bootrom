\ Forth 2012 Core extensions for CamelForth BootROM

.( Loading core-ext definitions... ) CR

ONLY FORTH DEFINITIONS
1 12 +THRU










\ Forth 2012 Core extensions for CamelForth BootROM       1 / n
: DEFER    ( "name" -- )      \  create a deferred word
   CREATE ['] NOOP ,
   DOES>
   @ EXECUTE ;

: DEFER!   ( xt2 xt1 -- )     \        store xt2 in xt1
   >BODY ! ;

: DEFER@   ( xt1 -- xt2 )     \        fetch xt2 from xt1
   >BODY @ ;

: IS       ( xt "name" -- )     \ define a deferred word
   STATE @  IF  POSTPONE [']  POSTPONE DEFER!
   ELSE  ' DEFER!  THEN ; IMMEDIATE

\ Forth 2012 Core extensions for CamelForth BootROM       2 / n

\ get the action of a deferred word
: ACTION-OF  ( "name -- xt" )    
   STATE @  IF POSTPONE ['] POSTPONE DEFER@
   ELSE  ' DEFER@  THEN ; IMMEDIATE

: 2NIP  2SWAP 2DROP  ;
: 2ROT  5 ROLL  5 ROLL ;

VARIABLE TO-STATE  FALSE TO-STATE !
: TO TRUE TO-STATE ! ;
: VALUE CREATE , 
   DOES> TO-STATE @ IF !  FALSE TO-STATE ! ELSE @ THEN  ;


\ Forth 2012 Core extensions for CamelForth BootROM       3 / n

: EXTRACTNUM    ( c-addr len base -- c-addr' len' u )
\ Extract a number in the given base from the start of the
\ string, returning the remaining string starting at the first
\ non-numeric character and the converted number.
  BASE @ >R  BASE !
  0 0 2SWAP >NUMBER 2SWAP DROP
  R> BASE !  ;







\ Forth 2012 Core extensions for CamelForth BootROM       3 / n
CREATE ESCAPETABLE    \  -- addr
 ( a )  7   C,   ( b )  8   C,   ( c )  'c' C,
 ( d )  'd' C,   ( e )  #27 C,   ( f )  #12 C,
 ( g )  'g' C,   ( h )  'h' C,   ( i )  'i' C,
 ( j )  'j' C,   ( k )  'k' C,   ( l )  #10 C,
 ( m )  'm' C,   ( n )  #10 C,   ( o )  'o' C,
 ( p )  'p' C,   ( q )  '"' C,   ( r )  #13 C,
 ( s )  's' C,   ( t )  9   C,   ( u )  'u' C,
 ( v )  #11 C,   ( w )  'w' C,   ( x )  'x' C,
 ( y )  'y' C,   ( z )  0   C,





\ Forth 2012 Core extensions for CamelForth BootROM       3 / n
: ADDESCAPE   \ caddr len dest -- caddr' len'
\ Add an escape sequence to the counted string at dest,
\ returning the remaining string.
  OVER 0=               \ zero length check
  IF  DROP  EXIT  THEN
  >R                    \ -- caddr len ; R: -- dest
  OVER C@ [CHAR] x = IF         \ hex number?
    1 /STRING               \ -- caddr len ; R: -- dest
    >R 2 #16 EXTRACTNUM  NIP R> CELL- SWAP \ -- caddr' len' u ; R: -- dest
    R> ADDCHAR  EXIT
  THEN
  OVER C@ [CHAR] m = IF         \ CR/LF pair
    1 /STRING  #13 R@ ADDCHAR  #10 R> ADDCHAR  EXIT
  THEN
  OVER C@   [CHAR] a  $7b  WITHIN IF   \ between a and z? 
    OVER C@ [CHAR] a - ESCAPETABLE + C@  R> ADDCHAR
  ELSE
    OVER C@ R> ADDCHAR
  THEN
  1 /STRING
;

: PARSE\"   \ caddr len dest -- caddr' len'
\  Parses a string up to an unescaped '"', translating '\'
\  escapes to characters much as C does. The returned
\  translated string is a counted string at dest
\  The supported escapes (case sensitive) are:
\    \a      BEL (alert)
\    \b      BS (backspace)
\    \e      ESC (escape, ASCII 27)
\    \f      FF (form feed, ASCII 12)
\    \l      LF (ASCII 10)
\    \m      CR/LF pair - for HTML etc.
\    \n      newline 
\    \q      double-quote
\    \r      CR (ASCII 13)
\    \t      HT (tab, ASCII 9)
\    \v      VT
\    \z      NUL (ASCII 0)
\    \"      "
\    \x[0-9a-f][0-9a-f]  Two digit hex numerical character value.
\    \\      backslash itself
\    \       before any other character represents that character
  DUP >R  0 SWAP C!         \ zero destination
  BEGIN                 \ -- caddr len ; R: -- dest
    DUP
   WHILE
    OVER C@ [CHAR] " <>         \ check for terminator
   WHILE
    OVER C@ [CHAR] \ = IF       \ deal with escapes
      1 /STRING R@ ADDESCAPE
    ELSE                \ normal character
      OVER C@ R@ ADDCHAR  1 /STRING
    THEN
  REPEAT THEN
  DUP                   \ step over terminating "
  IF  1 /STRING  THEN
  R> DROP
;

: READESCAPED   \ "string" -- c-addr
\ Parses an escaped string from the input stream according to
\ the rules of PARSE\" above, returning the address
\ of the translated counted string.
  SOURCE >IN @ /STRING TUCK     \ -- len caddr len
  SPAD DUP >R PARSE\"
  NIP - >IN +!  R>   ;

: \",       \ "string" --
\  Parse text up to the closing quote and compile into
\  the dictionary at HERE as a counted string.
\  The end of the string is aligned.
  READESCAPED $,
; IMMEDIATE


: S\"       \ "string" -- caddr u
\ As S" , but translates escaped characters using
\ PARSE\" above.
   STATE @ IF
     READESCAPED COUNT POSTPONE (S") $,
   ELSE
     READESCAPED COUNT
   THEN ; IMMEDIATE

: C\"       \ "string" -- caddr
\ As C" , but translates escaped characters using
\ PARSE\" above
   STATE @ IF
     READESCAPED  POSTPONE (C") $,
   ELSE
     READESCAPED
   THEN ; IMMEDIATE


