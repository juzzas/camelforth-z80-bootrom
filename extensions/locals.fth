CR .( Loading locals.... )
30 LOAD
1 12 +THRU













\ Execution:
\ ( c-addr u -- )
\ When executed during compilation, (LOCAL) passes a message to
\  the system that has one of two meanings. If u is non-zero,
\  the message identifies a new local whose definition name is
\  given by the string of characters identified by c-addr u. If
\  u is zero, the message is "last local" and c-addr has no
\  significance.

\ local Execution:
\ ( -- x )
\ Push the local's value, x, onto the stack. 

\ TO local Run-time:
\ ( x -- )
\ Assign the value x to the local value local.

WORDLIST CONSTANT  LOCALS-WID
LOCALS-WID >ORDER    LOCALS-WID  SET-CURRENT

0 VALUE locals-dp
0 VALUE locals#
0 VALUE locals-wid
0 VALUE real-dp
0 VALUE real-current
FALSE VALUE has-locals

: >tempdict
   GET-CURRENT TO real-current  locals-wid SET-CURRENT
   DP @  TO real-dp   locals-dp DP !  ;

: >realdict
   real-current SET-CURRENT
   DP @  TO locals-dp   real-dp DP !  ;

: tempdict  ( -- wid )   \ create temp dictionary space with wid
   HERE 1024 +    ( wid )
   0 OVER !
   DUP CELL+   TO locals-dp  ;




: ndrop   0 ?DO  DROP LOOP ;
: dolocals
     locals# IF ['] LIT COMPILE,  locals# ,
     ['] N>R  COMPILE, THEN  ;


: (lv) ( stk-offset -- addr ) 
   TO-STATE @ IF !  FALSE TO-STATE ! ELSE @ THEN ;
: lv, ['] R' COMPILE,  ['] @ COMPILE,   ['] (lv) COMPILE, ;


: LVALUE  ( offset c-addr u -- )
    CR ." LVALUE: "  .S
   locals-wid (CREATE-WID) ,
    CR ." LVALUE end: "  .S
    DOES> ['] LIT COMPILE, @ , lv,  
  \ DOES> @
 ;

: ((LOCAL))
    CR ." ((LOCAL)): "  .S
   >tempdict
    locals# 1+ -ROT  LVALUE  IMMEDIATE
   >realdict
    locals# 1+  TO locals#  ;

FORTH-WORDLIST SET-CURRENT

: (LOCAL)   ( c-addr u -- )
    CR ." (LOCAL): "  .S
   locals-dp 0= IF
      tempdict TO locals-wid
      0 TO locals#
      CR ." created locals-wid: " locals-wid U.
   THEN
   DUP IF
      ((LOCAL))
   ELSE
      2DROP
      CR ." locals: " locals# . 
      dolocals
   THEN   ;


: LOCALS| ( "name...name |" -- )
   BEGIN
      PARSE-NAME OVER C@
      [CHAR] | - OVER 1 - OR WHILE
      (LOCAL)
   REPEAT 2DROP   0 0 (LOCAL)
   CR ." locals to-order:" locals-wid U.
   GET-ORDER CR .S   ndrop
   locals-wid >ORDER
   GET-ORDER CR .S   ndrop
   CR ." after locals to-order:" locals-wid U.
   TRUE TO has-locals
; IMMEDIATE

LOCALS-WID  SET-CURRENT


12345 CONSTANT undefined-value
: match-or-end? ( c-addr1 u1 c-addr2 u2 -- f )
   2 PICK 0= >R   ROT MAX STRCMP 0= R> OR ;

: scan-args
   \ 0 c-addr1 u1 -- c-addr1 u1 ... c-addrn un n c-addrn+1 un+1
   BEGIN
     2DUP S" |" match-or-end? 0= WHILE
     2DUP S" --" match-or-end? 0= WHILE
     2DUP S" :}" match-or-end? 0= WHILE
     ROT 1+ PARSE-NAME
   AGAIN THEN THEN THEN ;

: scan-locals
   \ n c-addr1 u1 -- c-addr1 u1 ... c-addrn un n c-addrn+1 un+1
   2DUP S" |" ROT MAX STRCMP  0= 0= IF
     EXIT
   THEN
   2DROP PARSE-NAME
   BEGIN
     2DUP S" --" match-or-end? 0= WHILE
     2DUP S" :}" match-or-end? 0= WHILE
     ROT 1+ PARSE-NAME
     POSTPONE undefined-value
   AGAIN THEN THEN ;

: scan-end ( c-addr1 u1 -- c-addr2 u2 )
   BEGIN
     2DUP S" :}" match-or-end? 0= WHILE
     2DROP PARSE-NAME
   REPEAT ;

: define-locals ( c-addr1 u1 ... c-addrn un n -- )
   0 ?DO
     (LOCAL)
   LOOP
   0 0 (LOCAL) ;

FORTH-WORDLIST SET-CURRENT

: {: ( -- )
   0 PARSE-NAME
   scan-args scan-locals scan-end
   CR ." parsed: " .S
   2DROP define-locals
   locals-wid >ORDER
   TRUE TO has-locals
; IMMEDIATE

CR .( got to this point: before ; )

: ;   \ redefine ; to cope definitions with locals
   has-locals IF 
      GET-ORDER NIP 1- SET-ORDER
      locals# IF  ['] NR>  COMPILE,  ['] ndrop COMPILE, THEN
   THEN
   0 TO locals#
   0 TO locals-wid
   FALSE TO has-locals
   POSTPONE ;
;  IMMEDIATE

ONLY FORTH DEFINITIONS
CR .( got to LOCTEST point )

: LOCTEST
   LOCALS| a b |
   a .
   b .
;



\ : TEST   {: a b | xx yy zz -- :}
\  ." TEST" 
\  a .
\ ;

