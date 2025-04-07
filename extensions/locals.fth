CR .( Loading locals.... )
30 LOAD

ONLY FORTH DEFINITIONS  ALSO SYSTEM
1 14 +THRU

ONLY FORTH DEFINITIONS









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

WORDLIST CONSTANT  LOCALS-PRIVATE
LOCALS-PRIVATE >ORDER    LOCALS-PRIVATE  SET-CURRENT

0 VALUE locals-dp
0 VALUE locals#
0 VALUE real-dp
0 VALUE real-current
+USER lvframe  0 lvframe !
lvframe @ U.
: has-locals  locals#  0<> ;

: >tempdict
   GET-CURRENT TO real-current  LOCALS-WID SET-CURRENT
   DP @  TO real-dp   locals-dp DP !  ;

: >realdict
   real-current SET-CURRENT
   DP @  TO locals-dp   real-dp DP !  ;


: ndrop   0 ?DO  DROP LOOP ;


: (dolocals,)  ( nlocals -- )
   ['] LIT  COMPILE,  ,
   ['] RP@  COMPILE,
   ['] lvframe  COMPILE,
   ['] @  COMPILE,
   ['] >R  COMPILE,
   ['] lvframe  COMPILE,
   ['] !  COMPILE,
   ['] N>R  COMPILE, ;

: (endlocals,)
   ['] NR>  COMPILE,
   ['] ndrop  COMPILE,
   ['] R>  COMPILE,
   ['] lvframe  COMPILE,
   ['] !  COMPILE,  ;

: dolocals,
   locals# ?DUP IF (dolocals,)  THEN ;

: endlocals,
   has-locals IF (endlocals,) THEN  ;


: (lv)   ( stk-offset -- addr ) 
   lvframe @ + TO-STATE @ 
   IF !  FALSE TO-STATE ! ELSE @ THEN ;
: lv,   ['] (lv) COMPILE, ;


: LVALUE  ( offset c-addr u -- )
    CR ." LVALUE: "  .S
   LOCALS-WID (CREATE-WID) 2 +  2* NEGATE ,
    CR ." LVALUE end: "  .S
    DOES> ['] LIT COMPILE, @ , lv,  
 ;

: ((LOCAL))
    CR ." ((LOCAL)): "  .S
   >tempdict
    locals# -ROT  LVALUE  IMMEDIATE
   >realdict
    locals# 1+  TO locals#  ;

FORTH-WORDLIST SET-CURRENT

: reset-locals
   0 TO locals#
   0 LOCALS-WID  !
   0 TO locals-dp ;

: /locals
   reset-locals
   HERE 1024 + TO locals-dp
;

: (LOCAL)   ( c-addr u -- )
    CR ." (LOCAL): "  .S
   locals-dp 0= IF
      /locals
   THEN
   DUP IF
      ((LOCAL))
   ELSE
      2DROP
      CR ." locals: " locals# .
      dolocals,
      CR ." after locals: " .S
   THEN   ;


: LOCALS| ( "name...name |" -- )
   BEGIN
      PARSE-NAME OVER C@
      [CHAR] | - OVER 1 - OR WHILE
      (LOCAL)
   REPEAT 2DROP   0 0 (LOCAL)
   ." end locals|"
; IMMEDIATE

LOCALS-PRIVATE  SET-CURRENT


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
     locals# 1+ TO locals#
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
     locals# 1+ TO locals#
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
; IMMEDIATE

CR .( got to this point: before ; )

: ;   \ redefine ; to cope definitions with locals
   ." end define"
   endlocals,
   reset-locals
   POSTPONE ;
;  IMMEDIATE

: EXIT   \ redefine EXIT to cope definitions with locals
   endlocals,
   POSTPONE EXIT
;  IMMEDIATE

: DOES>   \ redefine DOES> to cope definitions with locals
   endlocals,
   reset-locals
   POSTPONE DOES>
;  IMMEDIATE

-1 SET-ORDER   FORTH-WORDLIST SET-CURRENT
CR .( got to LOCTEST point )


: LOCTEST
   LOCALS| a b |
   ." a= "  a U.
   ." b= "  b U.
   10 TO b  ." new b: "   b U.
;

CR .( got to after LOCTEST point )


: TEST   {: a b | xx -- :}
   ." TEST: "
   a . ." + "  b .    a b +  TO  xx  ." = "  xx .
;

CR .( got to after LOCTEST point )
