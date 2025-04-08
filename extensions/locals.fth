CR .( Loading locals.... )


ONLY FORTH DEFINITIONS  ALSO SYSTEM
1 15 +THRU

ONLY FORTH DEFINITIONS









\ Locals word set                                    jps 1 / 14

WORDLIST CONSTANT  LOCALS-PRIVATE
LOCALS-PRIVATE >ORDER    LOCALS-PRIVATE  SET-CURRENT

0 VALUE locals-dp
0 VALUE locals#
0 VALUE real-dp
0 VALUE real-current

+USER lvframe  0 lvframe !





\ Locals word set                                    jps 2 / 14
: has-locals  locals#  0<> ;

: >tempdict
   GET-CURRENT TO real-current  LOCALS-WID SET-CURRENT
   DP @  TO real-dp   locals-dp DP !  ;

: >realdict
   real-current SET-CURRENT
   DP @  TO locals-dp   real-dp DP !  ;


: ndrop   0 ?DO  DROP LOOP ;



\ Locals word set                                    jps 3 / 14

: (dolocals,)  ( nlocals -- )
   ['] LIT  COMPILE,  ,     ['] RP@  COMPILE,
   ['] lvframe  COMPILE,    ['] @  COMPILE,
   ['] >R  COMPILE,         ['] lvframe  COMPILE,
   ['] !  COMPILE,          ['] N>R  COMPILE, ;

: (endlocals,)
   ['] NR>  COMPILE,        ['] ndrop  COMPILE,
   ['] R>  COMPILE,         ['] lvframe  COMPILE,
   ['] !  COMPILE,  ;




\ Locals word set                                    jps 4 / 14

: dolocals,
   locals# ?DUP IF (dolocals,)  THEN ;

: endlocals,
   has-locals IF (endlocals,) THEN  ;


: (lv)   ( stk-offset -- addr ) 
   lvframe @ + TO-STATE @ 
   IF !  FALSE TO-STATE ! ELSE @ THEN ;
: lv,   ['] (lv) COMPILE, ;



\ Locals word set                                    jps 5 / 14
: LVALUE  ( offset c-addr u -- )
   LOCALS-WID (CREATE-WID) 2 +  2* NEGATE ,
   DOES> ['] LIT COMPILE, @ , lv,  
 ;

: ((LOCAL))
   >tempdict
    locals# -ROT  LVALUE  IMMEDIATE
   >realdict
    locals# 1+  TO locals#  ;





\ Locals word set                                    jps 6 / 14

: reset-locals
   0 TO locals#
   0 LOCALS-WID  !
   0 TO locals-dp ;

: /locals
   reset-locals
   HERE 1024 + TO locals-dp
;





\ Locals word set                                    jps 7 / 14

FORTH-WORDLIST SET-CURRENT

: (LOCAL)   ( c-addr u -- )
   locals-dp 0= IF  /locals  THEN
   DUP IF  ((LOCAL))  ELSE
      2DROP
      dolocals,
   THEN   ;






\ Locals word set                                    jps 8 / 14

: LOCALS| ( "name...name |" -- )
   BEGIN
      PARSE-NAME OVER C@
      [CHAR] | - OVER 1 - OR WHILE
      (LOCAL)
   REPEAT 2DROP   0 0 (LOCAL)
; IMMEDIATE







\ Locals word set                                    jps 9 / 14
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
\ Locals word set                                   jps 10 / 14
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


\ Locals word set                                   jps 11 / 14
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




\ Locals word set                                   jps 12 / 14
FORTH-WORDLIST SET-CURRENT

: {: ( -- )
   0 PARSE-NAME
   scan-args scan-locals scan-end
   2DROP define-locals
; IMMEDIATE

: ;   \ redefine ; to cope definitions with locals
   endlocals,
   reset-locals
   POSTPONE ;
;  IMMEDIATE


\ Locals word set                                   jps 13 / 14

: EXIT   \ redefine EXIT to cope definitions with locals
   endlocals,
   POSTPONE EXIT
;  IMMEDIATE

: DOES>   \ redefine DOES> to cope definitions with locals
   endlocals,
   reset-locals
   POSTPONE DOES>
;  IMMEDIATE




\ Locals word set                                   jps 14 / 14
-1 SET-ORDER   FORTH-WORDLIST SET-CURRENT


GET-CURRENT    ENVIRONMENT-WORDLIST SET-CURRENT
  TRUE CONSTANT LOCALS
  TRUE CONSTANT LOCALS-EXT
  16 CONSTANT #LOCALS
SET-CURRENT

