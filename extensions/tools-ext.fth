\ forth2012 tools-ext wordlist                        jps  0 / 4
CR .( Loading tools-ext definitions... )
FORTH DEFINITIONS
1 5 +THRU












   \ CamelForth tools -- conditional compile           jps  1 / 5
: [THEN] ( -- ) ; IMMEDIATE

: [UNDEFINED] BL WORD FIND NIP 0= ; IMMEDIATE

: [DEFINED] BL WORD FIND NIP 0<> ; IMMEDIATE










   \ forth2012 tools-ext wordlist                     jps  2 / 5
: [ELSE] ( -- )
   1 BEGIN                                       \ level
     BEGIN BL WORD COUNT DUP WHILE               \ level adr len
       2DUP S" [IF]" COMPARE 0= IF               \ level adr len
           2DROP 1+                              \ level'
        ELSE                                     \ level adr len
          2DUP S" [ELSE]" COMPARE 0= IF          \ level adr len
              2DROP 1- DUP IF 1+ THEN            \ level'
          ELSE                                   \ level adr len
              S" [THEN]" COMPARE 0= IF           \ level
                 1-                              \ level'
              THEN  THEN
        THEN ?DUP 0= IF EXIT THEN                \ level'
     REPEAT 2DROP                                \ level
   REFILL 0= UNTIL        DROP    ; IMMEDIATE
   \ forth2012 tools-ext wordlist                     jps  3 / 5
: [IF] ( flag -- )
   0= IF POSTPONE [ELSE] THEN  ; IMMEDIATE

: NAME>COMPILE  ( nt -- x xt )
   DUP NFA>CFA SWAP  ( cfa nt )
   IMMED?  ( cfa f )
     IF ['] EXECUTE ELSE ['] COMPILE, THEN ;

: NAME>INTERPRET ( nt -- xt | 0 )  NFA>CFA  ;

: NAME>STRING ( nt -- c-addr u )
   COUNT $7F AND ;



   \ forth2012 tools-ext wordlist                     jps  4 / 5
: SYNONYM ( "<spaces>newname" "<spaces>oldname" -- ) 
   CREATE IMMEDIATE
      HIDE ' , REVEAL
   DOES>
      @ STATE @ 0= OVER IMMED? OR
      IF EXECUTE ELSE COMPILE, THEN ;

: CS-PICK  PICK ;
: CS-ROLL  ROLL ;






   \ forth2012 tools-ext wordlist                     jps  5 / 5
: TRAVERSE-WORDLIST  ( i*x xt wid -- j*x )
\ The invoked xt has the stack effect ( k*x nt -- l*x flag ).
   WID>NFA DUP 0= IF 2DROP EXIT THEN
   BEGIN                      ( xt nfa )
       2DUP 2>R
       SWAP EXECUTE           ( f ; r:  nfa xt )
       2R> ROT                ( xt nfa f )
       IF    NFA>LFA @ DUP    ( xt nfa' nfa' )
       ELSE  ( xt nfa )   2DROP EXIT
       THEN
   0= UNTIL                   ( xt nfa'|0 )
   2DROP ;

