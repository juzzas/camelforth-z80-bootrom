\ forth2012 tools-ext wordlist                        jps  0 / 4
CR .( Loading tools-ext definitions... )
FORTH DEFINITIONS
1 5 +THRU












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

