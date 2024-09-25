\ forth2012 string wordlist                           jps  0 / 4
.( Loading string definitions... ) CR
FORTH DEFINITIONS
1 2 +THRU












   \ forth2012 string wordlist                     jps  1 / 4
: COMPARE  ( c-addr1 u1 caddr2 u2 -- n )
   ROT 2DUP 2>R  ( c-addr1 u1 caddr2 u2 u1 ; u2 u1 )
   MIN           ( c-addr1 caddr2 u'  ; u2 u1 )
   S=            ( n ; u2 u1 )
   ?DUP 0<> IF  2R> 2DROP                     \ no match
            ELSE  2R>   2DUP  = IF  2DROP 0   \ match
                 \ else which is shorter ?
                 ELSE  <  IF  -1  ELSE  1  THEN  THEN
            THEN    ;






   \ forth2012 string wordlist                     jps  1 / 4
: SEARCH  ( caddr1 u1 caddr2 u2 -- caddr3 u3 flag)
   BEGIN 
      DUP
   WHILE 
     2OVER  3 PICK  OVER  COMPARE
     WHILE
       1 /STRING
     REPEAT
     2NIP  TRUE EXIT
   THEN
   2DROP FALSE ;


