\ forth2012 tools-ext wordlist                        jps  0 / 4
.( Loading tools-ext definitions... ) CR
FORTH DEFINITIONS
1 5 +THRU












   \ forth2012 tools-ext wordlist                     jps  1 / 4
: COMPARE  ( c-addr1 u1 caddr2 u2 -- n )
   ROT 2DUP 2>R  ( c-addr1 u1 caddr2 u2 u1 ; u2 u1 )
   MIN           ( c-addr1 caddr2 u'  ; u2 u1 )
   S=            ( n ; u2 u1 )
   ?DUP 0<> IF  2R> 2DROP                     \ no match
            ELSE  2R>   2DUP  = IF  2DROP 0   \ match
                 \ else which is shorter ?
                 ELSE  <  IF  -1  ELSE  1  THEN  THEN
            THEN    ;






   \ forth2012 tools-ext wordlist                     jps  2 / 4
: [THEN] ( -- ) ; IMMEDIATE

: [UNDEFINED] BL WORD FIND NIP 0= ; IMMEDIATE

: [DEFINED] BL WORD FIND NIP 0<> ; IMMEDIATE










   \ forth2012 tools-ext wordlist                     jps  3 / 4
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
   \ forth2012 tools-ext wordlist                     jps  4 / 4
: [IF] ( flag -- )
   0= IF POSTPONE [ELSE] THEN  ; IMMEDIATE


