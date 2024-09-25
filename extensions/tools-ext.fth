\ forth2012 tools-ext wordlist                        jps  0 / 4
.( Loading tools-ext definitions... ) CR
FORTH DEFINITIONS
1 3 +THRU












   \ forth2012 tools-ext wordlist                     jps  1 / 3
: [THEN] ( -- ) ; IMMEDIATE

: [UNDEFINED] BL WORD FIND NIP 0= ; IMMEDIATE

: [DEFINED] BL WORD FIND NIP 0<> ; IMMEDIATE










   \ forth2012 tools-ext wordlist                     jps  2 / 3
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
   \ forth2012 tools-ext wordlist                     jps  3 / 3
: [IF] ( flag -- )
   0= IF POSTPONE [ELSE] THEN  ; IMMEDIATE


