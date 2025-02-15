\ forth2012 string wordlist                          jps  0 / 2
.( Loading string definitions... ) CR
FORTH DEFINITIONS
1 2 +THRU












   \ forth2012 string wordlist                     jps  1 / 2
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




   \ forth2012 string wordlist                     jps  2 / 2
