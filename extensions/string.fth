\ forth2012 string wordlist                          jps  0 / 2
.( Loading string definitions... ) CR
FORTH DEFINITIONS
1 3 +THRU












   \ forth2012 string wordlist -- COMPARE          jps  1 / 2
: COMPARE   ( caddr1 u1 caddr2 u2 -- flag )
  ROT SWAP                      \ c-addr1 c-addr2 u1 u2
  2DUP - >R MIN                 \ c-addr1 c-addr2 minlen --
                                \    R: lendiff?
  STRCMP                        \ f -- R: lendiff

  DUP IF R> DROP >R ELSE DROP THEN    \ replace lendiff 
                                      \ with errorcode
  R> DUP IF 0< 1 OR THEN     \ make flag, 0 becomes 0, 
                                \    -ve becomes -1
  ;                             \    and  +ve becomes 1



   \ forth2012 string wordlist -- SEARCH           jps  2 / 2
: SEARCH  ( caddr1 u1 caddr2 u2 -- caddr3 u3 flag )
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




   \ forth2012 string wordlist -- -TRAILING        jps  3 / 3
: -TRAILING     \ c-addr u1 -- c-addr u2
    DUP 0 ?DO
        2DUP + 1 - C@  DUP BL <> SWAP $09 <> AND IF LEAVE THEN
        1-
    LOOP ;

\ If u is greater than zero, store the character value for
\ space in u consecutive character positions beginning at
\ c-addr.
: BLANK  ( c-addr u -- )   BL FILL ;
