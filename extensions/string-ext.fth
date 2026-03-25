\ forth2012 string wordlist                          jps  0 / 9
CR .( Loading string-ext definitions... )
FORTH DEFINITIONS
1 10 +THRU












   \ forth2012 string wordlist                     jps  1 / 9
256 CHARS CONSTANT string-max

WORDLIST CONSTANT wid-subst
\ Wordlist ID of the wordlist used to hold subst. names and
\ replacement text.

CREATE toupperbuf 32 ALLOT
: toupper    ( c1 -- c2 = Convert the char. to upper case )
  DUP [CHAR] a  [ CHAR z 1+ ] LITERAL  WITHIN IF
    [ CHAR a CHAR A - ] LITERAL -
  THEN
;

: upper ( addr len -- ) BOUNDS DO I C@ toupper I C! LOOP ;

   \ forth2012 string wordlist                     jps  2 / 9
: makeSubst \ c-addr len -- c-addr
   TUCK toupperbuf SWAP MOVE
   toupperbuf SWAP   2DUP upper
   wid-subst (CREATE-WID)
   HERE string-max ALLOT 0 OVER C! \ create buffer space
;

: findSubst \ c-addr len -- xt flag | 0
\ Given a name string, find the substitution.
\ Return xt and flag if found, or just zero if not found.
   TUCK toupperbuf SWAP MOVE
   toupperbuf SWAP   2DUP upper
   wid-subst SEARCH-WORDLIST
;

   \ forth2012 string wordlist                     jps  3 / 9
: REPLACES \ text tlen name nlen --
\ Define the string text/tlen as the text to substitute for
\ the substitution named name/nlen.
\ If the substitution does not exist it is created.
   2DUP findSubst IF
     NIP NIP EXECUTE    \ get buffer address
   ELSE
     makeSubst
   THEN
   PLACE                  \ copy as counted string
;




   \ forth2012 string wordlist                     jps  4 / 9
CHAR % CONSTANT delim     \ Character used as the subst. delim.
string-max BUFFER: Name   \ Holds subst. name as a counted str.
VARIABLE DestLen          \ Max. length of the dest buffer.
2VARIABLE Dest            \ Holds dest. str. current length and
                          \ address.
VARIABLE SubstErr         \ Holds zero or an error code.

: addDest \ char --
\ Add the character to the destination string.
   Dest @ DestLen @ < IF
     SubstErr @ 0= IF
       Dest 2@ + C! 1 CHARS Dest +!
     ELSE  DROP  THEN
   ELSE  DROP -1 SubstErr !  THEN
;
   \ forth2012 string wordlist                     jps  5 / 9
: formName \ c-addr len -- c-addr' len' name nlen
\ Given a source string pointing at a leading delimiter, place
\ the name string in the name buffer.
   1 /STRING 2DUP delim SCAN DUP IF
     >R DROP    \ use length of residue
     2DUP R> - TUCK      ( c-addr' len' nlen name nlen )
     2>R 1+ /STRING  2R>      \ step over name and trailing %
   THEN ;

: >dest \ c-addr len --
\ Add a string to the output string.
   BOUNDS ?DO
     I C@ addDest
   1 CHARS +LOOP
;
   \ forth2012 string wordlist                     jps  6 / 9
: processName \ caddr len -- flag
\ Process the last substitution name. Return true if found, 
\                                     0 if not found.
   2DUP findSubst IF
     NIP NIP EXECUTE COUNT >dest TRUE
   ELSE
     delim addDest >dest delim addDest FALSE
   THEN
;






   \ forth2012 string wordlist                     jps  7 / 9
: not-overlapped?    \ caddr1 len1 caddr2 len2 -- f
\ *G Return true if the two strings do not overlap.
  2OVER +  2 PICK U<   \ caddr1+len1 < caddr2
  IF  2DROP 2DROP TRUE  EXIT  THEN
  + ROT U<    \ caddr2+len2 < caddr1
  NIP
;
: overlapped?  not-overlapped? 0= ;

: SUBSTITUTE \ src slen dest dlen -- dest dlen' n
\ Expand the source string using substitutions.
\ Note that this version is simplistic, performs no error
\ checking, and requires a global buffer and global variables.
  2OVER 2OVER overlapped? IF
    DROP NIP NIP  0 -78 EXIT
  THEN

   DestLen ! 0 Dest 2! 0 -ROT \ -- 0 src slen
   0 SubstErr !
   BEGIN
     DUP 0 >
   WHILE
     OVER C@ delim <> IF                \ character not %
       OVER C@ addDest 1 /STRING
     ELSE
       OVER 1 CHARS + C@ delim = IF    \ %% for one output %
         delim addDest 2 /STRING       \ add one % to output
       ELSE
   \ forth2012 string wordlist                     jps  8 / 9
         formName DUP IF 
           processName IF
           ROT 1+ -ROT THEN               \ count substitutions
         ELSE 2DROP delim addDest
         THEN
       THEN
     THEN
   REPEAT
   2DROP Dest 2@ ROT SubstErr @ IF
     DROP SubstErr @
   THEN
;



   \ forth2012 string wordlist                     jps  9 / 9
: UNESCAPE \ c-addr1 len1 c-addr2 -- c-addr2 len2
\ Replace each '%' character in the input string c-addr1 len1
\ with two '%' characters. The output is represented by
\ c-addr2 len2. If you pass a string through UNESCAPE and
\ then SUBSTITUTE, you get the original string.
   DUP 2SWAP OVER + SWAP ?DO
     I C@ [CHAR] % = IF
       [CHAR] % OVER C! 1+
     THEN
     I C@ OVER C! 1+
   LOOP
   OVER -
;



