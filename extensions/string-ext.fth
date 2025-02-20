\ forth2012 string wordlist                          jps  0 / 9
.( Loading string definitions... ) CR
FORTH DEFINITIONS
1 9 +THRU












   \ forth2012 string wordlist                     jps  1 / 9
256 CHARS CONSTANT string-max

WORDLIST CONSTANT wid-subst
\ Wordlist ID of the wordlist used to hold subst. names and
\ replacement text.










   \ forth2012 string wordlist                     jps  2 / 9
: makeSubst \ c-addr len -- c-addr
 wid-subst (CREATE-WID)  \ like CREATE but takes c-addr/len/wid
 HERE string-max ALLOT 0 OVER C! \ create buffer space
;

: findSubst \ c-addr len -- xt flag | 0
\ Given a name string, find the substitution.
\ Return xt and flag if found, or just zero if not found.
\ Some systems may need to perform case conversion here.
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
     Dest 2@ + C! 1 CHARS Dest +!
   ELSE
     DROP -1 SubstErr !
   THEN
;
   \ forth2012 string wordlist                     jps  5 / 9
: formName \ c-addr len -- c-addr' len'
\ Given a source string pointing at a leading delimiter, place
\ the name string in the name buffer.
   1 /STRING 2DUP delim SCAN >R DROP \ find length of residue
   2DUP R> - DUP >R Name PLACE        \ save name in buffer
   R> 1 CHARS + /STRING        \ step over name and trailing %
;

: >dest \ c-addr len --
\ Add a string to the output string.
   BOUNDS ?DO
     I C@ addDest
   1 CHARS +LOOP
;

   \ forth2012 string wordlist                     jps  6 / 9
: processName \ -- flag
\ Process the last substitution name. Return true if found, 
\                                     0 if not found.
   Name COUNT findSubst DUP >R IF
     EXECUTE COUNT >dest
   ELSE
     delim addDest Name COUNT >dest delim addDest
   THEN
   R>
;





   \ forth2012 string wordlist                     jps  7 / 9
: SUBSTITUTE \ src slen dest dlen -- dest dlen' n
\ Expand the source string using substitutions.
\ Note that this version is simplistic, performs no error
\ checking, and requires a global buffer and global variables.
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
         formName processName IF
           ROT 1+ -ROT                    \ count substitutions
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



