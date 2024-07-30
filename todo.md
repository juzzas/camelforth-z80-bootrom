# TODO

## core word set

- base prefixes to numbers
- 'c' character symantics
- using fullstop to recognise a double number

## core extensions wordset

VALUE
C"   ( create a counted string )
S\"  ( create an escaped string )
\     --  should modify >IN to end of parsing area.
.(
PARSE
PARSE-NAME
TO
UNUSED    -- report  ( WORKSPACE - HERE )
?DO
2R@
TRUE
FALSE

## core rc2014 extensions wordset

Z"   ( create a nul-terminated C-style string )

## strings words set

SEARCH

## strings extenstions word set

REPLACES
SUBSTITUTE
UNESCAPE

## tools 

[DEFINED]
[ELSE]
[THEN]
[UNDEFINED]

 - definitions at https://forth-standard.org/standard/tools/BracketIF

CS-PICK
CS-ROLL

- manipulate leave stack?

## block wordset

LOAD needs to split lines into seperate parsing areas

## block extensions wordset

## block rc2014 extensions wordset

TLOAD-able files are structured text files on block boundaries.
They must end with ASCII 26, with optional page breaks (ASCII
12) marking variable block markers. Line endings may be 13 or
10.

Based on flexible screens: https://theforth.net/package/screens

TLOAD
TLIST
TINDEX

## facility word set

## facility extensions word set

BEGIN-STRUCTURE
END-STRUCTURE
+FIELD
FIELD
CFIELD

 - based on: https://forth-standard.org/standard/facility/BEGIN-STRUCTURE

## locals word set

- create a temporary dictionary in a buffer below block buffers
- words stack space to store locals? (in case of recursion)


## tools word set

WORDS should list all searchable words

## tools rc2014 extension word set

VLIST should show only the current wordlist

## assembler word set

CODE
;CODE


## behaviour changes

