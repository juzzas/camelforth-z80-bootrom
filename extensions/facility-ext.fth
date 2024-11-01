\ Forth 2012 facillity extensions for CamelForth BootROM

.( Loading facility definitions... ) CR

FORTH DEFINITIONS
1 +LOAD










   \ Forth 2012 Facility-ext -- STRUCT
: BEGIN-STRUCTURE                       ( -- addr 0 ; -- size )
   CREATE   HERE 0 0 ,                ( mark stack, lay dummy )
   DOES> @  ;                              ( -- record length )
: +FIELD ( # n ++ #'  define a field with offset # and size n )
   CREATE OVER , +
   DOES> @ + ;  ( addr1 -- addr2 ; calculate address of field )
: FIELD: ALIGNED 1 CELLS +FIELD ;
: CFIELD: 1 CHARS +FIELD ;
: END-STRUCTURE SWAP ! ;
