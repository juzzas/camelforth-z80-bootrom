: /STRUCT 0 ;  ( start a new structure )
: FIELD ( # n ++ #'  define a field with offset # and size n )
    CREATE OVER , +
    DOES> @ + ;  ( addr1 -- addr2 ; calculate address of field )
: STRUCT CONSTANT ;
