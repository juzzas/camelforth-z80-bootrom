\ Unit tests for private words in ROM
\
CR .( CamelForth BootROM unit tests  )

3012 LOAD  \ load tester


DECIMAL FORTH DEFINITIONS

1 2 +THRU

CR .( CamelForth BootROM unit tests complete )
CR .( Errors: ) #ERRORS @ . CR



\ S>NUMBER?
TESTING S>NUMBER?

T{ S" 1234" S>NUMBER?   -> 1234 0 TRUE }T
T{ S" kkkk" S>NUMBER?   -> FALSE }T

T{ S" 65535" S>NUMBER?  -> 65535 0 TRUE }T
T{ S" $FFFF" S>NUMBER?  -> 65535 0 TRUE }T

T{ S" -$10" S>NUMBER?  ->  -16 65535 TRUE }T
T{ S" $-10" S>NUMBER?  ->  -16 65535 TRUE }T
T{ S" -5"   S>NUMBER?  ->  -5  65535 TRUE }T
T{ S" %101" S>NUMBER?  ->  5   0 TRUE }T
T{ HEX S" #42" S>NUMBER? DECIMAL ->  42   0 TRUE }T

T{ S" 100000."   S>NUMBER?  -> 34464 1 TRUE }T
T{ S" -100000."  S>NUMBER?  -> 31072 65534  TRUE }T

T{ S" "     S>NUMBER?  ->  FALSE }T
T{ S" $"    S>NUMBER?  ->  FALSE }T
T{ S" 12x"  S>NUMBER?  ->  FALSE }T
T{ S" 1.5"  S>NUMBER?  ->  FALSE }T

