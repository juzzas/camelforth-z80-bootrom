\ Display the word lists in the search order in their search
\ order sequence, from first searched to last searched. Also
\ display the word list into which new definitions will be
\ placed.

: ORDER
    GET-ORDER 0 ?DO U. LOOP 
    ." (CURRENT: " GET-CURRENT 1 U.R ." )" CR   ;
