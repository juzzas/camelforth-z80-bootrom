\ Calendar demo
\ date calculation. ref : Forth Dimensions XV/1.
( from date to daynum. NOTE: only good for 1900-2079 )

( usage: 25 12 92 DAYNUM WEEKDAY . )
( 25 12 92 DAYNUM NUMDAY . . . )
( to print a small calender type d m y .CAL )









  \ calendar DAYNUM WEEKDAY              ( 1 / n )
: UM/  UM/MOD NIP ;
: UMOD SWAP 0 ROT UM/MOD DROP ;

: PREPY  ( y-z )    DUP 1900 > IF 1900 - THEN ;
: PREPM  ( m-n )    DUP 3 < IF 12 + THEN 3 - ;
: PREPD ( dmy-dnz ) PREPY SWAP PREPM SWAP OVER 9 > IF 1- THEN ;
: YDAYS ( y-z )     DUP 365 * SWAP 25 * 100 / + ;
: MDAYS ( m-n )     DUP IF DUP 304 * 10 / SWAP DUP 11 /
                    SWAP 6 / + + 1+ THEN ;

: DAYNUM ( dmy-g )  PREPD YDAYS SWAP MDAYS + + ;
: WEEKDAY ( g-n )   5 - 7 UMOD ; ( O=Mon, l=Tue, etc. )



  \ calendar NUMDAY                      ( 2 / n )
: N2Y ( g-n )      4 UM* 1461 UM/ ( ie. / 365.25 ) ;
: D2M ( n-m )      DUP 184 / OVER 337 / + 1+ - 100 3041 */ ;
: YADJ ( yn-ym )   DUP 0= IF DROP 1- 366 THEN ;
: MADJ ( n-m )      3 + DUP 12 > IF 12 - THEN ;
: NUMDAY ( g-dmy )  DUP N2Y DUP YDAYS ROT SWAP - YADJ
   DUP D2M DUP MADJ SWAP MDAYS ROT SWAP -
   SWAP ROT OVER 3 < IF 1+ THEN 1900 + ;








  \ calendar  -- output subroutines       ( 3 / n )
7 VALUE #CELLS  6 VALUE #ROWS

VARIABLE TODAY
VARIABLE MONTH
VARIABLE YEAR

: WEEKSTART  ( g -- g' ) 
   DUP WEEKDAY -  ;

: ?REVERSE  ( f -- ) IF 27 EMIT ." [7m" THEN  ; 
: BRIGHT   27 EMIT ." [1m"  ;
: NORMAL   27 EMIT ." [0m"  ;



  \ calendar  -- output subroutines       ( 4 / n )
: .MMYY ( g -- )
   BRIGHT    NUMDAY SWAP CASE
       1 OF ." January " ENDOF      2 OF ." February " ENDOF
       3 OF ." March " ENDOF        4 OF ." April " ENDOF
       5 OF ." May " ENDOF          6 OF ." June " ENDOF
       7 OF ." July " ENDOF         8 OF ." August " ENDOF
       9 OF ." September " ENDOF    10 OF ." October " ENDOF
      11 OF ." November " ENDOF     12 OF ." Decemper " ENDOF
      DROP
   ENDCASE  4 .R  NORMAL   DROP ;

: .HEADER ( g - )
   CR .MMYY
   CR ." Mon Tue Wed Thu Fri Sat Sun" ;

  \ calendar  -- output subroutines       ( 5 / n )
: (.DAY)  ( n f -- )   ?REVERSE  3 .R   NORMAL  ;
: .DAY ( g -- ) 
   NUMDAY DROP     ( d m ) 
   MONTH @ = IF  DUP TODAY @ = (.DAY)
             ELSE  DROP  3 SPACES  THEN  ;
: .ROW ( g -- g' )
   CR #CELLS 0 ?DO
      DUP .DAY   SPACE 1+
   LOOP ;
: .ROWS ( g n -- g' ) 0 ?DO .ROW LOOP ;
: .PAGE ( g -- )   #ROWS .ROWS CR ;

  \ calendar -- .CAL                     ( 6 / n )
: .CAL ( d m y -- g )
     ROT  TODAY !  1  -ROT   \ first of month
     OVER MONTH !
     DUP YEAR !
     DAYNUM 
     DUP .HEADER
     WEEKSTART .PAGE ;

