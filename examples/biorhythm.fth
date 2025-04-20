900 LOAD

\ SIN lookuptable for 0-90 degrees, to 4 implied decimal places
CREATE tab-sin 
     0 ,    175 ,    349 ,    523 ,    698 ,    872 ,   1045 , 
  1219 ,   1392 ,   1564 ,   1736 ,   1908 ,   2079 ,   2250 , 
  2419 ,   2588 ,   2756 ,   2924 ,   3090 ,   3256 ,   3420 , 
  3584 ,   3746 ,   3907 ,   4067 ,   4226 ,   4384 ,   4540 , 
  4695 ,   4848 ,   5000 ,   5150 ,   5299 ,   5446 ,   5592 , 
  5736 ,   5878 ,   6018 ,   6157 ,   6293 ,   6428 ,   6561 , 
  6691 ,   6820 ,   6947 ,   7071 ,   7193 ,   7314 ,   7431 , 
  7547 ,   7660 ,   7771 ,   7880 ,   7986 ,   8090 ,   8192 , 
  8290 ,   8387 ,   8480 ,   8572 ,   8660 ,   8746 ,   8829 , 
  8910 ,   8988 ,   9063 ,   9135 ,   9205 ,   9272 ,   9336 , 
  9397 ,   9455 ,   9511 ,   9563 ,   9613 ,   9659 ,   9703 , 
  9744 ,   9781 ,   9816 ,   9848 ,   9877 ,   9903 ,   9925 , 
  9945 ,   9962 ,   9976 ,   9986 ,   9994 ,   9998 ,  10000 , 

\ sine of 0 - 180 
: (sin180) ( deg -- n ) 
   DUP 90 >  ( 91 - 180 degrees )
   IF 180 SWAP - THEN  ( reflect )
   CELLS tab-sin + @ ;

: SIN ( deg -- n ) \ sine to 4 implied decimal places
   360 MOD
   DUP 0< IF 360 + THEN
   DUP 180 >  IF    180 -  (sin180)   NEGATE 
              ELSE   (sin180)   THEN ;

: COS ( deg -- n ) \ cosine to 4 implied decimal places
   360 MOD 90 + SIN ;

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



: 360mod ( d -- n )
   360. DD/MOD 2DROP D>S ;  

: physical ( u-days -- n% )    \ where n is between -100 and 100
   U>D 360 23 M*/ 360mod SIN 100 / ;

: emotional ( u-days -- n% ) 
   U>D 360 28 M*/ 360mod SIN 100 / ;

: intellectual ( u-days -- n% )
   U>D 360 33 M*/ 360mod SIN 100 / ;



31 10 2014 DAYNUM  VALUE birthdate
26 3 2025 DAYNUM  VALUE today
1 VALUE column

: (to-row) ( convert to row number ) 
    10 +  20 SWAP -  1+ ; 

: MAGENTA  27 EMIT ." [35m"  ;
: YELLOW   27 EMIT ." [33m"  ;
: CYAN     27 EMIT ." [36m"  ;
: NORMAL   27 EMIT ." [0m"  ;

: normalize \ percentage to -10 to 10 ( n -- n )
   DUP 0< IF 5 -  ELSE  5 +  THEN
   10 /  ;

: plot-physical ( col u-days -- row )
   physical normalize  (to-row)
   AT-XY CYAN [CHAR] P EMIT NORMAL ; 

: plot-emotional ( col u-days -- row )
   emotional  normalize  (to-row)
   AT-XY MAGENTA [CHAR] E EMIT NORMAL  ; 

: plot-intellectual ( col u-days -- row )
   intellectual normalize  (to-row)
   AT-XY YELLOW [CHAR] I  EMIT NORMAL ; 

: .y-axis
   11 -10 DO  0  I (to-row) AT-XY [CHAR] | EMIT  LOOP ;

: .x-axis
   1 0 (to-row) AT-XY
   64 0 DO ." +---"   4 +LOOP 

   today  64 0 DO   I 1+   -1 (to-row) AT-XY 
             DUP    NUMDAY 2DROP  .   2 +  4 +LOOP  ;

: .date ( u-days )   NUMDAY  
   -ROT SWAP (.) TYPE [CHAR] - EMIT
             (.) TYPE [CHAR] - EMIT   (.) TYPE ;

: .%  <# DUP ABS 0  [CHAR] % HOLD #S ROT SIGN #>  TYPE ;

: .cycle ( u-days cycle )
   MOD (.) TYPE ;

: .physical ( u-days )
   ." Physical day " DUP 23 .cycle
   ." : "  physical .%   ;

: .emotional ( u-days )
   ." Emotional day "  DUP 28 .cycle
   ." : " emotional .%  ;

: .intellectual ( u-days )
   ." Intellectual day " DUP 33 .cycle
   ." : " intellectual .%  ;

: days today birthdate -  ;

: .today  ." Today: " today  .date ;

: .birthdate  ." Birthdate: " birthdate  .date ;

: INFO 
    CR .today
    CR .birthdate
    CR days .physical  
    CR days .emotional  
    CR days .intellectual  
;

: today! ( dmy )      DAYNUM TO today ;
: birthdate! ( dmy )   DAYNUM TO birthdate ;

: plot
   PAGE
   days    ( today - birthdate )
   64 0 DO
      I  1+   OVER  plot-physical
      I  1+   OVER  plot-emotional
      I  1+   OVER  plot-intellectual
      1+
   2 +LOOP 
   .x-axis  .y-axis
   0 22 AT-XY 
   INFO CR ;
     
CR .( Biorhythms -- instructions: )
CR .( Set today's date with:  dd mm yyyy today! )
CR .( Set birth date with:    dd mm yyyy birthdate! )
CR .( Plot graph with:        plot )

