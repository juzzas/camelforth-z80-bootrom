
HEX
F400 CONSTANT BLKFIRST

DECIMAL
64 CONSTANT C/L

: BUFFER ( n -- addr   push buffer address )
    BLKFIRST BLKBUFFER !
    BLKUPDATE @ IF BLOCK-WRITE  0 BLKUPDATE ! THEN
    BLK !
    BLKBUFFER @      ( push buffer address ) ;

: BLOCK ( n -- addr    load block )
    DUP BLK @ = IF BUFFER EXIT THEN
    BUFFER BLOCK-READ ;

: UPDATE ( --    update block )
    1 BLKUPDATE ! ;

: FLUSH ( --    flush block )
    BLKUPDATE @ IF BLOCK-WRITE  0 BLKUPDATE ! THEN ;

: DUMP ( addr n --    dump memory )
    0 DO DUP I + C@ . LOOP ;



: (.)  ( n -- addr c   convert value to string )
    S>D <# #S #> ;

: ESC  ( -- emit escape character )
    27 EMIT ;

: CLS  ( -- clear screen )
    ESC ." [2J" ;

: RESET  ( -- reset attributes )
    ESC ." [0m" ;

: AT-XY  ( x y -- move cursor to x,y )
    ESC ." [" 1+ (.) TYPE ." ;" 1+ (.) TYPE ." H" ;

: \ 13 WORD DROP ; IMMEDIATE

: LL ( line# -- ( List Line )
DUP C/L * SCR @ BLOCK + C/L TYPE S>D <# # # #> TYPE ;

: LIST ( n -- ) DUP SCR ! . ." scr" 0
BEGIN CR DUP LL 1+ DUP 16 = UNTIL DROP ;

( miniEDIT extensions ) DECIMAL
: RE ( -- n ) SCR @ ; : L ( -- ) 0 DUP AT-XY RE LIST ;
: B ( -- ) -1 SCR +! ; : N ( -- ) 1 SCR +! ;
: mx1 ( c i -- c i ( miniEDIT extension 1 )
OVER 127 = IF 1- THEN ( left delete )
OVER 2 = IF B L THEN ( back ^b )
OVER 14 = IF N L THEN ( next ^n ) ;

( miniEDIT full screen overwrite mode ) DECIMAL
: !XY ( i -- i ) 1023 AND DUP C/L /MOD 1+ AT-XY ;
: !CH ( c i -- c i ) 2DUP SCR @ BLOCK + C! UPDATE OVER EMIT ;
: ?CH ( c i -- c i' ( VIM like controls )
OVER BL - 95 U< IF !CH 1+ EXIT THEN ( text )
OVER 8 = IF 1- THEN ( left ^h )
OVER 12 = IF 1+ THEN ( right ^l )
OVER 11 = IF C/L - THEN ( up ^k )
OVER 10 = IF C/L + THEN ( down ^j )
OVER 13 = IF C/L 2DUP MOD - + THEN ( crlf return )
mx1 ( mx2 ( mx3 ( mx4 ( mx5 ( mx6 ) ;
: EDIT ( n -- ) CLS 0 DUP AT-XY LIST 0
BEGIN !XY KEY SWAP ?CH SWAP 27 = UNTIL DROP L ;
\ ==============================================================
