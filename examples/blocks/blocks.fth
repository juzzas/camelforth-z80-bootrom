HEXLOAD
:10F0000000000000C30AF0C328F0010000ED5B021D
:10F01000F0CB23CB2ACB29CB282A00F0C5D5CD773E
:10F02000F0D1C11CCD77F0C9010000ED5B02F0CB3F
:10F0300023CB2ACB29CB282A00F0C5D5CD94F0D1FB
:10F04000C11CCD94F0C97BD3137AD31479D315782E
:10F05000E60FF6E0D316C9DB17E621C0DB17E6C0E2
:10F06000EE40C257F037C9DB17E621C0DB17E68850
:10F07000EE08C267F037C9CD57F0CD46F03E01D358
:10F08000123E20D317CD57F0CD67F0011000EDB23E
:10F09000EDB237C9CD57F0CD46F03E01D3123E3028
:10F0A000D317CD57F0CD67F0011000EDB3EDB3C32A
:02F0B00057F017
:00000001FF

HEX
F000 CONSTANT &BUFFER
F002 CONSTANT &BLOCK
VARIABLE SCR             0 SCR !
VARIABLE BLK             65535 BLK !
VARIABLE BLK.UPDATE      0 BLK.UPDATE !
VARIABLE BLK.&BUFFER     0 BLK.&BUFFER !
F400 CONSTANT BLK.FIRST

: BLOCK-READ ( --     read block in BLK )
    BLK @ &BLOCK !
    BLK.&BUFFER @ &BUFFER !
    F004 CALL ;

: BLOCK-WRITE ( --     write block in BLK )
    BLK @ &BLOCK !
    BLK.&BUFFER @ &BUFFER !
    F007 CALL ;
DECIMAL

64 CONSTANT C/L

: BUFFER ( n -- addr   push buffer address )
    BLK.FIRST BLK.&BUFFER !
    BLK.UPDATE @ IF BLOCK-WRITE  0 BLK.UPDATE ! THEN
    BLK !
    BLK.&BUFFER @      ( push buffer address ) ;

: BLOCK ( n -- addr    load block )
    DUP BLK @ = IF BUFFER EXIT THEN
    BUFFER BLOCK-READ ;

: UPDATE ( --    update block )
    1 BLK.UPDATE ! ;

: FLUSH ( --    flush block )
    BLK.UPDATE @ IF BLOCK-WRITE  0 BLK.UPDATE ! THEN ;

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
