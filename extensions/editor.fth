\  RC2014 simple editor
ONLY FORTH ALSO UTILS 
VOCABULARY EDITOR    
ALSO EDITOR DEFINITIONS

1 13 +THRU

ONLY FORTH DEFINITIONS
ALSO EDITOR

.( Editor loaded )





\  RC2014 simple editor
: S    ( n -- )   \  select screen n
     DUP SCR ! BLOCK DROP ;
: IA   ( column row -- )  \ insert at column,row
     (LINE) + >R 13 WORD COUNT R> SWAP MOVE UPDATE ;
: Y   ( n -- )      \  yank line into PAD
    (LINE) PAD 1+ C/L MOVE
    C/L PAD !  ;
: P   ( n -- )      \  paste contents of PAD at line n
   PAD COUNT (LINE) SWAP MOVE UPDATE  ;
: I   ( n -- )       \     put text at line n
     0 SWAP IA ;
: E   ( n -- )     \  erase line n
    (LINE) C/L BL FILL UPDATE ;


\  RC2014 simple editor
: B   ( -- )   \ back one screen
     -1 SCR +!
     SCR @ 0 SLICE SLICE>LIMIT @
     WITHIN INVERT IF
        0 SCR !
     THEN ;
: N   ( -- )   \ next screen
     1 SCR +! ;
     SCR @ 0 SLICE SLICE>LIMIT @
     WITHIN INVERT IF
        SLICE SLICE>LIMIT @ 1- SCR !
     THEN ;
: L   ( -- )
     SCR @ LIST ;

\  RC2014 full screen editor   ( 1 / n)
: .BLOCK  ( -- )  \ block status
    ." Screen: " SCR @ DUP . UPDATED? 43 + EMIT SPACE ;
: .RULER  ( -- ) \ print ruler
   3 SPACES   4 0 DO ." +---:---+---:---" LOOP  CR ;
: VB  ( -- )   \ visual list
     .RULER  SCR @ BLOCK (LIST) DROP .RULER ;
: .STACK  ( -- )  \    display stack status
    ." Stack: " .S ;
: V    ( -- ) \      visual list
     PAGE 0 0 AT-XY VB .BLOCK .STACK ;





\  RC2014 Full screen editor =========================

: !XY ( i -- i ) 1023 AND DUP C/L /MOD 3 1 D+ AT-XY ;

: !CH ( c i -- c i ) 2DUP SCR @ BLOCK + C! UPDATE OVER EMIT ;

: 'I    ( i -- c-addr )
    SCR @ BLOCK +  ;








\  RC2014 Full screen editor =========================
: INSERT_CHAR    ( i -- i )
    1023 AND DUP             ( i i )
    'I DUP 1+          ( i c-addr c-addr+1 )
    DUP C/L MOD C/L SWAP -  ( i c-addr c-addr+1 n  )
    MOVE           ( i )
    BL OVER !CH 2DROP   ( i )
    DUP C/L /      ( i l )
    13 EMIT  DUP 2 .R SPACE LL     ( i )
    !XY   ;






\  RC2014 Full screen editor =========================
: DELETE_CHAR  ( i -- i )
    1023 AND DUP             ( i i )
    'I DUP 1+ SWAP         ( i c-addr+1 c-addr )
    OVER C/L MOD C/L SWAP -  ( i c-addr+1 c-addr n  )
    MOVE           ( i )
    BL OVER C/L 1- OR !XY !CH 2DROP   ( i )
    DUP C/L /      ( i l )
    13 EMIT  DUP 2 .R SPACE LL     ( i )
    !XY   ;






\  RC2014 Full screen editor =========================
: DELETE_LINE  ( i -- i )
    1023 AND DUP 65472 ( 0xffc0 ) AND 'I ( i 'i-sol )
    DUP C/L +                ( i 'i-sol 'i-nl )
    SWAP                     ( i 'i-nl 'i-sol )
    OVER 1023 AND B/BLK SWAP -        ( i 'i-nl 'i-sol #n )
    MOVE
    B/BLK C/L - 'I C/L BL FILL  ;   ( i )

: K   ( n -- )     \   cut line into PAD (move lines up)
    DUP Y   (LINE) DELETE_LINE DROP UPDATE  ;





\  RC2014 Full screen editor =========================
: INSERT_LINE  ( i -- i )
    1023 AND DUP 65472 ( 0xffc0 ) AND 'I  ( i 'i-sol )
    DUP >R
    DUP C/L +                ( i 'i-sol 'i-nl )
    OVER 1023 AND B/BLK SWAP -        ( i 'i-nl 'i-sol #n )
    MOVE
    R> C/L BL FILL  ;   ( i )








\  RC2014 Full screen editor =========================
: ?CH ( c i -- c i' )
    OVER BL - 95 U< IF !CH 1+ EXIT THEN           \ text
    OVER 8 = IF 1- THEN                           \ left ^h
    OVER 19 = IF 1- THEN                          \ left ^s
    OVER 4 = IF 1+ THEN                           \ right ^d
    OVER 5 = IF C/L - THEN                        \ up ^e
    OVER 24 = IF C/L + THEN                       \ down ^x
    OVER 13 = IF C/L 2DUP MOD - + THEN            \ crlf return
    OVER 127 = IF 1- THEN                         \ left delete
    OVER 18 = IF B  >R >R  V  R> R>  THEN         \ back ^r
    OVER 3 = IF N  >R >R  V  R> R>  THEN          \ nextscr ^c
    OVER 22 = IF INSERT_CHAR THEN           \ insert space ^v
    OVER 7 = IF DELETE_CHAR THEN            \ delete char ^g
\    OVER 25 = IF CUT_LINE THEN        \ cut line, shift up ^y
    OVER 15 = IF INSERT_LINE THEN \ shift down, empty line ^o
\    OVER 16 = IF PASTE_LINE THEN  \ shift down, paste line ^p
\    OVER 11 = IF UPDATE_SCREEN THEN            \ do UPDATE ^k
    ;


: EDIT ( n -- )    S V  0
    BEGIN !XY KEY SWAP ?CH SWAP 27 = UNTIL DROP V ;
