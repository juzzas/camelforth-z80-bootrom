; RC2014 Extension Words - 16K
;
; ===============================================
; CamelForth for the Zilog Z80
; Copyright (c) 1994,1995 Bradford J. Rodriguez
; Copyright (c) 2020 Justin Skists
;
; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 3 of the License, or
; (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program.  If not, see <http://www.gnu.org/licenses/>.

; Commercial inquiries should be directed to the author at 
; 115 First St., #105, Collingwood, Ontario L9Y 4W3 Canada
; or via email to bj@camelforth.com
;
; ===============================================
; RC2014 Extension Words
; 
;   Forth words are documented as follows:
;*   NAME     stack -- stack    description
;   Word names in upper case are from the ANS
;   Forth Core word set.  Names in lower case are
;   "internal" implementation words & extensions.
; ===============================================


SECTION code_user_16k

; RC2014 EXTENSION CONSTANTS ====================

;C C/L      -- n         columns per line
    head(C_L,C/L,docon)
        dw 64

;C L/B      -- n         lines per block
    head(L_B,L/B,docon)
        dw 16

;C B/BLK      -- n       bytes per block
    head(B_BLK,B/BLK,docon)
        dw 1024


; RC2014 EXTENSION (plan for 8K) ================

; 8 CONSTANT #COLOURS
    head(NCOLOURS,``#COLOURS'',docon)
        dw 8

;: INK  ( n -- change fg to n 0-7 )
;    DUP 0 #COLOURS WITHIN IF
;        VT-ESC
;        30 + (.) TYPE ." m"
;    THEN ;
    head(INK,INK,docolon)
        dw DUP,lit,0,NCOLOURS,WITHIN,qbranch,INK1
        dw VT_ESC
        dw lit,30,PLUS,XDOT,TYPE
        dw lit,'m',EMIT
INK1:
        dw EXIT

;: BRIGHT.INK  ( n -- change fg to n 0-7 )
;    DUP 0 #COLOURS WITHIN IF
;        VT-ESC
;        90 + (.) TYPE ." m"
;    THEN ;
    head(BRIGHTINK,BRIGHT.INK,docolon)
        dw DUP,lit,0,NCOLOURS,WITHIN,qbranch,BRINK1
        dw VT_ESC
        dw lit,90,PLUS,XDOT,TYPE
        dw lit,'m',EMIT
BRINK1:
        dw EXIT

;: PAPER  ( n -- change bg to n 0-7 )
;    DUP 0 #COLOURS WITHIN IF
;        VT-ESC
;        40 + (.) TYPE ." m"
;    THEN ;
    head(PAPER,PAPER,docolon)
        dw DUP,lit,0,NCOLOURS,WITHIN,qbranch,PAPER1
        dw VT_ESC
        dw lit,40,PLUS,XDOT,TYPE
        dw lit,'m',EMIT
PAPER1:
        dw EXIT

;: BRIGHT.PAPER  ( n -- change bg to n 0-7 )
;    DUP 0 #COLOURS WITHIN IF
;        VT-ESC
;        100 + (.) TYPE ." m"
;    THEN ;
    head(BRIGHTPAPER,BRIGHT.PAPER,docolon)
        dw DUP,lit,0,NCOLOURS,WITHIN,qbranch,BRPAPER1
        dw VT_ESC
        dw lit,100,PLUS,XDOT,TYPE
        dw lit,'m',EMIT
BRPAPER1:
        dw EXIT

;Z REVERSE  ( -- reverse attributes )
;    VT-ESC ." 7m" ;
    head(REVERSE,REVERSE,docolon)
        dw VT_ESC, XSQUOTE
        db 2,"7m"
        dw TYPE
        dw EXIT

; DEBUG LED implementation ========================

;Z /LED ( -- )                       initialise LED
;      0 led_state C!
;      0 0 PC!   ;
    head(SLASHLED,/LED,docolon)
        DW lit,0,lit,led_state,CSTORE
        DW lit,0,lit,0,PCSTORE
        DW EXIT

;Z @LED ( -- c )                    fetch LED value
;      led_state C@ ;
    head(LEDFETCH,@LED,docolon)
        DW led_state,CFETCH
        DW EXIT

;Z !LED ( -- c )                    store LED value
;      DUP led_state C! 0 PC! ;
    head(LEDSTORE,!LED,docolon)
        DW DUP,lit,led_state,CSTORE
        DW lit,0,PCSTORE
        DW EXIT

;Z +LED ( n -- )                       enable LED n
;       DUP 0 8 WITHIN IF
;           1 SWAP LSHIFT LED@ OR LED!
;       ELSE
;           DROP
;       THEN ;
    head(PLUSLED,+LED,docolon)
        DW DUP,lit,0,lit,8,WITHIN,qbranch,PLUSLED1
        DW lit,1,SWOP,LSHIFT,LEDFETCH,OR,LEDSTORE
        DW EXIT
PLUSLED1:
        DW DROP,EXIT

;Z -LED ( n -- )                      disable LED n
;       DUP 0 8 WITHIN IF
;           1 SWAP LSHIFT INVERT LED@ AND LED!
;       ELSE
;           DROP
;       THEN ;
    head(MINUSLED,-LED,docolon)
        DW DUP,lit,0,lit,8,WITHIN,qbranch,MINUSLED1
        DW lit,1,SWOP,LSHIFT,INVERT,LEDFETCH,AND,LEDSTORE
        DW EXIT
MINUSLED1:
        DW DROP,EXIT

SECTION data_user

led_state:
        DEFB 0

SECTION code_user_16k


; RC2014 EXTENSION (MISC) =======================

;C ERASE       ( a-addr u --   fill with 0's )
;    0 FILL
;    ;
    head(ERASE,ERASE,docolon)
        dw lit,0,FILL
        dw EXIT

;C BLANKS       ( a-addr u --   fill with spaces )
;    BL FILL
;    ;
    head(BLANKS,BLANKS,docolon)
        dw lit,32,FILL
        dw EXIT

;Z (XORSHIFT) ( n -- n   xorshift random number generator )
;    DUP 7 LSHIFT XOR
;    DUP 9 RSHIFT XOR
;    DUP 8 LSHIFT XOR ;
    head(XXORSHIFT,(XORSHIFT),docolon)
        DW DUP,lit,7,LSHIFT,XOR
        DW DUP,lit,9,RSHIFT,XOR
        DW DUP,lit,8,LSHIFT,XOR
        DW EXIT

;: RND  ( -- n   generate random 16bit value from seed )
;    SEED @
;    (XORSHIFT)
;    DUP SEED ! ;
    head(RND,RND,docolon)
        DW SEED,FETCH
        DW XXORSHIFT
        DW DUP,SEED,STORE
        DW EXIT

;: RANDOM (  n -- n  generate random value between 0 and value on stack )
;    ( WARNING: Not evenly distributed but should be good )
;    RND SWAP MOD ABS ;
    head(RANDOM,RANDOM,docolon)
        DW RND,SWOP,MOD,ABS
        DW EXIT

;C NOOP        ( -- )        no operation
;    ;
    head(NOOP,NOOP,docolon)
        dw EXIT

;C DEFER    ( "name" -- )        create a deferred word
;    CREATE ['] NOOP ,
;   DOES>
;    @ EXECUTE ;
    head(DEFER,DEFER,docolon)
        DW CREATE,lit,NOOP,COMMA

        DW XDOES
        call dodoes
        DW FETCH,EXECUTE
        dw EXIT

;C DEFER!   ( xt2 xt1 -- )             store xt2 in xt1
;   >BODY ! ;
    head(DEFERSTOR,DEFER!,docolon)
        DW TOBODY,STORE
        DW EXIT

;C DEFER@   ( xt2 xt1 -- )             store xt2 in xt1
;   >BODY ! ;
    head(DEFERFETCH,DEFER@,docolon)
        DW TOBODY,FETCH
        DW EXIT

;C IS       ( xt "name" -- )     define a deferred word
;   STATE @ IF
;      POSTPONE ['] POSTPONE DEFER!
;   ELSE
;      ' DEFER!
;   THEN ; IMMEDIATE
    immed(IS,IS,docolon)
        DW STATE,FETCH,qbranch,IS1
        DW BRACTICK,lit,DEFERSTOR,COMMAXT
        DW branch,IS2
IS1:
        DW TICK,DEFERSTOR
IS2:
        DW EXIT

;C ACTION-OF  ( "name -- xt" )     get the action of a deferred word
;   STATE @ IF
;      POSTPONE ['] POSTPONE DEFER@
;   ELSE
;      ' DEFER@
;   THEN ; IMMEDIATe
    immed(ACTION_OF,ACTION-OF,docolon)
        DW STATE,FETCH,qbranch,AOF1
        DW BRACTICK,lit,DEFERFETCH,COMMAXT
        DW branch,AOF2
AOF1:
        DW TICK,DEFERFETCH
AOF2:
        DW EXIT


; RC2014 EXTENSIONS (TERMINAL) ==================

;Z D.R                       ( d width -- right align )
;    >R (D.)
;    R> OVER - SPACES TYPE ;
    head(DDOTR,D.R,docolon)
        dw TOR,XDDOT
        dw RFROM,OVER,MINUS,SPACES,TYPE
        dw EXIT

;Z .R                ( n width -- right align )
;    >R S>D R> D.R ;
    head(DOTR,.R,docolon)
        dw TOR,STOD,RFROM,DDOTR
        dw EXIT

;Z (D.W)         ( d width --   width with leading 0's )
;    1- DUP 1 < IF DROP 1 THEN <# 0 DO # LOOP #S #> ;
    head(XDDOTW,(D.W),docolon)
        dw ONEMINUS,DUP,lit,1,LESS,qbranch,XDDOTW1
        dw DROP,lit,1
XDDOTW1:
        dw LESSNUM,lit,0,xdo
XDDOTW2:
        dw NUM,xloop,XDDOTW2
        dw NUMS,NUMGREATER
        dw EXIT

;Z (.W)         ( n width --   width with leading 0's )
;    >R S>D R> (D.W) ;
    head(XDOTW,(.W),docolon)
        dw TOR,STOD,RFROM,XDDOTW
        dw EXIT

;Z .W           ( n width --   width with leading 0's )
;    (.W) TYPE SPACE ;
    head(DOTW,.W,docolon)
        dw XDOTW,TYPE,SPACE
        dw EXIT

;Z (U.W)        ( u width --   width with leading 0's )
;    0 SWAP (D.W) ;
    head(XUDOTW,(U.W),docolon)
        dw lit,0,SWOP,XDDOTW
        dw EXIT

;Z U.W         ( u width --   width with leading 0's )
;    (U.W) TYPE SPACE ;
    head(UDOTW,U.W,docolon)
        dw XUDOTW,TYPE,SPACE
        dw EXIT

;Z PRINTABLE?         ( n - flag  is characte printable? )
;    20 7F WITHIN ;
    head(PRINTABLEQ,PRINTABLE?,docolon)
        dw lit,0x20,lit,0x7f,WITHIN
        dw EXIT

; BLOCK implementation ==========================

;Z BLKFIRST      -- a-adrs      address of first block buffer
    head(BLKFIRST,BLKFIRST,docon)
        dw 0x8800

;Z BLOCK-READ  ( -- f )   Compact Flash read block  BLK and DSK
; Reads the block from the Compact Flash card into memory
; address found at 'adrs'. 'dks' and 'blk' are the disk
; and block numbers respectively
    head(BLOCK_READ,BLOCK-READ,docolon)
        dw DSK,FETCH,BLK,FETCH,BLKBUFFER,FETCH
        dw lit,cflash_read_block,CALL
        dw INVERT,XSQUOTE
        db 10,"DISK ERROR"
        dw QABORT,EXIT

;Z BLOCK-WRITE  ( -- f )  Compact Flash read write BLK and DSK
; Reads the block from the Compact Flash card into memory
; address found at 'adrs'. 'dks' and 'blk' are the disk
; and block numbers respectively
    head(BLOCK_WRITE,BLOCK-WRITE,docolon)
        dw DSK,FETCH,BLK,FETCH,BLKBUFFER,FETCH
        dw lit,cflash_write_block,CALL
        dw INVERT,XSQUOTE
        db 10,"DISK ERROR"
        dw QABORT,EXIT

;C BUFFER        n -- addr         push buffer address
;     FLUSH
;     BLK !
;     BLKFIRST BLKBUFFER !
;     BLKBUFFER @      ( push buffer address ) ;
    head(BUFFER,BUFFER,docolon)
        dw FLUSH
        dw BLK,STORE
        dw BLKFIRST,BLKBUFFER,STORE
        dw BLKBUFFER,FETCH
        dw EXIT

;C BLOCK                    n -- addr    load block
;     DUP BLK @ = IF
;       DROP BLKBUFFER @
;     ELSE
;       BUFFER BLOCK-READ
;     THEN ;
    head(BLOCK,BLOCK,docolon)
        dw DUP,BLK,FETCH,EQUAL,qbranch,BLOCK1
        dw DROP,BLKBUFFER,FETCH
        dw branch,BLOCK2
BLOCK1:
        dw BUFFER,BLOCK_READ
BLOCK2:
        dw EXIT

;C UPDATE                    --    mark block to update
;     -1 BLKUPDATE ! ;
    head(UPDATE,UPDATE,docolon)
        dw lit,0xffff,BLKUPDATE,STORE
        dw EXIT

;C UPDATED?                 n -- f   is block updated?
;     BLK @ = IF
;         BLKUPDATE FETCH
;     ELSE 0 THEN ;
    head(UPDATEDQ,UPDATED?,docolon)
        dw BLK,FETCH,EQUAL,qbranch,UPDATEDQ1
        dw BLKUPDATE,FETCH,branch,UPDATEDQ2
UPDATEDQ1:
        dw lit,0
UPDATEDQ2:
        dw EXIT

;C FLUSH                    --    flush blocks to disk
;     BLK @ UPDATED? IF BLOCK-WRITE  0 BLKUPDATE ! THEN ;
    head(FLUSH,FLUSH,docolon)
        dw BLK,FETCH,UPDATEDQ,qbranch,FLUSH1
        dw BLOCK_WRITE,lit,0,BLKUPDATE,STORE
FLUSH1:
        dw EXIT

;C LOAD                  n  --    load block n
;     BLK @ >R
;     DUP 0= IF    BLOCK DROP
;            ELSE  BLOCK B/BLK EVALUATE  THEN
;     R> BLOCK DROP  ;
    head(LOAD,LOAD,docolon)
        dw BLK,FETCH,TOR
        dw DUP,lit,0,EQUAL,qbranch,LOAD1
        dw BLOCK,DROP,branch,LOAD2
LOAD1:
        dw BLOCK,B_BLK,EVALUATE
LOAD2:
        dw RFROM,BLOCK,DROP
        dw EXIT

;C THRU            n1 n2  --    load blocks n1 to n2
;     1+ SWAP DO I LOAD LOOP ;
    head(THRU,THRU,docolon)
        dw ONEPLUS,SWOP,xdo
THRU1:
        dw II,DUP,DOT,LOAD,xloop,THRU1
        dw EXIT

; RC2014 EXTENSION (SCREENS) ====================

;Z (BLOCK)                 -- a-addr  load block in SCR
;     SCR @ BLOCK ;
    head(XBLOCK,(BLOCK),docolon)
        dw SCR,FETCH,BLOCK
        dw EXIT

;Z (LINE)           line# -- c-addr   address of line in block
;     C/L * (BLOCK) + ;
    head(XLINE,(LINE),docolon)
        dw C_L,STAR,XBLOCK,PLUS
        dw EXIT

;Z LL               line# --      List Line
;     (LINE) C/L TYPE CR ;
    head(LL,LL,docolon)
        dw XLINE,C_L,TYPE,CR
        dw EXIT


;Z  (LIST)            --    runtime for list screen
;     L/B 0 DO I 2 .R SPACE I LL LOOP ;
    head(XLIST,(LIST),docolon)
        dw L_B,lit,0,xdo
XLIST1:
        dw II,lit,2,DOTR,SPACE,II,LL,xloop,XLIST1
        dw EXIT

;C LIST ( n -- )            list screen number
;     DUP SCR ! ." SCR # " . CR (LIST) ;
    head(LIST,LIST,docolon)
        dw DUP,SCR,STORE,XSQUOTE
        db 6,"SCR # "
        dw TYPE,DOT,CR
        dw XLIST
        dw EXIT

;C INDEX ( from to -- )     print first line of each screen
;     CR 1+ SWAP DO I 2 .R SPACE I DUP SCR ! BLOCK DROP 0 LL LOOP ;
    head(INDEX,INDEX,docolon)
        dw CR,ONEPLUS,SWOP,xdo
INDEX1:
        dw II,lit,2,DOTR,SPACE
        dw II,DUP,SCR,STORE,BLOCK,DROP
        dw lit,0,LL,xloop,INDEX1
        dw EXIT

;Z WIPE     ( n -- erase block n )
;    BUFFER 1024 BLANKS
;    UPDATE ;
    head(WIPE,WIPE,docolon)
        dw BUFFER,lit,1024,BLANKS
        dw UPDATE
        dw EXIT

;: MARKER  ( "name" -- )
;    HERE >R
;    CREATE LATEST @ NFA>LFA FETCH ,
;    R> ,
;    DOES> DUP  @ LATEST !
;            CELL+  @ DP ! ;
    head(MARKER,MARKER,docolon)
        DW HERE,TOR
        DW CREATE,LATEST,FETCH
        DW NFATOLFA,FETCH,COMMA
        DW RFROM,COMMA

        DW XDOES
        call dodoes
        DW DUP,FETCH,LATEST,STORE
        DW CELLPLUS,FETCH,DP,STORE
        dw EXIT

; (BSAVE) ( c-addr u -- )  save block to file
;    BLK @ BUFFER                               ( c-addr u buffer )
;    SWAP MOVE
;    UPDATE  ;
    head(XBSAVE,(BSAVE),docolon)
        dw BLK,FETCH,BUFFER
        dw SWOP,MOVE
        dw UPDATE
        dw EXIT

;: BSAVE   ( c-addr u blk -- )
;    BUFFER DROP    ( c-addr u -- ; blk in BLK)
;
;    B/BLK /MOD    ( c-addr rem #blks )
;    SWAP   >R      (c-addr #blks ; rem )
;    ?DUP IF
;      0 DO                  ( c-addr ; rem )
;        DUP B/BLK (BSAVE)   ( c-addr ; rem )
;        B/BLK +             ( c-addr' ; rem )
;        1 BLK +!            ( c-addr' ; rem )
;      LOOP
;    THEN           ( c-addr ; rem )
;
;    R> (BSAVE) ( c-addr rem )
;    FLUSH  ;
    head(BSAVE,BSAVE,docolon)
        dw BUFFER,DROP
        dw B_BLK,SLASHMOD
        dw SWOP,TOR
        dw QDUP,qbranch,BSAVE1
        dw lit,0,xdo
BSAVE2:
        dw DUP,B_BLK,XBSAVE
        dw B_BLK,PLUS
        dw lit,1,BLK,PLUSSTORE
        dw xloop,BSAVE2
BSAVE1:
        dw RFROM,XBSAVE
        dw FLUSH
        dw EXIT

; (BLOAD) ( c-addr u -- )  save block to file
;    BLK @ BLOCK                            ( c-addr u buffer )
;    ROT ROT MOVE    ;
    head(XBLOAD,(BLOAD),docolon)
        dw BLK,FETCH,BLOCK
        dw ROT,ROT,MOVE
        dw EXIT

;: BLOAD   ( blk c-addr u -- )
;    ROT BLOCK DROP    ( c-addr u -- ; blk in BLK)
;
;    B/BLK /MOD    ( c-addr rem #blks )
;    SWAP   >R      (c-addr #blks ; rem )
;    ?DUP IF
;      0 DO                  ( c-addr ; rem )
;        DUP B/BLK (BLOAD)   ( c-addr ; rem )
;        B/BLK +             ( c-addr' ; rem )
;        1 BLK +!            ( c-addr' ; rem )
;      LOOP
;    THEN           ( c-addr ; rem )
;
;    R> (BLOAD) ( c-addr rem )     ;
    head(BLOAD,BLOAD,docolon)
        dw ROT,BLOCK,DROP
        dw B_BLK,SLASHMOD
        dw SWOP,TOR
        dw QDUP,qbranch,BLOAD1
        dw lit,0,xdo
BLOAD2:
        dw DUP,B_BLK,XBLOAD
        dw B_BLK,PLUS
        dw lit,1,BLK,PLUSSTORE
        dw xloop,BLOAD2
BLOAD1:
        dw RFROM,XBLOAD
        dw EXIT


defc BLK_HEADER_SIZE = 64
defc BLK_DATA_SIZE   = 960

; (SAVEHDR)            ( c-addr u buffer -- c-addr' u' )
; --------------------------------------------------------------
; Header
;    size of header
;    size of data
;    LATEST 2 bytes. last word in dict
;    DP 2 bytes. dictionary pointer
;    VOCLNK 2 bytes.
;    CONTEXT 2 bytes (for number of items on wordlist stack)
;         followed by n items of stack.
; --------------------------------------------------------------
;    Setup header
;    BUFFER     ( c-addr u buffer -- ; blk in BLK)
;      DUP BLK_HEADER_SIZE ERASE
;      BLK_HEADER_SIZE OVER !    ( store header size )
;      CELL+ 2DUP !              ( store data size )
;      CELL+ LATEST @ OVER !     ( store LATEST )
;      CELL+ DP @ OVER !         ( store DP )
;      DROP                      ( c-addr u )
;
;    BUFFER BLK_HDR_SIZE +    ( c-addr u buffer' )
;    OVER BLK_DATA_SIZE > IF
;      >R OVER R> BLK_DATA_SIZE MOVE UPDATE  ( c-addr u )
;      SWAP BLK_DATA_SIZE +                  ( u c-addr' )
;      SWAP BLK_DATA_SIZE -                  ( c-addr' u' )
;    ELSE                     ( c-addr u buffer' )
;      >R 2DUP R>             ( c-addr u c-addr u buffer' )
;       SWAP MOVE UPDATE      ( c-addr u )
;       + 0                   ( c-addr' 0 )
;    THEN   FLUSH  ;
    head(XSAVEHDR,(SAVEHDR),docolon)
        dw DUP,lit,BLK_HEADER_SIZE,ERASE
        dw lit,BLK_HEADER_SIZE,OVER,STORE
        dw CELLPLUS,TWODUP,STORE
        dw CELLPLUS,LATEST,FETCH,OVER,STORE
        dw CELLPLUS,DP,FETCH,OVER,STORE
        dw DROP

        dw BLK,FETCH,BUFFER,lit,BLK_HEADER_SIZE,PLUS
        dw OVER,lit,BLK_HEADER_SIZE,GREATER,qbranch,XSAVEHDR1
        dw TOR,OVER,RFROM,lit,BLK_DATA_SIZE,MOVE,UPDATE
        dw SWOP,lit,BLK_DATA_SIZE,PLUS
        dw SWOP,lit,BLK_DATA_SIZE,MINUS
        dw branch,XSAVEHDR2
XSAVEHDR1:
        dw TOR,TWODUP,RFROM
        dw SWOP,MOVE,UPDATE
        dw PLUS,lit,0
XSAVEHDR2:
        dw FLUSH
        dw EXIT

;: SAVE   ( blk -- )
;    enddict   DP @ enddict -    ( blk c-addr u )
;    ROT BUFFER                  ( c-addr u buffer -- ; blk in BLK)
;    (BSAVEHDR)                  ( c-addr' u' )
;
;    B/BLK /MOD    ( c-addr rem #blks )
;    SWAP   >R      (c-addr #blks ; rem )

;    BLK @ 1+ BUFFER DROP
;
;    ?DUP IF
;      0 DO                  ( c-addr ; rem )
;        DUP B/BLK (BSAVE)   ( c-addr ; rem )
;        B/BLK +             ( c-addr' ; rem )
;        BLK @ 1+ BUFFER DROP            ( c-addr' ; rem )
;      LOOP
;    THEN           ( c-addr ; rem )
;
;    R>
;    QDUP  IF (BSAVE) ELSE DROP THEN    (  )
;    FLUSH   ;
    head(SAVE,SAVE,docolon)
        dw lit,enddict,DP,FETCH,lit,enddict,MINUS  ;  ( block c-addr u )
        dw ROT,BUFFER                              ;  ( c-addr u buffer )
        dw XSAVEHDR
        dw BLK,FETCH,ONEPLUS,BUFFER,DROP

        dw B_BLK,SLASHMOD
        dw SWOP,TOR

        dw QDUP,qbranch,SAVE2
        dw lit,0,xdo
SAVE1:
        dw DUP,B_BLK,XBSAVE
        dw B_BLK,PLUS
        dw BLK,FETCH,ONEPLUS,BUFFER,DROP
        dw xloop,SAVE1
SAVE2:
        dw RFROM
        dw QDUP,qbranch,SAVE3
        dw XBSAVE
        dw branch,SAVE4
SAVE3:
        dw DROP
SAVE4:
        dw FLUSH
        dw EXIT

;: RESTORE   ( blk -- )
;    BLOCK                       ( buffer -- ; blk in BLK)
;    DUP @                       ( buffer hdr_size )
;    OVER +                      ( buffer buffer' )
;    SWAP CELL+ DUP @            ( buffer' buffer data_size )
;    SWAP CELL+ DUP @ LATEST !   ( buffer' data_size buffer )
;    CELL+ DUP @ DP !            ( buffer' data_size buffer )
;
;    DROP SWAP enddict ROT       ( buffer' enddict u)
;
;    DUP BLK_DATA_SIZE > IF   ( buffer' c-addr u )
;      >R 2DUP BLK_DATA_SIZE MOVE NIP R>     ( c-addr u )
;      SWAP BLK_DATA_SIZE +                  ( u c-addr' )
;      SWAP BLK_DATA_SIZE -                  ( c-addr' u' )
;    ELSE                     ( buffer' c-addr u )
;      >R 2DUP R@             ( buffer' c-addr buffer' c-addr u )
;       MOVE NIP R>           ( c-addr u )
;       + 0                   ( c-addr' 0 )
;    THEN
;
;    lastword enddict !          ( patch link address )
;
;    BLK @ 1+ BLOCK DROP
;
;    B/BLK /MOD    ( c-addr rem #blks )
;    SWAP   >R      (c-addr #blks ; rem )
;    ?DUP IF
;      0 DO                  ( c-addr ; rem )
;        DUP B/BLK (BLOAD)   ( c-addr ; rem )
;        B/BLK +             ( c-addr' ; rem )
;        BLK @ 1+ BLOCK DROP ( c-addr' ; rem )
;      LOOP
;    THEN           ( c-addr ; rem )
;    R>       ( c-addr rem )
;
;    QDUP  IF (BLOAD) ELSE DROP THEN  ;  (  )
    head(RESTORE,RESTORE,docolon)
        dw BLOCK
        dw DUP,FETCH               ; header size
        dw OVER,PLUS
        dw SWOP,CELLPLUS,DUP,FETCH
        dw SWOP,CELLPLUS,DUP,FETCH,LATEST,STORE
        dw CELLPLUS,DUP,FETCH,DP,STORE

        dw DROP,SWOP,lit,enddict,ROT

        dw DUP,lit,BLK_DATA_SIZE,GREATER,qbranch,RESTORE1
        dw TOR,TWODUP,lit,BLK_DATA_SIZE,MOVE,NIP,RFROM
        dw SWOP,lit,BLK_DATA_SIZE,PLUS
        dw SWOP,lit,BLK_DATA_SIZE,MINUS
        dw branch,RESTORE2
RESTORE1:
        dw TOR,TWODUP,RFETCH
        dw MOVE,NIP,RFROM
        dw PLUS,lit,0

RESTORE2:
        dw lit,lastword,lit,enddict,STORE
        dw BLK,FETCH,ONEPLUS,BLOCK,DROP

        dw B_BLK,SLASHMOD
        dw SWOP,TOR

        dw QDUP,qbranch,RESTORE4
        dw lit,0,xdo
RESTORE3:
        dw DUP,B_BLK,XBLOAD
        dw B_BLK,PLUS
        dw BLK,FETCH,ONEPLUS,BLOCK,DROP
        dw xloop,RESTORE3
RESTORE4:
        dw RFROM
        dw QDUP,qbranch,RESTORE5
        dw XBLOAD
        dw branch,RESTORE6
RESTORE5:
        dw DROP

RESTORE6:
        dw EXIT

