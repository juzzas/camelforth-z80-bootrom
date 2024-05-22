dnl ; \ Forth-94 version of Klaus Schleisiek's dynamic memory allocation (FORML'88) uh 2016-10-28
dnl ;
dnl ;        |<------------- len ------------->|
dnl ;0       |2      4                         |
dnl ;---------------------------------------------------
dnl ;| X_len | >PTR | <PTR |   empty memory    | X_len |
dnl ;---------------------------------------------------
dnl ;         ^
dnl ;       anchor
dnl ;
dnl ;address of >PTR is the reference address of a memory block which becomes the address of useable memory after allocation.
dnl ;
dnl ;X is MSB and set, if block is free, not set if used
dnl ;LEN is usable length in bytes
dnl ;>PTR is absolute Addr. of next empty block
dnl ;<PTR is absolute Addr. of previous empty block
dnl
; Variable anchor  0 anchor !
    head(ANCHOR,ANCHOR,docolon)
        DW lit,anchor_ptr
        DW EXIT

SECTION data_user

anchor_ptr:
    DEFW 0

SECTION code_user_16k

; decimal 050 Constant waste
    head(HEAP_WASTE,HEAP-WASTE,docon)
        DW 50

; -1 1 rshift Constant #max
    head(HEAP_NUMMAX,``#HEAP-MAX'',docon)
        DW 0x7FFF

; #max invert Constant #free  \ sign bit
    head(HEAP_NUMFREE,``#HEAP-FREE'',docon)
        DW 0x8000

; : size ( mem -- size ) 1 cells - @ #max and ;
    head(HEAP_SIZE,HEAP-SIZE,docolon)
        DW lit,1,CELLS,MINUS,FETCH,HEAP_NUMMAX,AND
        DW EXIT

; : addr&size ( mem -- mem size ) dup size ;
    head(HEAP_ADDR_SIZE,``HEAP-ADDR&SIZE'',docolon)
        DW DUP,HEAP_SIZE
        DW EXIT

; : above ( mem -- >mem )   addr&size + 2 cells + ;
HEAP_ABOVE:
        call docolon
        DW HEAP_ADDR_SIZE,PLUS
        DW lit,2,CELLS,PLUS
        DW EXIT

; : use ( mem size -- )
;     dup >r swap  2dup 1 cells - !  r> #max and + ! ;
HEAP_USE:
        call docolon
        DW DUP,TOR,SWOP,TWODUP,lit,1,CELLS,MINUS,STORE,RFROM,HEAP_NUMMAX,AND,PLUS,STORE
        DW EXIT


; : release ( mem size -- )
;       #free or use ;
HEAP_RELEASE:
        call docolon
        DW HEAP_NUMFREE,OR,HEAP_USE
        DW EXIT

; : fits? ( size -- mem | false )
;    >r anchor @
;    BEGIN addr&size  r@ u< 0=
;          IF r> drop EXIT THEN
;          @ dup anchor @ =
;    UNTIL 0= r> drop ;
HEAP_FITSQ:
        call docolon
        DW TOR,ANCHOR,FETCH
HEAP_FITSQ1:
        DW HEAP_ADDR_SIZE,RFETCH,ULESS,ZEROEQUAL,qbranch,HEAP_FITSQ2
        DW RFROM,DROP,EXIT
HEAP_FITSQ2:
        DW FETCH,DUP,ANCHOR,FETCH,EQUAL,qbranch,HEAP_FITSQ1
        DW ZEROEQUAL,RFROM,DROP
        DW EXIT

; : link ( mem >mem <mem -- )
;    >r 2dup cell+ !  over !  r> 2dup !  swap cell+ ! ;
HEAP_LINK:
        call docolon
        DW TOR,TWODUP,CELLPLUS,STORE,OVER,STORE,RFROM,TWODUP,STORE,SWOP,CELLPLUS,STORE
        DW EXIT

; : @links ( mem -- <mem mem> )  dup @  swap cell+ @ ;
HEAP_FETCHLINKS:
        call docolon
        DW DUP,FETCH,SWOP,CELLPLUS,FETCH
        DW EXIT

; : setanchor ( mem -- mem )
;    dup anchor @ = IF  dup @ anchor ! THEN ;
HEAP_SETANCHOR:
        call docolon
        DW DUP,ANCHOR,FETCH,EQUAL,qbranch,HEAP_SETANCHOR1
        DW DUP,FETCH,ANCHOR,STORE
HEAP_SETANCHOR1:
        DW EXIT

; : unlink ( mem -- ) setanchor  @links 2dup !  swap cell+ ! ;
HEAP_UNLINK:
        call docolon
        DW HEAP_SETANCHOR,HEAP_FETCHLINKS,TWODUP,STORE,SWOP,CELLPLUS,STORE
        DW EXIT

; : allocate ( size -- mem ior )
;    3 cells max dup >r  fits? ?dup 0= IF r> -8 EXIT THEN ( "dictionary overflow" )
;    addr&size r@ -  dup waste u<
;    IF  drop  dup @ over unlink  over addr&size use
;    ELSE 2 cells -   over r@ use
;         over above   dup rot release
;         2dup swap @links link THEN
;    r> drop  anchor ! 0 ;
    head(ALLOCATE,ALLOCATE,docolon)
        DW lit,3,CELLS,MAX,DUP,TOR,HEAP_FITSQ,QDUP,ZEROEQUAL,qbranch,HEAP_ALLOCATE1
        DW RFROM,lit,-8
        DW EXIT
HEAP_ALLOCATE1:
        DW HEAP_ADDR_SIZE,RFETCH,MINUS,DUP,HEAP_WASTE,ULESS,qbranch,HEAP_ALLOCATE2
        DW DROP,DUP,FETCH,OVER,HEAP_UNLINK,OVER,HEAP_ADDR_SIZE,HEAP_USE
        DW branch,HEAP_ALLOCATE3
HEAP_ALLOCATE2:
        DW lit,2,CELLS,MINUS,OVER,RFETCH,HEAP_USE
        DW OVER,HEAP_ABOVE,DUP,ROT,HEAP_RELEASE
        DW TWODUP,SWOP,HEAP_FETCHLINKS,HEAP_LINK
HEAP_ALLOCATE3:
        DW RFROM,DROP,ANCHOR,STORE
        DW lit,0
        DW EXIT

; : free ( mem -- ior )
;    addr&size  over 2 cells -  @ dup 0<
;    IF #max and 2 cells +  rot over - rot rot +
;    ELSE  drop  over anchor @  dup cell+ @  link THEN
;    2dup + cell+ dup @ dup 0<
;    IF  #max and swap cell+ unlink  +  2 cells +  release 0 EXIT THEN
;    2drop release 0 ;
    head(FREE,FREE,docolon)
        DW HEAP_ADDR_SIZE,OVER,lit,2,CELLS,MINUS,FETCH,DUP,ZEROLESS,qbranch,FREE1
        DW HEAP_NUMMAX,AND,lit,2,CELLS,PLUS,ROT,OVER,MINUS,ROT,ROT,PLUS
        DW branch,FREE2
FREE1:
        DW DROP,OVER,ANCHOR,FETCH,DUP,CELLPLUS,FETCH,HEAP_LINK
FREE2:
        DW TWODUP,PLUS,CELLPLUS,DUP,FETCH,DUP,ZEROLESS,qbranch,FREE3
        DW HEAP_NUMMAX,AND,SWOP,CELLPLUS,HEAP_UNLINK,PLUS,lit,2,CELLS,PLUS,HEAP_RELEASE,lit,0
        DW EXIT
FREE3:
        DW TWODROP,HEAP_RELEASE,lit,0
        DW EXIT

; : resize ( mem newsize -- mem' ior )
;  ;    over swap  over size  2dup >
;     IF ( mem mem size newsize )  swap allocate ?dup IF >r drop 2drop r>  EXIT THEN
;         dup >r swap move free r> swap EXIT THEN
;     2drop drop 0 ;
    head(RESIZE,RESIZE,docolon)
        DW OVER,SWOP,OVER,HEAP_SIZE,TWODUP,GREATER,qbranch,RESIZE1
        DW SWOP,ALLOCATE,QDUP,qbranch,RESIZE2
        DW TOR,DROP,TWODROP,RFROM
        DW EXIT
RESIZE1:
        DW DUP,TOR,SWOP,MOVE,FREE,RFROM,SWOP
        DW EXIT
RESIZE2:
        DW TWODROP,DROP,lit,0
        DW EXIT

; : empty-memory ( addr size -- )
;    >r  cell+ dup anchor !   dup 2 cells use  dup 2dup link
;    dup above  swap over  dup link
;    dup r> 7 cells -  release  above 1 cells -  0 swap ! ;
    head(EMPTYMEMORY,EMPTY-MEMORY,docolon)
        DW TOR,CELLPLUS,DUP,ANCHOR,STORE,DUP,lit,2,CELLS,HEAP_USE,DUP,TWODUP,HEAP_LINK
        DW DUP,HEAP_ABOVE,SWOP,OVER,DUP,HEAP_LINK
        DW DUP,RFROM,lit,7,CELLS,MINUS,HEAP_RELEASE,HEAP_ABOVE,lit,1,CELLS,MINUS,lit,0,SWOP,STORE
        DW EXIT

; cr
; cr .( dynamic memory allocation:)
; cr .( Use   addr size EMPTY-MEMORY  to initialize,)
; cr .( then use the standard memory allocation wordset ALLOCATE FREE RESIZE to manage memory.)

;\ display chain of free memory blocks ks 13 nov
; : end? ( addr - addr f )
;     dup anchor @ = key? or ;

HEAP_ENDQ:
        call docolon
        DW DUP,ANCHOR,FETCH,EQUAL,QUERYKEY,OR
        DW EXIT

; : ?cr ( f -- f )
;     DUP IF CR THEN ;
    head(QCR,``?CR'',docolon)
        DW DUP,qbranch,QCR1
        DW CR
QCR1:
        DW EXIT

; : ?memory anchor @
;     cr ." ->:"
;     BEGIN ?cr dup 6 u.r ." : '
;     addr&len 4 u.r @ end?
;     UNTIL
;     cr .' <-:'
;     BEGIN ?cr dup 6 u.r ." : "
;         addr&len 4 u.r cell* @ end?
;     UNTIL drop ;
    head(HEAP_MEMORYQ,``?MEMORY'',docolon)
        DW ANCHOR,FETCH
        DW CR,XSQUOTE
        DB 4," ->:"
        DW TYPE
HEAP_MEMORYQ1:
        DW QCR,DUP,lit,6,UDOTR,XSQUOTE
        DB 3," : "
        DW TYPE
        DW HEAP_ADDR_SIZE,lit,4,UDOTR,FETCH,HEAP_ENDQ,qbranch,HEAP_MEMORYQ1
        DW CR,XSQUOTE
        DB 4," <-:"
        DW TYPE
HEAP_MEMORYQ2:
        DW QCR,DUP,lit,6,UDOTR,XSQUOTE
        DB 3," : "
        DW TYPE
        DW HEAP_ADDR_SIZE,lit,4,UDOTR,CELLPLUS,FETCH,HEAP_ENDQ,qbranch,HEAP_MEMORYQ2
        DW DROP
        DW EXIT

