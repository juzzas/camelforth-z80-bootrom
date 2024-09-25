\ Version of Klaus Schleisiek's dynamic memory alloc.   0 / 8 

.( Loading heap definitions... ) CR

\ dynamic memory allocation:
\ Use   addr size EMPTY-MEMORY  to initialize,
\ then use the standard memory allocation wordset
\ ALLOCATE FREE RESIZE to manage memory.

DECIMAL
FORTH DEFINITIONS
1 8 +THRU





   \ Version of Klaus Schleisiek's dynamic memory alloc.  1 / 8
\ 
\         |<------------- len ------------->|
\ 0       |2      4                         |
\ ---------------------------------------------------
\ | X_len | >PTR | <PTR |   empty memory    | X_len |
\ ---------------------------------------------------
\          ^
\        anchor
\ 
\ address of >PTR is the reference address of a memory block
\ which becomes the address of useable memory after allocation.
\ X is MSB and set, if block is free, not set if used
\ LEN is usable length in bytes
\ >PTR is absolute Addr. of next empty block
\ <PTR is absolute Addr. of previous empty block
   \ Version of Klaus Schleisiek's dynamic memory alloc.  2 / 8
VARIABLE ANCHOR  0 ANCHOR !
DECIMAL 050 CONSTANT WASTE
-1 1 RSHIFT CONSTANT #MAX
#MAX INVERT CONSTANT #FREE    ( sign bit )

: SIZE ( mem -- size ) 1 CELLS - @ #MAX AND ;
: ADDR&SIZE ( mem -- mem size ) DUP SIZE ;
: ABOVE ( mem -- >mem )   ADDR&SIZE + 2 CELLS + ;

: USE ( mem size -- )
    DUP >R SWAP  2DUP 1 CELLS - !  R> #MAX AND + ! ;
: RELEASE ( mem size -- )
      #FREE OR USE ;

   \ Version of Klaus Schleisiek's dynamic memory alloc.  3 / 8
: FITS? ( size -- mem | false )
   >R ANCHOR @
   BEGIN ADDR&SIZE  R@ U< 0=
      IF R> DROP EXIT THEN
      @ DUP ANCHOR @ =
   UNTIL 0= R> DROP ;

: LINK ( mem >mem <mem -- )
   >R 2DUP CELL+ !  OVER !  R> 2DUP !  SWAP CELL+ ! ;

: @LINKS ( mem -- <mem mem> )  DUP @  SWAP CELL+ @ ;

: SETANCHOR ( mem -- mem )
    DUP ANCHOR @ = IF  DUP @ ANCHOR ! THEN ;


   \ Version of Klaus Schleisiek's dynamic memory alloc.  4 / 8
: UNLINK ( mem -- ) SETANCHOR  @LINKS 2DUP !  SWAP CELL+ ! ;

: ALLOCATE ( size -- mem ior )
    3 CELLS MAX DUP >R  FITS? ?DUP 0= IF R> -8 EXIT THEN 
                                    ( "dictionary overflow" )
    ADDR&SIZE R@ -  DUP WASTE U<
    IF  DROP  DUP @ OVER UNLINK  OVER ADDR&SIZE USE
    ELSE 2 CELLS -   OVER R@ USE
       OVER ABOVE   DUP ROT RELEASE
       2DUP SWAP @LINKS LINK THEN
    R> DROP  ANCHOR ! 0 ;




   \ Version of Klaus Schleisiek's dynamic memory alloc.  5 / 8
: FREE ( mem -- ior )
   ADDR&SIZE  OVER 2 CELLS -  @ DUP 0<
   IF #MAX AND 2 CELLS +  ROT OVER - ROT ROT +
   ELSE  DROP  OVER ANCHOR @  DUP CELL+ @  LINK THEN
   2DUP + CELL+ DUP @ DUP 0<
   IF  #MAX AND SWAP CELL+ UNLINK  +  2 CELLS +
          RELEASE 0 EXIT THEN
   2DROP RELEASE 0 ;
: RESIZE ( mem newsize -- mem' ior )
    OVER SWAP  OVER SIZE  2DUP >
    IF ( mem mem size newsize )  SWAP ALLOCATE ?DUP
        IF >R DROP 2DROP R>  EXIT THEN
        DUP >R SWAP MOVE FREE R> SWAP EXIT THEN
    2DROP DROP 0 ;

   \ Version of Klaus Schleisiek's dynamic memory alloc.  6 / 8
: EMPTY-MEMORY ( addr size -- )
   >R  CELL+ DUP ANCHOR !   DUP 2 CELLS USE  DUP 2DUP LINK
   DUP ABOVE  SWAP OVER  DUP LINK
   DUP R> 7 CELLS -  RELEASE  ABOVE 1 CELLS -  0 SWAP ! ;











   \ Version of Klaus Schleisiek's dynamic memory alloc.  7 / 8
( display chain of free memory blocks ks 13 nov   )
: END? ( addr - addr f )
    DUP ANCHOR @ = KEY? OR ;

: ?CR ( f -- f )
    DUP IF CR THEN ;









   \ Version of Klaus Schleisiek's dynamic memory alloc.  8 / 8
: .HEAP ANCHOR @
    CR ." ->:"
    BEGIN ?CR DUP 6 U.R ." : "
    ADDR&SIZE 4 U.R @ END?
    UNTIL
    CR ." <-:"
    BEGIN ?CR DUP 6 U.R ." : "
       ADDR&SIZE 4 U.R CELL+ @ END?
    UNTIL DROP ;
