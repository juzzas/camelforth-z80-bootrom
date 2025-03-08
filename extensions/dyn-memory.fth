( Dynamic Memory Allocation -- Screen 1 )
\ The dynamic-memory is based on the algorithm given by
\ Dreas Nielsen in Forth Dimensions Volume 12 Issue 3
\ It's a basic first-fit memory allocation algorithm. For
\ simplicity reasons, RESIZE always allocates a new block if it
\ can.
\
\ Create a heap using
\      start-addr heap INIT-HEAP
\
\ Use the following standard Forth words to manipulate
\     ALLOCATE, FREE, RESIZE

CR .( Loading dynamic memory... )
1 5 +THRU

( Dynamic Memory Allocation -- Screen 1 )
\ Each block of free space begins with a 4-byte control block.
\ The first word contains the address of the next free block
\ [or 0 if none] and the second contains the number of bytes in
\ the current block [including the control block]. )

( Create pointer to beginning of free space, w/ size=O. )
2VARIABLE FREELIST 0 0 FREELIST 2!
( Initialize memory pool. )
: INIT-HEAP  ( start-addr length -- )
   OVER DUP FREELIST !    ( Save starting addr. )
   0 SWAP !               ( Set null pointer. )
   SWAP CELL+ !           ( Save length in 1st control block. )
;


( Dynamic Memory Allocation, Screen 2: MALLOC )
\ Returns pointer to n free bytes, or 0 if there is no space.
\  Word before returned address holds size of block. No free
\  blocks of less than 4 bytes are allowed. )
: (MALLOC) ( n -- c-addr if successful, else 0 )
   CELL+ FREELIST DUP
   BEGIN
   WHILE DUP @  CELL+ @  ( Size )  2 PICK  U<
   IF @ @ DUP                                  ( get new link )
   ELSE DUP @ CELL+ @    ( size )  2 PICK - 4 MAX DUP 4 =
      IF DROP  DUP @  DUP @  ROT !
      ELSE 2DUP SWAP  @  CELL+  ! SWAP @ +
      THEN 2DUP ! CELL+  0        ( store size, bump pointer, )
   THEN ( and set exit flag )
   REPEAT SWAP DROP                             ( dump #bytes )
;
( Dynamic Memory screen 3: FREE )
( Deallocates memory. Pointer passed must be from MALLOC )
: (FREE) ( c-addr -- )
   2 -  DUP @ SWAP  2DUP CELL+ !   FREELIST DUP
   BEGIN  DUP  3 PICK U<  AND
   WHILE  @ DUP @
   REPEAT                         ( at exit:  size block ptrl )
   DUP @ DUP  3 PICK !  ?DUP   ( sz blk ptrl 0 -or- ptr2 ptr2 )
   IF DUP  3 PICK  5 PICK +  =       ( size blk ptrl ptr2 t/f )
      IF DUP CELL+ @  4 PICK +  3 PICK  CELL+ !  @  2 PICK !
      ELSE DROP THEN                            ( sz blk ptrl )
  THEN ( sz blk ptrl )
  DUP CELL+  @ OVER +  2 PICK =             ( sz blk ptrl t/f )
  IF OVER CELL+ @  OVER CELL+ DUP @ ROT + SWAP ! SWAP @  SWAP !
  ELSE !
  THEN DROP ;
( Dynamic Memory screen 4: ALLOCATE, FREE )
: ALLOCATE  ( u -- a-addr ior )
    DUP 0< IF  -59 EXIT  THEN   ( error on negative value )
    (MALLOC) DUP IF  0  ( ior = success )
    ELSE  -59  THEN  ( ior = allocate exception value ) ;

: FREE  ( a-addr -- ior )
    (FREE)  0  ( always succeeds )  ;








( Dynamic Memory screen 5: RESIZE )
: RESIZE   ( a-addr1 u -- a-addr2 ior )
   DUP 0< IF DROP -61 EXIT THEN     ( error on negative value )
   DUP  (MALLOC)      ( addr1 i addr2 )
   DUP IF
      >R                                ( addr1 u2 ; r: addr2 )
      OVER  CELL- @  MIN              ( addr1 umin ; r: addr2 )
      OVER SWAP R@ SWAP   ( addr1 addr1 addr2 umin ; r: addr2 ) 
      MOVE   (FREE)
      R>  0    ( ior = success )
   ELSE     ( a-addr1 0 )
      DROP  -61  ( ior = resize exception value )
   THEN  ;
