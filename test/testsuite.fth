\ Forth-2012 test suite
CR .( Starting Forth 2012 test suite... )

150 LOAD     \ blkfile
3012 LOAD     \ ttester
30 LOAD      \ load Tools
1 9 +THRU









\ Forth-2012 test suite   1 / 2
3021 CONSTANT TEST_PRELIM
3033 CONSTANT TEST_CORE
3063 CONSTANT TEST_COREPLUS
3093 CONSTANT TEST_COREEXT
3123 CONSTANT TEST_BLOCK
3150 CONSTANT TEST_DOUBLE
3168 CONSTANT TEST_EXCEPTION
3174 CONSTANT TEST_FACILITY
3180 CONSTANT TEST_FILE
3192 CONSTANT TEST_LOCALS
3201 CONSTANT TEST_MEMORY
3228 CONSTANT TEST_SEARCHORDER
3237 CONSTANT TEST_STRING


\ CR .( Starting preliminary tests... )  3021 INCLUDE-BLKFILE
CR .( Starting core tests... )         3033 INCLUDE-BLKFILE
DECIMAL
CR .( Starting coreplustests )         3063 INCLUDE-BLKFILE

CR .( Loading test utilities... )        3078 INCLUDE-BLKFILE
CR .( Loading error reporting... )       3087 INCLUDE-BLKFILE
300 LOAD     \ load Forth 2012 extenstion wordset loaders

MARKER <reset>
CR .( Starting core ext tests... )
WORDSET_CORE_EXT LOAD
TEST_COREEXT INCLUDE-BLKFILE
CR .( free space: ) UNUSED U.
<reset>

\ test the toolset before we use the compilation wordset
CR .( Starting tools tests... )
MARKER <reset>
WORDSET_TOOLS_EXT LOAD
3207 INCLUDE-BLKFILE
<reset>

[DEFINED] TEST_EXCEPTION [IF]
MARKER <reset>
CR .( Starting exception tests... )
\  WORDSET_EXCEPTION LOAD
TEST_EXCEPTION INCLUDE-BLKFILE
<reset>
[THEN]

[DEFINED] TEST_LOCALS [IF]
   MARKER <reset>
   CR .( Starting locals tests... )
   WORDSET_LOCALS LOAD
   TEST_LOCALS INCLUDE-BLKFILE
   <reset> 
[THEN]


[DEFINED] TEST_BLOCK [IF]
   MARKER <reset>
   CR .( Starting block tests... )
   CR .( - loading assembler )
   90 LOAD    \ load assembler
   CR .( - loading pagedram )
   927 LOAD   \ load pagedram wordset
   CR .( - loading ramloader )
   3252 INCLUDE-BLKFILE  \ load ramloader
   TEST_BLOCK  RAMLOAD
   <reset>
[THEN]

[DEFINED] TEST_FILE [IF]
   MARKER <reset>
   CR .( Starting file tests... )
   WORDSET_CORE_EXT LOAD
   WORDSET_FILEACCESS LOAD
   3300 ROOT!
   TEST_FILE INCLUDE-BLKFILE
   <reset>
[THEN]


[DEFINED] TEST_STRING [IF]
   MARKER <reset>
   CR .( Starting string tests... )
   WORDSET_STRING LOAD
   WORDSET_STRING_EXT LOAD
   TEST_STRING INCLUDE-BLKFILE
   <reset>
[THEN]

[DEFINED] TEST_DOUBLE [IF]
   MARKER <reset>
   CR .( Starting double tests... )
   WORDSET_DOUBLE_EXT LOAD
   TEST_DOUBLE INCLUDE-BLKFILE
   <reset>
[THEN]

[DEFINED] TEST_FACILITY [IF]
   MARKER <reset>
   CR .( Starting facility tests... )
   WORDSET_FACILITY_EXT LOAD
   TEST_FACILITY INCLUDE-BLKFILE
   <reset>
[THEN]

[DEFINED] TEST_MEMORY [IF]
   MARKER <reset>
   CR .( Starting memory tests... )
   WORDSET_MEMORY LOAD
   2048 CONSTANT heap-size
   heap-size BUFFER: heap
   heap heap-size INIT-HEAP
   TEST_MEMORY INCLUDE-BLKFILE
   <reset>
[THEN]

[DEFINED] TEST_SEARCHORDER [IF]
   MARKER <reset>
   CR .( Starting search order tests... )
   WORDSET_SEARCHORDER LOAD
   TEST_SEARCHORDER INCLUDE-BLKFILE
   <reset>
[THEN]



