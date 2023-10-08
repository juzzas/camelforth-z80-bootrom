( Retro Forth block editor for CamelForth )

: .BLOCK ." Screen: " SCR @ DUP . UPDATED? 43 + EMIT SPACE ;
: +--- ." +---" ;
: :--- ." :---" ;
: x--- +--- :--- +--- :--- ;
: ---  3 SPACES x--- x--- x--- x--- CR ;
: VB --- SCR @ BLOCK (LIST) DROP --- ;
: .STACK ." Stack: " .S ;
: STATUS .BLOCK .STACK ;
: V CR VB STATUS ;

: V* UPDATE V ;
: S DUP SCR ! BLOCK DROP V ;
: IA (LINE) + >R 13 WORD COUNT R> SWAP MOVE V* ;
: P 0 SWAP IA ;
: D (LINE) C/L BL FILL V* ;
: X (BLOCK) L/B C/L * BL FILL V* ;
: B -1 SCR +! V ;
: N 1 SCR +! V ;
: E SCR @ LOAD ;

CR ( editor loaded ) CR
