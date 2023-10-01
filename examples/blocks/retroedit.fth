.( Retro Forth block editor for gForth )
cr

16 constant l/b
: (block) scr @ block ;
: (line) c/l * (block) + ;

: row dup c/l type c/l + cr ;
: .rows l/b 0 do i . row loop ;
: .block ." Block: " scr @ dup . updated? 43 + emit space ;
: +--- ." +---" ;
: :--- ." :---" ;
: x--- +--- :--- +--- :--- ;
: --- space space x--- x--- x--- x--- cr ;
: vb --- scr @ block .rows drop --- ;
: .stack ." Stack: " .s ;
: status .block .stack ;
: v cr vb status ;

: v* update v ;
: s dup scr ! block drop v ;
: ia (line) + >r 10 parse r> swap move v* ;
: i 0 swap ia v* ;
: d (line) c/l bl fill v* ;
: x (block) l/b c/l * bl fill v* ;
: p -1 scr +! v ;
: n 1 scr +! v ;
: e scr @ load ;

cr .( editor loaded ) cr
