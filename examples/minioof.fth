( Mini-OOF )
: METHOD ( m v "name" -- m' v ) CREATE  OVER , SWAP CELL+ SWAP
  DOES> ( ... o -- ... ) @ OVER @ + @ EXECUTE ;
: VAR ( m v size "name" -- m v' ) CREATE  OVER , +
  DOES> ( o -- addr ) @ + ;
: CLASS ( class -- class methods vars ) DUP 2@ ;
: END-CLASS  ( class methods vars "name" -- )  CREATE  HERE >R
  , DUP , 2 CELLS 2DUP <> IF DO ['] NOOP , 1 CELLS +LOOP THEN
   CELL+ DUP CELL+ R> ROT @ 2 CELLS /STRING MOVE ;
: >VT ( class "name" -- addr )  ' >BODY @ + ;
: BIND ( class "name" -- xt )    >VT @ ;
: DEFINES ( xt class "name" -- ) >VT ! ;
: NEW ( class -- o )  HERE OVER @ ALLOT SWAP OVER ! ;
: :: ( class "name" -- ) BIND COMPILE, ;
CREATE OBJECT  1 CELLS , 2 CELLS ,


OBJECT CLASS
   METHOD init
   METHOD draw
END-CLASS graphical

graphical CLASS
  CELL VAR circle-radius
   METHOD draw2
END-CLASS circle  ( "graphical" is the parent class )

:NONAME ( x y -- )
  circle-radius @ >R  SWAP ." X=" . ." Y=" . ." Radius=" R> . ;

circle DEFINES draw

:NONAME ( r -- )
  circle-radius ! ;

circle DEFINES init

 circle NEW CONSTANT my-circle
     50 my-circle init

 100 100 my-circle draw
