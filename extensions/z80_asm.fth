HEX VOCABULARY ASSEMBLER IMMEDIATE
ALSO ASSEMBLER DEFINITIONS
\ 1 26 +THRU
VARIABLE TORIGIN


: TC,  TORIGIN @ 8 U.R DUP . C, ;
: T,   ,  ;
: THERE  HERE ;
: ABSOLUTE   T,  ;
: REL8   TC,  ;
: ERROR   -1 THROW ;




\ Define operand addressing modes for two operands sfp 15/04/87
HEX

1 CONSTANT 8BR                 ( 8 bit register, or [hl] )
2 CONSTANT IND-MODE            ( indirect - [ix], [iy] )
3 CONSTANT 16BR                ( 16 bit register )
4 CONSTANT IMM-MODE            ( immediate operand )
5 CONSTANT SPEC-REG            ( special cases of reg usage )
6 CONSTANT SP-IND              ( indirect through special regs )

\ Define registers                                 sfp 15/04/87
HEX
( 8 bit registers )
0 CONSTANT B-REG                1 CONSTANT C-REG
2 CONSTANT D-REG                3 CONSTANT E-REG
4 CONSTANT H-REG                5 CONSTANT L-REG
6 CONSTANT (HL)-REG             7 CONSTANT A-REG

( 16 bit registers )
0 CONSTANT BC-REG               1 CONSTANT DE-REG
2 CONSTANT HL-REG               3 CONSTANT SP-REG
0DD CONSTANT IX-REG             0FD CONSTANT IY-REG

\

\ define special registers                         sfp 29/01/90

-1 CONSTANT I-REG               -2 CONSTANT R-REG
-3 CONSTANT AF-REG              -4 CONSTANT AF'-REG
-5 CONSTANT (BC)-REG            -6 CONSTANT (DE)-REG
-7 CONSTANT (SP)-REG

\

\ variables                                        sfp 29/01/90

\ -- addr
  VARIABLE M1   ( addressing mode of first operand )
  VARIABLE R1   ( register used by first operand )
  VARIABLE M2   ( addressing mode of second/only operand )
  VARIABLE R2   ( register used by second/only operand )
  VARIABLE CC-SET       ( condition set flag )
  VARIABLE C-CODE       ( condition used )
  VARIABLE (C)-MODE     ( [c] for in & out instructions )

\

\ reset words                                      sfp 29/01/90

: RESETCC         \ -- ; used to reset assembler variables
  0 M1 !   0 R1 !   0 M2 !   0 R2 !
  0 CC-SET !   0 C-CODE !   0 (C)-MODE !  ;

\ Define addressing modes                          sfp 18/05/87
.( Compiling addressing modes ) CR
( mode register -- ; -- )
: OP1           CREATE  , ,  DOES> DUP  @ R1 !  CELL+ @ M1 !  ;
: OP2           CREATE  , ,  DOES> DUP  @ R2 !  CELL+ @ M2 !  ;

( Eight bit registers )
8BR B-REG OP1 B,                8BR B-REG OP2 B
( 8BR C-REG OP1 C,                8BR C-REG OP2 C  )
8BR D-REG OP1 D,                8BR D-REG OP2 D
8BR E-REG OP1 E,                8BR E-REG OP2 E
8BR H-REG OP1 H,                8BR H-REG OP2 H
8BR L-REG OP1 L,                8BR L-REG OP2 L
8BR (HL)-REG OP1 (HL),          8BR (HL)-REG OP2 (HL)
8BR A-REG OP1 A,                8BR A-REG OP2 A
8BR (HL)-REG OP1 S,             \
\ 16 bit register addressing / indexed addressing
HEX

16BR BC-REG OP1 BC,             16BR BC-REG OP2 BC
16BR DE-REG OP1 DE,             16BR DE-REG OP2 DE
16BR HL-REG OP1 HL,             16BR HL-REG OP2 HL
16BR SP-REG OP1 SP,             16BR SP-REG OP2 SP
16BR IX-REG OP1 IX,             16BR IX-REG OP2 IX
16BR IY-REG OP1 IY,             16BR IY-REG OP2 IY
16BR AF-REG OP1 AF,             16BR AF'-REG OP2 AF'
                                16BR AF-REG OP2 AF
( indexed addressing )

IND-MODE IX-REG OP1 (IX),       IND-MODE IX-REG OP2 (IX)
IND-MODE IY-REG OP1 (IY),       IND-MODE IY-REG OP2 (IY)
\
\ Special cases                                    sfp 15/04/87

SP-IND (BC)-REG OP1 (BC),       SP-IND (BC)-REG OP2 (BC)
SP-IND (DE)-REG OP1 (DE),       SP-IND (DE)-REG OP2 (DE)
SP-IND (SP)-REG OP1 (SP),       SP-IND (SP)-REG OP2 (SP)
\ checks
( -- ; error checking, error if not so )
: ?SYNTAX       0= IF 021 THROW THEN ;
: ?SYNTAX-DEST  0= IF 02D THROW THEN ;
: ?SYNTAX-SRC   0= IF 02C THROW THEN ;
( -- t/f )
: 8R1?          M1 @ 8BR =  ;
: 8R2?          M2 @ 8BR =  ;
: 16R1?         M1 @ 16BR =  ;
: 16R2?         M2 @ 16BR =  ;
: ACC1?         8R1?  R1 @ A-REG =  AND  ;
: ACC2?         8R2?  R2 @ A-REG =  AND  ;
: DIR1?         M1 @  R1 @  OR 0=  ;
: DIR2?         M2 @  R2 @  OR 0=  ;
: #?            M2 @ IMM-MODE =  ;      \
\ index mode detection
( -- t/f )
: IND1?         M1 @ IND-MODE =  ;
: IND2?         M2 @ IND-MODE =  ;
: XY?           @ DUP  IX-REG =  SWAP IY-REG =  OR  ;
: XY1?          16R1?  R1 XY?  AND  ;
: XY2?          16R2?  R2 XY?  AND  ;
: NOT-XY        XY? IF 022 THROW THEN  ;
: (HL)?         @ (HL)-REG =  ;
: (HL)2?        8R2?  R2 (HL)?  AND  ;
: HL?           @ HL-REG =  ;
( Detect HL,rp modes as these are - sort of - special cases )
: HL,RP?        16R1?  R1 HL?  AND  16R2? AND  ;
( n -- ; check index range and embed it )
: INDEX,        DUP -080 07F WITHIN 0= IF 024 THROW THEN 
                TC,  ;
\
\ ?ixy, ?[ixy], ?index, ..............          sfpsfp 13/10/87

: ?IXY, ( addr -- )
        DUP  XY?                        ( addr t/f -- )
        IF  DUP @ TC,  HL-REG SWAP !  ( lay prefix, subst. HL)
        ELSE  DROP  THEN  ;            ( do nothing )

: ?(IXY),       ( addr -- )
        DUP  XY?
        IF  DUP @ TC,  (HL)-REG SWAP !
        ELSE  DROP  THEN  ;

: ?INDEX,       ( [ disp ] addr -- )
        @ IND-MODE = IF  INDEX,  THEN  ;
\

\ <<3  <<4  op+rp,  op+r8,  prefix, ?warn ....  sfp sept 85
( n -- n<< )
: <<3           3 LSHIFT  ;
: <<4           4 LSHIFT  ;

( opcode addr-of-R1/R2 -- ; adds register pair and embeds it )
: OP+RP,        @ <<4 + TC,  ;
: OP+R8,        @ <<3 + TC,  ;

( -- )
: PREFIX,       0ED TC,  ;
: EXTEND,       0CB TC,  ;

( t/f n -- )
: ?WARN         SWAP IF  CR THERE U. ( MESSAGE CR )
                     ELSE  DROP  THEN  ;       \
\ 3modes tri-mode ..........................       sfp 15/04/87
( ... opcode -- t/f ; true returned if one of three modes )
: 3MODES        ACC1?                   ( A, check )
                IF  #?                  ( A, # check )
                    IF  046 + TC,  TC,  1
                    ELSE  8R2? IND2? OR ( A, R or [ixy] check )
                          IF  R2 ?(IXY),  R2 @ + TC,
                              M2 ?INDEX,  1
                          ELSE DROP 0
                          THEN
                    THEN
                ELSE  DROP 0  THEN  ;

( n -- ; ... -- ; used for AND CP OR SUB XOR  )
: TRI-MODE      CREATE  ,  DOES> @ 3MODES ?SYNTAX  RESETCC  ;

.( No operand operations ) CR

: 1MI   CREATE  C, DOES> C@ C, RESETCC ;
: 2MI   CREATE  PREFIX, C, DOES> C@ C, RESETCC ;

00 1MI NOP,    76 1MI HALT,    F3 1MI DI,      0FB 1MI EI,
07 1MI RLCA,   0F 1MI RRCA,    17 1MI RLA,     1F 1MI RRA,
E9 1MI JPHL,   F9 1MI LDSPHL,  E3 1MI EXSPHL,  0EB 1MI EXDEHL
27 1MI DAA,    2F 1MI CPL,     37 1MI SCF,     3F 1MI CCF,
08 1MI EXAFAF',   D9 1MI EXX,

0A9 2MI CPD,    0B9 2MI CPDR,   0A1 2MI CPI,
0B1 2MI CPIR,   0AA 2MI IND,    0BA 2MI INDR,   0A2 2MI INI,
0B2 2MI INIR,   0A8 2MI LDD,
0B8 2MI LDDR,   0A0 2MI LDI,    0B0 2MI LDIR,   044 2MI NEG,
0AB 2MI OUTD,   0BB 2MI OTDR,   0A3 2MI OUTI,   0B3 2MI OTIR,
04D 2MI RETI,   045 2MI RETN,
06F 2MI RLD,    067 2MI RRD,

\ adc ......................................... sfpsfp 15/04/87


( ... -- ; ADC ADD SBC are special cases )
: ADC,           088 3MODES 0=
                IF  HL,RP? ?SYNTAX  R2 NOT-XY
                    PREFIX,  04A R2 OP+RP,
                THEN
                RESETCC  ;
\

\ add - sufficiently special case
: ADD,           080 3MODES 0=  (  0 if not 1 of 3 main modes )
                IF  HL,RP?                     ( HL, rp check )
                    IF  R2 NOT-XY       ( xy not allowed here )
                        09 R2 OP+RP,
                    ELSE  XY1? ?SYNTAX
                          16R2? ?SYNTAX    ( XY, rp only )
                          R2 HL? 0= ?SYNTAX ( HL invalid )
                          R2 XY?           ( both index regs? )
                          IF  R1 @ R2 @ = ?SYNTAX
                              HL-REG R2 !
                          THEN
                          R1 @ TC,  09 R2 OP+RP,
                     THEN
                THEN  RESETCC  ; \
\ sbc .....................................

: SBC,           098 3MODES 0=
                IF  HL,RP? ?SYNTAX  R2 NOT-XY
                    PREFIX,  042 R2 OP+RP,
                THEN
                RESETCC  ;


\ bit-ins ...................................      sfp 15/04/87

: BIT-INS       ( e.g. b 5 [ixy] bit )
        CREATE  ,  DOES> @ >R  8R2? IND2? OR ?SYNTAX
        R2 ?(IXY),                               ( lay prefix )
        EXTEND,                            ( extension opcode )
        M2 ?INDEX,                               ( ix,iy disp )
        DUP 0 7 WITHIN 0= 023 ?SYNTAX         ( bit in range? )
        <<3                                ( align bit number )
        R2 @ + R> + TC,  RESETCC  ;    ( combine reg + opcode )

\

\ Conditionals - indicators                        sfp 15/04/87

: CONDITION
                CREATE  ,  DOES> @ C-CODE !  1 CC-SET !  ;
( -- )
: C-TOGGLE      CC-SET @ IF  C-CODE @ 1 XOR C-CODE !  THEN  ;

0 CONDITION NZ,         1 CONDITION Z,
2 CONDITION NC,         2 CONDITION NCY,
3 CONDITION CY,
4 CONDITION PO,         5 CONDITION PE,
6 CONDITION P,          7 CONDITION M,

\ +cond call ret ...........................
HEX

: +COND,        ( opcode -- )
        CC-SET @
        IF  C-CODE @ <<3  ELSE  9  THEN
        + TC,  ;

: CALL,
        0C4 +COND, ABSOLUTE RESETCC  ;

: RET,
        0C0 +COND, RESETCC  ;

\ inc/dec ....................................     sfp 18/05/87

( -- ; -- )
: INC/DEC
        CREATE  , ,  DOES>
        8R2? IND2? OR                  ( indexed or 8bit reg? )
        IF  R2 ?(IXY),  CELL+ @ R2 OP+R8,  M2 ?INDEX,
        ELSE  16R2? ?SYNTAX
              R2 ?IXY,  @ R2 OP+RP,
        THEN
        RESETCC  ;

\

\ djnz im ..................................

: DJNZ,  010 TC, REL8 RESETCC  ;

: IM,    PREFIX,
        CASE
          0     OF  046 TC,   ENDOF
          1     OF  056 TC,   ENDOF
          2     OF  05E TC,   ENDOF
                026 THROW
        ENDCASE
        RESETCC  ;

\ ex ......................................        sfp 12/08/87
: EX,    16R1? 16R2? AND         ( check both ops 16-bit reg )
        IF  R1 @ AF-REG =  R2 @ AF'-REG = AND
            IF  08 TC,
            ELSE  R1 @ DE-REG =  R2 HL?  AND
                  R1 HL?  R2 @ DE-REG =  AND  OR
                  0= IF 025 THROW THEN   0EB TC,
            THEN
        ELSE  M1 @ SP-IND =  R1 @ (SP)-REG =  AND  ?SYNTAX
              16R2? ?SYNTAX
              R2 ?IXY,  R2 HL? ?SYNTAX  0E3 TC,
        THEN
        RESETCC  ;
\ in out .....................................     sfp 18/04/87
: PORT, DUP  0 0FF WITHIN 0= 027 ?SYNTAX  TC,  ;

: (C)   1 (C)-MODE !  ;
: (C),  (C)  ;
: IN,    ACC1? DIR2? AND  (C)-MODE @ 0= AND
        IF  0DB TC, PORT,                          ( a, port )
        ELSE  8R1? (C)-MODE @  AND  ?SYNTAX         ( reg, [c] )
              PREFIX,  040 R1 OP+R8,
        THEN  RESETCC  ;
: OUT,   DIR1? ACC2? AND  (C)-MODE @ 0= AND
        IF  0D3 TC, PORT,     ( port , a )
        ELSE  8R2? (C)-MODE @  AND  ?SYNTAX   (HL)2? 02A ?WARN
              PREFIX,  041 R2 OP+R8,
        THEN  RESETCC  ;
\ jp jr ......................................
: JP,    DIR2?
        IF  CC-SET @
            IF  0C2 C-CODE @ <<3 +  ELSE  0C3  THEN
            TC,  ABSOLUTE
        ELSE  (HL)2? IND2? OR ?SYNTAX
              R2 ?(IXY), 0E9 TC,
        THEN
        RESETCC  ;

: JR,    CC-SET @
        IF  C-CODE @ DUP  3 > IF 028 THROW THEN  <<3 020 +
        ELSE  018 THEN
        TC, REL8  RESETCC  ;
\
\ ld - screen 1 ...........................

: LD,    CASE
        #?      ?OF  CASE  M1 @
                     8BR   OF  06 R1 OP+R8, TC,  ENDOF
                     16BR  OF  R1 ?IXY,
                               01 R1 OP+RP,  ABSOLUTE  ENDOF
                     IND-MODE
                           OF  R1 ?(IXY),  036 TC,
                               SWAP  INDEX,  TC,  ENDOF
                           0 ?SYNTAX
                     ENDCASE
                ENDOF



\ ld - screen 2 ...........................        sfp 15/04/87
        DIR2?   ?OF  ACC1?
                     IF  03A TC, ABSOLUTE     ( a, nnnn )
                     ELSE  16R1? ?SYNTAX   ( rp, nnn )
                           R1 ?IXY,  R1 HL?
                           IF  02A TC,
                           ELSE  PREFIX,  04B R1 OP+RP,  THEN
                           ABSOLUTE
                     THEN      ENDOF

        8R1?  8R2? IND2? OR  AND        ( r, r or r, d [xy] )
                ?OF  R1 (HL)?  IND2? AND  02A ?WARN
                     R2 ?(IXY), 040 R2 @ + R1 OP+R8,  M2 ?INDEX,
                ENDOF


\ ld - screen 3 ...............................

        DIR1?   ?OF  ACC2?
                     IF  032 TC, ABSOLUTE     ( nnnn , a )
                     ELSE  16R2? ?SYNTAX
                           R2 ?IXY,  R2 HL?
                           IF  022 TC,
                           ELSE  PREFIX,  043 R2 OP+RP,  THEN
                           ABSOLUTE
                     THEN      ENDOF

        IND1? 8R2? AND                  ( d [ixy], r )
                ?OF  (HL)2? 02A ?WARN
                     R1 ?(IXY),  070 R2 @ + TC,  INDEX,  ENDOF

\
\ ld - screen 4 ...........................

        16R1? 16R2? AND                 ( sp, hl or sp, ixy )
                ?OF  R1 @ SP-REG = ?SYNTAX
                     R2 ?IXY,  R2 HL? ?SYNTAX
                     0F9 TC,          ENDOF

        ACC1?   ?OF  CASE  M2 @
                       SPEC-REG  OF  PREFIX,  R2 @ I-REG =
                                     IF  057  ELSE  05F  THEN
                                     TC,      ENDOF
                       SP-IND    OF  R2 @ (BC)-REG =
                                     IF  0A  ELSE  01A  THEN
                                     TC,      ENDOF
                                 0 ?SYNTAX
                      ENDCASE           ENDOF           \
\ ld - screen 5 ...........................

        ACC2?   ?OF  CASE  M1 @
                     SPEC-REG   OF  PREFIX,  R1 @ I-REG =
                                    IF  047  ELSE  04F  THEN
                                    TC,       ENDOF
                     SP-IND     OF  R1 @ (BC)-REG =
                                    IF  02  ELSE  012  THEN
                                    TC,       ENDOF
                                0 ?SYNTAX
                     ENDCASE            ENDOF
                0 ?SYNTAX  1
        ENDCASE
        RESETCC  ;

\ pop/push ...................................     sfp 15/04/87

: POP/PUSH
        CREATE  ,  DOES> @
        16R2? ?SYNTAX                       ( 16-bit regs only)
        R2 @  DUP SP-REG = 0= ?SYNTAX       ( not stack point!)
        AF-REG = IF SP-REG R2 ! THEN       ( take AF instead )
        R2 ?IXY,  R2 OP+RP,  RESETCC  ;       ( check xy, add op)

\

\ shift rst ..................................     sfp 15/04/87

: SHIFT         CREATE  ,  DOES> @ >R
                8R2? IND2? OR ?SYNTAX
                R2 ?(IXY),  EXTEND,  M2 ?INDEX,
                R> R2 @ + TC,
                RESETCC  ;

: RST,           DIR2? ?SYNTAX
                DUP 8 < IF  <<3  THEN
                0C7 + TC,  RESETCC  ;


\ tri-mode bit-ins                                 sfp 15/04/87

0A0 TRI-MODE AND,        0B8 TRI-MODE CP,
0B0 TRI-MODE OR,         090 TRI-MODE SUB,
0A8 TRI-MODE XOR,

040 BIT-INS BIT,         080 BIT-INS RES,
0C0 BIT-INS SET,

\ inc/dec pop/push shift

05 0B INC/DEC DEC,       04 03 INC/DEC INC,

0C1 POP/PUSH POP,        0C5 POP/PUSH PUSH,

010 SHIFT RL,    000 SHIFT RLC,   020 SHIFT SLA,   018 SHIFT RR,
008 SHIFT RRC,   028 SHIFT SRA,   038 SHIFT SRL,

\ special addressing modes, or conflict with Forth sfp 15/04/87

8BR C-REG OP1 C,                8BR C-REG OP2 C

SPEC-REG I-REG OP1 I,           SPEC-REG I-REG OP2 I
SPEC-REG R-REG OP1 R,           SPEC-REG R-REG OP2 R

IMM-MODE 0 OP2 #        ( immediate operand )


( fig-FORTH 8080 Assembler with Z80 extensions       SCR 6 of 6)

: ?PAIRS  <> -22 THROW ;
: ENDIF,   2 ?PAIRS HERE SWAP ! ;
: THEN,    POSTPONE ENDIF, ;
: IF,      C-TOGGLE JP, HERE 0 , 2 ;
: ELSE,    2 ?PAIRS IF, ROT SWAP ENDIF, 2 ;
: BEGIN,   HERE 1 ;
: UNTIL,   1 ?PAIRS C-TOGGLE JP, , ;
: AGAIN,   1 ?PAIRS C3 C, , ;
: WHILE,   IF, 2 + ;
: REPEAT,  >R >R AGAIN, R> R> 2 - ENDIF, ;
FORTH DEFINITIONS DECIMAL
