( fig-FORTH 8080 Assembler with Z80 extensions       SCR 0 of 7)
HEX VOCABULARY ASSEMBLER IMMEDIATE
ALSO ASSEMBLER DEFINITIONS

1 7 +THRU

.( Assembler loaded )









( fig-FORTH 8080 Assembler with Z80 extensions       SCR 1 of 7)
VARIABLE CONDITIONAL?
VARIABLE CONDITIONAL

: RESETCC   0 CONDITIONAL? ! ;
: COND   CREATE  C, DOES> C@ CONDITIONAL !  -1 CONDITIONAL? ! ;

0 COND NZ,   1 COND Z,   2 COND NC,   3 COND CS,
4 COND PO,   5 COND PE,  6 COND P,    7 COND M,







( fig-FORTH 8080 Assembler with Z80 extensions       SCR 2 of 7)
4 CONSTANT H     5 CONSTANT L     7 CONSTANT A   6 CONSTANT PSW
2 CONSTANT D     3 CONSTANT E     0 CONSTANT B   1 CONSTANT C
6 CONSTANT M     6 CONSTANT (HL)  6 CONSTANT SP

: 8*     DUP + DUP + DUP + ;

: 1MI   CREATE  C,  DOES> C@ C, RESETCC ;
: 2MI   CREATE  C,  DOES> C@ + C, RESETCC ;
: 3MI   CREATE  C,  DOES> C@ SWAP 8* + C, RESETCC ;
: 4MI   CREATE  C,  DOES> C@ C, C, RESETCC ;
: 5MI   CREATE  C,  DOES> C@ C, , RESETCC ;




( fig-FORTH 8080 Assembler with Z80 extensions       SCR 3 of 7)
00 1MI NOP,   76 1MI HALT,  F3 1MI DI,    FB 1MI EI,
07 1MI RLC,   0F 1MI RRC,   17 1MI RAL,   1F 1MI RAR,
E9 1MI PCHL,  F9 1MI SPHL,  E3 1MI XTHL,  EB 1MI XCHG,
27 1MI DAA,   2F 1MI CMA,   37 1MI STC,   3F 1MI CMC,
08 1MI EXAF,  D9 1MI EXX,   C0 1MI RNZ,   C8 1MI RZ,
D0 1MI RNC,   D8 1MI RC,    E0 1MI RPO,   E8 1MI RPE,
F0 1MI RP,    F8 1MI RM,    C9 1MI RET,

80 2MI ADD,   88 2MI ADC,   90 2MI SUB,   98 2MI SBB,
A0 2MI ANA,   A8 2MI XRA,   B0 2MI ORA,   B8 2MI CMP,

09 3MI DAD,   C1 3MI POP,   C5 3MI PUSH,  02 3MI STAX,
0A 3MI LDAX,  04 3MI INR,   05 3MI DCR,   03 3MI INX,
0B 3MI DCX,   C7 3MI RST,

( fig-FORTH 8080 Assembler with Z80 extensions       SCR 4 of 7)
D3 4MI OUT,   DB 4MI IN,    C6 4MI ADI,   CE 4MI ACI, 
D6 4MI SUI,   DE 4MI SBI,   E6 4MI ANI,   EE 4MI XRI,
F6 4MI ORI,   FE 4MI CPI,

22 5MI SHLD,  2A 5MI LHLD,  32 5MI STA,   3A 5MI LDA,
C4 5MI CNZ,   CC 5MI CZ,    D4 5MI CNC,   DC 5MI CC,
E4 5MI CPO,   EC 5MI CPE,   F4 5MI CP,    FC 5MI CM,
CD 5MI CALL,  C3 5MI JMP,







( fig-FORTH 8080 Assembler with Z80 extensions       SCR 5 of 7)
: NOT,   8 + ;
: MOV,   8* 40 + + C, ;
: MVI,   8* 6 + C, C, ;
: LXI,   8* 1+ C, , ;
: PCIX,  DD C, E9 C,  ;
: PCIY,  FD C, E9 C, ;









( fig-FORTH 8080 Assembler with Z80 extensions       SCR 6 of 7)
: NOTCC ( cc -- cc' )  CONDITIONAL @ 1 XOR CONDITIONAL ! ;
: JPNOTCC    CONDITIONAL? @ IF CONDITIONAL @ NOTCC 8* C2 +
   ELSE C3 THEN RESETCC ;
: CALL,  CONDITIONAL? @ IF CONDITIONAL @ 8* C4 +
   ELSE CD THEN   C, ,  RESETCC ;
: JP,    CONDITIONAL? @ IF CONDITIONAL @ 8* C2 +
   ELSE C3 THEN   C, ,  RESETCC ;
: JR,    CONDITIONAL? @ IF CONDITIONAL @ 3 AND 8* 20 +
   ELSE 18 THEN   C, C,  RESETCC ;
: RET,   CONDITIONAL? @ IF CONDITIONAL @ 8* C0 +
   ELSE C9 THEN   C, ,  RESETCC ;




( fig-FORTH 8080 Assembler with Z80 extensions       SCR 7 of 7)

: ?PAIRS  <> -22 THROW ;
: ENDIF,   2 ?PAIRS HERE SWAP ! ;
: THEN,    POSTPONE ENDIF, ;
: IF,      JPNOTCC C, HERE 0 , 2 ;
: ELSE,    2 ?PAIRS IF, ROT SWAP ENDIF, 2 ;
: BEGIN,   HERE 1 ;
: UNTIL,   1 ?PAIRS JPNOTCC C, , ;
: AGAIN,   1 ?PAIRS C3 C, , ;
: WHILE,   IF, 2 + ;
: REPEAT,  >R >R AGAIN, R> R> 2 - ENDIF, ;
FORTH DEFINITIONS DECIMAL

