# CamelForth Bootrom 

## Assembler interface

Main words:

  CODE <name>   Begin a code definition

  LABEL <name>  As for CODE but instead of <name> executing the code
                sequence, it returns the execution address (xt).

  ;CODE         Ends a code definition


## Instruction format

As with most forth assemblers, operands precede the instruction.
Intel mnemonics are employed for the 8080 instructions while the
Z80-only instructions use TDL "extended Intel" mnemonics.

The following examples show DX-Forth assembler syntax as compared
with conventional Zilog notation.  Refer to the instruction reference
later for a complete listing.

|     Zilog           |       DX-Forth   |
|----------------------------------------|
|     LD   B,C        |       C  B  MOV  |
|     LD   A,5        |       5  A  MVI  |
|     INC  A          |          A  INR  |
|     DEC  (HL)       |          M  DCR  |
|     INC  (IX+9)     |       9 (X) INR  |
|     INC  (IY-6)     |      -6 (Y) INR  |
|     BIT  3,C        |       C  3  BIT  |
|     RES  2,(HL)     |       M  2  RES  |
|     SET  7,(IX-5)   |   -5 (X) 7  SET  |


3. Register usage

Code words may use any cpu register except:

    BC           Top of forth stack
    DE
    IX           Return stack pointer
    IY           User space pointer

If any of these registers are to be used in a code definition for
other purposes, their contents must be saved beforehand and restored
afterwards.


## Structured conditionals

Structured conditionals are an alternative or adjunct to local labels.
They include:

IF  ELSE  THEN  BEGIN  WHILE  REPEAT  UNTIL  AGAIN  AHEAD

Conditionals that perform a test i.e. IF  WHILE  UNTIL  must be
preceeded by one of the following condition flags:

U>=  U<  0<>  0=  U>  U<=  CY  NC  OV  NO  PO  PE  NEVER

NEVER is used before a conditional test to create an unconditional
jump.  E.g. AHEAD and AGAIN are macros for NEVER IF and NEVER UNTIL
respectively.

Example

    CODE 0= ( n -- flag )
      0 H LXI
      D POP
      D A MOV
      E ORA
    0= IF
      H DCX
    THEN
    NEXT,
    ;CODE



## Miscellaneous tools

When machine language is used extensively there can be a need for
tools found in conventional assemblers.  Below are several the author
has found useful.  They are not resident in the forth assembler but
defined as needed.

  \ Adjust HERE to an even address padding with a NOP instruction
  : EVEN ( -- )  HERE 1 AND IF $00 C, THEN ;

  \ Name value x
  : EQU ( x "name" -- )  SYS @ TUCK 0= SYS ! VALUE SYS ! ;

  \ Name address at HERE and compile a 16-bit value
  : DW ( 16b "name" -- )  HERE EQU , ;

  \ Name address at HERE and compile a 8-bit value
  : DB ( 8b "name" -- )  HERE EQU C, ;


## Instruction Reference

```
The following listing shows each Zilog instruction and its
Intel/TDL equivalent in Forth format.

b   bit position (0..7)
d   displacement added to IX or IY registers (-127..127)
r   register A,B,C,D,E,H or L
n   8 bit operand (0..255)
nn  16 bit operand (0..65535)

*  indicates Z80 only instructions
** indicates 8085 only instructions


  Zilog              Intel/TDL         Zilog              Intel/TDL

  ADC  A,(HL)           M  ADC         LD  BC,nn          nn B  LXI
* ADC  A,(IX+d)     d (X)  ADC       * LD  DE,(nn)          nn  LDED
* ADC  A,(IY+d)     d (Y)  ADC         LD  DE,nn          nn D  LXI
  ADC  A,n              n  ACI         LD  HL,(nn)          nn  LHLD
  ADC  A,r              r  ADC         LD  HL,nn          nn H  LXI
* ADC  HL,BC            B  DADC      * LD  I,A                  STAI
* ADC  HL,DE            D  DADC      * LD  IX,(nn)          nn  LIXD
* ADC  HL,HL            H  DADC      * LD  IX,nn          nn X  LXI
* ADC  HL,SP           SP  DADC      * LD  IY,(nn)          nn  LIYD
  ADD  A,(HL)           M  ADD       * LD  IY,nn          nn Y  LXI
* ADD  A,(IX+d)     d (X)  ADD       * LD  R,A                  STAR
* ADD  A,(IY+d)     d (Y)  ADD       * LD  SP,(nn)          nn  LSPD
  ADD  A,n              n  ADI         LD  SP,HL                SPHL
  ADD  A,r              r  ADD       * LD  SP,IX                SPIX
  ADD  HL,BC            B  DAD       * LD  SP,IY                SPIY
  ADD  HL,DE            D  DAD         LD  SP,nn         nn SP  LXI
  ADD  HL,HL            H  DAD         LD  r,(HL)          M r  MOV
  ADD  HL,SP           SP  DAD       * LD  r,(IX+d)    d (X) r  MOV
* ADD  IX,BC            B  DADX      * LD  r,(IY+d)    d (Y) r  MOV
* ADD  IX,DE            D  DADX        LD  r,n             n r  MVI
* ADD  IX,IX            X  DADX        LD  r,r'           r' r  MOV
* ADD  IX,SP           SP  DADX      * LDD                      LDD
* ADD  IY,BC            B  DADY      * LDDR                     LDDR
* ADD  IY,DE            D  DADY      * LDI                      LDI
* ADD  IY,IY            Y  DADY      * LDIR                     LDIR
* ADD  IY,SP           SP  DADY      * NEG                      NEG
  AND  (HL)             M  ANA         NOP                      NOP
* AND  (IX+d)       d (X)  ANA         OR  (HL)              M  ORA
* AND  (IY+d)       d (Y)  ANA       * OR  (IX+d)        d (X)  ORA
  AND  n                n  ANI       * OR  (IY+d)        d (Y)  ORA
  AND  r                r  ANA         OR  n                 n  ORI
* BIT  b,(HL)         M b  BIT         OR  r                 r  ORA
* BIT  b,(IX+d)   d (X) b  BIT       * OTDR                     OUTDR
* BIT  b,(IY+d)   d (Y) b  BIT       * OTIR                     OUTIR
* BIT  b,r            r b  BIT       * OUT  (C),r            r  OUTP
  CALL  C,nn           nn  CC          OUT  (n),A            n  OUT
  CALL  M,nn           nn  CM        * OUTD                     OUTD
  CALL  NC,nn          nn  CNC       * OUTI                     OUTI
  CALL  NZ,nn          nn  CNZ         POP  AF             PSW  POP
  CALL  P,nn           nn  CP          POP  BC               B  POP
  CALL  PE,nn          nn  CPE         POP  DE               D  POP
  CALL  PO,nn          nn  CPO         POP  HL               H  POP
  CALL  Z,nn           nn  CZ        * POP  IX               X  POP
  CALL  nn             nn  CALL      * POP  IY               Y  POP
  CCF                      CMC         PUSH  AF            PSW  PUSH
  CP  (HL)              M  CMP         PUSH  BC              B  PUSH
* CP  (IX+d)        d (X)  CMP         PUSH  DE              D  PUSH
* CP  (IY+d)        d (Y)  CMP         PUSH  HL              H  PUSH
  CP  n                 n  CPI       * PUSH  IX              X  PUSH
  CP  r                 r  CMP       * PUSH  IY              Y  PUSH
* CPD                      CCD       * RES  b,(HL)         M b  RES
* CPDR                     CCDR      * RES  b,(IX+d)   d (X) b  RES
* CPI                      CCI       * RES  b,(IY+d)   d (Y) b  RES
* CPIR                     CCIR      * RES  b,r            r b  RES
  CPL                      CMA         RET                      RET
  DAA                      DAA         RET  C                   RC
  DEC  (HL)             M  DCR         RET  M                   RM
* DEC  (IX+d)       d (X)  DCR         RET  NC                  RNC
* DEC  (IY+d)       d (Y)  DCR         RET  NZ                  RNZ
  DEC  BC               B  DCX         RET  P                   RP
  DEC  DE               D  DCX         RET  PE                  RPE
  DEC  HL               H  DCX         RET  PO                  RPO
* DEC  IX               X  DCX         RET  Z                   RZ
* DEC  IY               Y  DCX       * RETI                     RETI
  DEC  SP              SP  DCX       * RETN                     RETN
  DEC  r                r  DCR       * RL  (HL)              M  RALR
  DI                       DI        * RL  (IX+d)        d (X)  RALR
* DJNZ  nn             nn  DJNZ      * RL  (IY+d)        d (Y)  RALR
  EI                       EI        * RL  r                 r  RALR
  EX  (SP),HL              XTHL        RLA                      RAL
* EX  (SP),IX              XTIX      * RLC  (HL)             M  RLCR
* EX  (SP),IY              XTIY      * RLC  (IX+d)       d (X)  RLCR
* EX  AF,AF'               EXAF      * RLC  (IY+d)       d (Y)  RLCR
  EX  DE,HL                XCHG      * RLC  r                r  RLCR
* EXX                      EXX         RLCA                     RLC
  HALT                     HLT       * RLD                      RLD
* IM  0                    IM0       * RR  (HL)              M  RARR
* IM  1                    IM1       * RR  (IX+d)        d (X)  RARR
* IM  2                    IM2       * RR  (IY+d)        d (Y)  RARR
  IN  A,(n)             n  IN        * RR  r                 r  RARR
* IN  r,(C)             r  INP         RRA                      RAR
  INC  (HL)             M  INR       * RRC  (HL)             M  RRCR
* INC  (IX+d)       d (X)  INR       * RRC  (IX+d)       d (X)  RRCR
* INC  (IY+d)       d (Y)  INR       * RRC  (IY+d)       d (Y)  RRCR
  INC  BC               B  INX       * RRC  r                r  RRCR
  INC  DE               D  INX         RRCA                     RRC
  INC  HL               H  INX       * RRD                      RRD
* INC  IX               X  INX         RST  00h              0  RST
* INC  IY               Y  INX         RST  08h              1  RST
  INC  SP              SP  INX         RST  10h              2  RST
  INC  r                r  INR         RST  18h              3  RST
* IND                      IND         RST  20h              4  RST
* INDR                     INDR        RST  28h              5  RST
* INI                      INI         RST  30h              6  RST
* INIR                     INIR        RST  38h              7  RST
  JP  (HL)                 PCHL        SBC  A,(HL)           M  SBB
* JP  (IX)                 PCIX      * SBC  A,(IX+d)     d (X)  SBB
* JP  (IY)                 PCIY      * SBC  A,(IY+d)     d (Y)  SBB
  JP  C,nn             nn  JC          SBC  A,n              n  SBI
  JP  M,nn             nn  JM          SBC  A,r              r  SBB
  JP  NC,nn            nn  JNC       * SBC  HL,BC            B  DSBC
  JP  NZ,nn            nn  JNZ       * SBC  HL,DE            D  DSBC
  JP  P,nn             nn  JP        * SBC  HL,HL            H  DSBC
  JP  PE,nn            nn  JPE       * SBC  HL,SP           SP  DSBC
  JP  PO,nn            nn  JPO         SCF                      STC
  JP  Z,nn             nn  JZ        * SET  b,(HL)         M b  SET
  JP  nn               nn  JMP       * SET  b,(IX+d)   d (X) b  SET
* JR  C,nn             nn  JRC       * SET  b,(IY+d)   d (Y) b  SET
* JR  NC,nn            nn  JRNC      * SET  b,r            r b  SET
* JR  NZ,nn            nn  JRNZ      * SLA  (HL)             M  SLAR
* JR  Z,nn             nn  JRZ       * SLA  (IX+d)       d (X)  SLAR
* JR  nn               nn  JMPR      * SLA  (IY+d)       d (Y)  SLAR
  LD  (BC),A            B  STAX      * SLA  r                r  SLAR
  LD  (DE),A            D  STAX      * SRA  (HL)             M  SRAR
  LD  (HL),n          n M  MVI       * SRA  (IX+d)       d (X)  SRAR
  LD  (HL),r          r M  MOV       * SRA  (IY+d)       d (Y)  SRAR
* LD  (IX+d),n    n d (X)  MVI       * SRA  r                r  SRAR
* LD  (IX+d),r    r d (X)  MOV       * SRL  (HL)             M  SRLR
* LD  (IY+d),n    n d (Y)  MVI       * SRL  (IX+d)       d (X)  SRLR
* LD  (IY+d),r    r d (Y)  MOV       * SRL  (IY+d)       d (Y)  SRLR
  LD  (nn),A           nn  STA       * SRL  r                r  SRLR
* LD  (nn),BC          nn  SBCD        SUB  (HL)             M  SUB
* LD  (nn),DE          nn  SDED      * SUB  (IX+d)       d (X)  SUB
  LD  (nn),HL          nn  SHLD      * SUB  (IY+d)       d (Y)  SUB
* LD  (nn),IX          nn  SIXD        SUB  n                n  SUI
* LD  (nn),IY          nn  SIYD        SUB  r                r  SUB
* LD  (nn),SP          nn  SSPD        XOR  (HL)             M  XRA
  LD  A,(BC)            B  LDAX      * XOR  (IX+d)       d (X)  XRA
  LD  A,(DE)            D  LDAX      * XOR  (IY+d)       d (Y)  XRA
  LD  A,(nn)           nn  LDA         XOR  n                n  XRI
* LD  A,I                  LDAI        XOR  r                r  XRA
* LD  A,R                  LDAR     ** n/a                      RIM
* LD  BC,(nn)          nn  LBCD     ** n/a                      SIM
```
