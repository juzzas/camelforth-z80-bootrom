\ Z80 assembler, based on TDL mnemonics

\ 8080/85/Z80 assembler with local labels
\ Based on J. Cassady 8080 forth assembler

\ Uses TDL extended INTEL mnemonics

FORTH DEFINITIONS  HEX

VOCABULARY ASSEMBLER  ALSO ASSEMBLER DEFINITIONS
CR .( loading Z80 Assembler )
1 11 +THRU

PREVIOUS DEFINITIONS  DECIMAL


\ modes
VARIABLE <xy>  ( ix/iy mode )
VARIABLE dsp   ( ix/iy displacement )

: ?xy   <xy> @ 1 = IF  -1 ALLOT  THEN FALSE <xy> ! ;
: ?dsp  <xy> @ 2 = IF  dsp @ C,  THEN FALSE <xy> ! ;

: rel8 ( a1 a2 -- offs )
  1+ - DUP 80 -80 WITHIN ABORT" BRANCH OUT OF RANGE" ;









 .( 8080 )
:  1m  CONSTANT DOES> C@ C, ;
:  2m  CONSTANT DOES> C@ + C, ?dsp ;
:  3m  CONSTANT DOES> C@ SWAP 8 * + C, ?dsp ;
:  4m  CONSTANT DOES> C@ C, C, ;
:  5m  CONSTANT DOES> C@ C, , ;

: MOV  8 * 40 + + C, ?dsp ;
: MVI  8 * 6 + C, ?dsp C, ;
: LXI  8 * 1+ C, , ;






\ 8080 mnemonics
000 1m NOP   007 1m RLC   00F 1m RRC   017 1m RAL   01F 1m RAR
027 1m DAA   02F 1m CMA   037 1m STC   03F 1m CMC   076 1m HLT
0C0 1m RNZ   0C8 1m RZ    0C9 1m RET   0D0 1m RNC   0D8 1m RC
0E0 1m RPO   0E3 1m XTHL  0E8 1m RPE   0E9 1m PCHL  0EB 1m XCHG
0F0 1m RP    0F3 1m DI    0F8 1m RM    0F9 1m SPHL  0FB 1m EI

080 2m ADD   088 2m ADC   090 2m SUB   098 2m SBB   0A0 2m ANA
0A8 2m XRA   0B0 2m ORA   0B8 2m CMP

002 3m STAX  003 3m INX   004 3m INR   005 3m DCR   009 3m DAD
00A 3m LDAX  00B 3m DCX   0C1 3m POP   0C5 3m PUSH  0C7 3m RST

0C6 4m ADI   0CE 4m ACI   0D3 4m OUT   0D6 4m SUI   0DB 4m IN
0DE 4m SBI   0E6 4m ANI   0EE 4m XRI   0F6 4m ORI   0FE 4m CPI

\ 8080 mnemonics
022 5m SHLD  02A 5m LHLD  032 5m STA   03A 5m LDA   0C2 5m JNZ
0C3 5m JMP   0C4 5m CNZ   0CA 5m JZ    0CC 5m CZ    0CD 5m CALL
0D2 5m JNC   0D4 5m CNC   0DA 5m JC    0DC 5m CC    0E2 5m JPO
0E4 5m CPO   0EA 5m JPE   0EC 5m CPE   0F2 5m JP    0F4 5m CP
0FA 5m JM    0FC 5m CM










 .( 8085 )
020 1m RIM   030 1m SIM














 .( Z80 )
:  6m  CONSTANT DOES> C@ C, HERE rel8 C, ;
:  7m  CONSTANT DOES> 0ED C, C@ C, ;
:  8m  CONSTANT DOES> 0ED C, C@ C, , ;
:  9m  CONSTANT DOES> 0CB C, ?dsp C@ + C, ;
: 10m  CONSTANT DOES> 0CB C, ?dsp C@ SWAP 8 * + + C, ;
: 11m  VARIABLE DOES> @ , ;
: 12m  VARIABLE DOES> @ , , ;
: 13m  VARIABLE DOES> COUNT C, ?xy C@ SWAP 8 * + C, ;







\ Z80 mnemonics
008 1m EXAF  0D9 1m EXX

010 6m DJNZ  018 6m JMPR  020 6m JRNZ  028 6m JRZ   030 6m JRNC
038 6m JRC

044 7m NEG   045 7m RETN  046 7m IM0   047 7m STAI  04D 7m RETI
04F 7m STAR  056 7m IM1   057 7m LDAI  05E 7m IM2   05F 7m LDAR
067 7m RRD   06F 7m RLD   0A0 7m LDI   0A1 7m CCI   0A2 7m INI
0A3 7m OUTI  0A8 7m LDD   0A9 7m CCD   0AA 7m IND   0AB 7m OUTD
0B0 7m LDIR  0B1 7m CCIR  0B2 7m INIR  0B3 7m OUTIR 0B8 7m LDDR
0B9 7m CCDR  0BA 7m INDR  0BB 7m OUTDR

043 8m SBCD  04B 8m LBCD  053 8m SDED  05B 8m LDED  073 8m SSPD
07B 8m LSPD

\ Z80 mnemonics
000 9m RLCR  008 9m RRCR  010 9m RALR  018 9m RARR  020 9m SLAR
028 9m SRAR  038 9m SRLR

040 10m BIT   080 10m RES   0C0 10m SET

0E3DD 11m XTIX  0E9DD 11m PCIX  0F9DD 11m SPIX  0E3FD 11m XTIY
0E9FD 11m PCIY  0F9FD 11m SPIY

022DD 12m SIXD  022FD 12m SIYD  02ADD 12m LIXD  02AFD 12m LIYD

009DD 13m DADX  009FD 13m DADY  040ED 13m INP   041ED 13m OUTP
042ED 13m DSBC  04AED 13m DADC



\ Z80 registers
: X ( -- 4 )  0DD C,  1 <xy> !  4 ;
: Y ( -- 4 )  0FD C,  1 <xy> !  4 ;

: (X) ( n -- 6 )  dsp !  0DD C,  2 <xy> !  6 ;
: (Y) ( n -- 6 )  dsp !  0FD C,  2 <xy> !  6 ;










\ registers
0 CONSTANT B    1 CONSTANT C    2 CONSTANT D    3 CONSTANT E
4 CONSTANT H    5 CONSTANT L    6 CONSTANT M    6 CONSTANT PSW
6 CONSTANT SP   7 CONSTANT A












