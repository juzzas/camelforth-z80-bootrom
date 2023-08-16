EXTERN main_code_start
EXTERN main_code_warmstart

PUBLIC _main

SECTION code_user

DEFC CHAR_CR = 13
DEFC CHAR_LF = 10

_main:
    ld hl, signon_msg

loop_signon:
    ld a, (hl)
    or a
    jr z, loop_exit
    rst 8
    inc hl
    jr loop_signon

loop_exit:
    jp main_code_start


SECTION rodata_user

signon_msg:
    DEFM 13, 10
    DEFM "RC2014 - CamelForth BootROM - v20230817", 13, 10, 0

