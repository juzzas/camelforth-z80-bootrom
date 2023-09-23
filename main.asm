; MIT License
;
; Copyright (c) 2023 Justin Skists
;
; Permission is hereby granted, free of charge, to any person obtaining a copy
; of this software and associated documentation files (the "Software"), to deal
; in the Software without restriction, including without limitation the rights
; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
; copies of the Software, and to permit persons to whom the Software is
; furnished to do so, subject to the following conditions:
;
; The above copyright notice and this permission notice shall be included in all
; copies or substantial portions of the Software.
;
; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
; SOFTWARE.


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
    DEFM "RC2014 - CamelForth BootROM - DEV", 13, 10
    DEFM "Ported to RC2014 ROM by Justin Skists", 13, 10, 0

