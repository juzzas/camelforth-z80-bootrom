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

EXTERN acia_init
EXTERN acia_putc
EXTERN acia_getc
EXTERN acia_pollc

EXTERN intvec_ptr
EXTERN nmivec_ptr

PUBLIC _main

SECTION code

DEFC CHAR_CR = 13
DEFC CHAR_LF = 10

;; _main arrives with interrupts disabled
_main:
    jp main_code_start


SECTION rc2014
    DEFB 127, 127, 72, 72, 72, 127, 55, 62, 127, 65, 65, 65, 99, 34, 39, 111
    DEFB 73, 73, 89, 115, 5, 62, 127, 67, 93, 97, 127, 62, 1, 33, 127, 127
    DEFB 1, 1, 28, 124, 100, 4, 127, 127, 4

