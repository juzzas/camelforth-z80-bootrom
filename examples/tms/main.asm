; TMS9918A graphics subroutines - CamelForth bindings
; Copyright
;
; Permission is hereby granted, free of charge, to any person obtaining a
; copy of this software and associated documentation files (the "Software"),
; to deal in the Software without restriction, including without limitation
; the rights to use, copy, modify, merge, publish, distribute, sublicense,
; and/or sell copies of the Software, and to permit persons to whom the
; Software is furnished to do so, subject to the following conditions:
;
; The above copyright notice and this permission notice shall be included in
; all copies or substantial portions of the Software.
;
; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
; DEALINGS IN THE SOFTWARE.

; VDP Programmer's Guide: http://map.grauw.nl/resources/video/ti-vdp-programmers-guide.pdf

; ---------------------------------------------------------------------------
; configuration parameters; can be changed at runtime

org 0xf000

; jump table

; xx00 TMSPROBE?   ( -- flag )
JT_TMSPROBE:
    jp _TmsProbe

; xx03 /TMSTEXT
JT_TMSTEXT:
    jp _TmsTextMode

; xx06 /TMSMULTICOLOR
JT_TMSMULTICOLOR:
    jp _TmsMulticolor

; xx09 /TMSBITMAP
JT_TMSBITMAP:
    jp _TmsBitmap

; f00c /TMSTILE
JT_TMSTILE:
    jp _TmsTile

; f00f TMSBG   ( color -- )
JT_TMSBG:
    jp _TmsBg

; f012 TMSFG   ( color -- )
JT_TMSFG:
    jp _TmsFg

; f015 TMSTEXT-XY  ( x y -- )
JT_TMSTEXTXY:
    jp _TmsTextXY

; f018 TMSEMIT  ( c -- )
JT_TMSEMIT:
    jp _TmsEmit

; f01b TMSPLOT-XY  ( x y -- )
JT_TMSPLOTXY:
    jp _TmsPlotXY

; f01e TMSCLR-XY  ( x y -- )
JT_TMSCLRXY:
    jp _TmsClrXY

; f021 TMSINT+  ( -- )
EXTERN TmsIntEnable
JT_TMS_INTPLUS:
    jp TmsIntEnable

; f024 TMSINT-  ( -- )
EXTERN TmsIntDisable
JT_TMS_INTMINUS:
    jp TmsIntDisable

EXTERN TmsProbe
_TmsProbe:
    call TmsProbe
    jr z, noprobe
    ld bc, -1
    rst 0x20
    ret

noprobe:
    ld bc, 0
    rst 0x20
    ret

EXTERN TmsMulticolor
_TmsMulticolor:
    jp TmsMulticolor

EXTERN TmsBitmap
_TmsBitmap:
    jp TmsBitmap


EXTERN TmsIntDisable
EXTERN TmsTextMode
_TmsTextMode:
    call TmsIntDisable
    rst 0x28
    ld  hl, bc
    jp TmsTextMode

EXTERN TmsTile
_TmsTile:
    jp TmsTile

;;  Text routines

EXTERN TmsBackground
_TmsBg:
    rst 0x28
    ld  a, c
    jp TmsBackground

EXTERN TmsTextColor
_TmsFg:
    rst 0x28
    ld  a, c
    jp TmsTextColor

EXTERN TmsTextPos
_TmsTextXY:
    rst 0x28   ;y
    ld  e, c
    rst 0x28   ;x
    ld  a, c
    jp TmsTextPos

EXTERN TmsChrOut
_TmsEmit:
    rst 0x28
    ld  a, c
    jp TmsChrOut

EXTERN TmsPlotPixel
EXTERN TmsSetPixel
EXTERN TmsPixelOp
_TmsPlotXY:
    ld hl, TmsSetPixel
    call TmsPixelOp
    rst 0x28   ;y
    ld  e, c
    rst 0x28   ;x
    ld  a, c
    jp TmsPlotPixel

EXTERN TmsPlotPixel
EXTERN TmsClearPixel
EXTERN TmsPixelOp
_TmsClrXY:
    ld hl, TmsClearPixel
    call TmsPixelOp
    rst 0x28   ;y
    ld  e, c
    rst 0x28   ;x
    ld  a, c
    jp TmsPlotPixel



