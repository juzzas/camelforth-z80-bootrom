ORG 0xe000
; jump table

; xx00 /PFF   ( xi-init xt-readsector xtwrite-sector -- flag )
JT_SLASHFAT:
    jp _SlashFat

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
scripts/docker/localcontainer/docker-u20-make -j32  MFG_BUILD=1 BUILD_PLATFORM=lakeport port-image port-mfg-image-create
start:
    rst 0x28  ; fetch TOS to BC
    ld a, b   ; swap B with C
    ld b, c
    ld c, a
    rst 0x20  ; push BC onto forth stack
    ret
