CFLAGS=-v --list -m --c-code-in-asm


.PHONY: all clean tms

all: tms

clean:
	rm -f *.o $(TARGET)
	rm -f .o *.lis *.map *.ihx *.bin *.rom
	rm -f build/*

tms:
	zcc +embedded -subytpe=none --no-crt -clib=sdcc_iy ${CFLAGS} @tms.lst -o tms -create-app -Cz"--org 0xf000 --ihex"

font:
	zcc +embedded -subytpe=none --no-crt -clib=sdcc_iy ${CFLAGS} tmsfont.asm -o tmsfont -create-app -Cz"--org 0xe000 --ihex"
