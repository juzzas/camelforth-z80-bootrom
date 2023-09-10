CFLAGS=-v --list -m --c-code-in-asm


.PHONY: all clean nibswap

all: nibswap

clean:
	rm -f *.o $(TARGET)
	rm -f .o *.lis *.map *.ihx *.bin
	rm -f build/*

nibswap:
	zcc +embedded -subytpe=none --no-crt -clib=sdcc_iy ${CFLAGS} nibswap.asm -o nibswap -create-app -Cz"--org 0xf000 --ihex"
