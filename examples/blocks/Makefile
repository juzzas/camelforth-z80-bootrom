CFLAGS=-v --list -m --c-code-in-asm


.PHONY: all clean cflash

all: cflash

clean:
	rm -f *.o $(TARGET)
	rm -f .o *.lis *.map *.ihx *.bin
	rm -f build/*

cflash:
	zcc +embedded -subytpe=none --no-crt -clib=sdcc_iy ${CFLAGS} cflash.asm -o cflash -create-app -Cz"--org 0xf000 --ihex"

