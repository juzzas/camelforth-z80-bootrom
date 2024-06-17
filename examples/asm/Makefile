CFLAGS=-v --list -m --c-code-in-asm


.PHONY: all clean byteswap call-xt

all: byteswap call-xt

clean:
	rm -f *.o $(TARGET)
	rm -f .o *.lis *.map *.ihx *.bin
	rm -f build/*

byteswap:
	zcc +embedded -subytpe=none --no-crt -clib=sdcc_iy ${CFLAGS} byteswap.asm -o byteswap -create-app -Cz"--org 0xf000 --ihex"

call-xt:
	zcc +embedded -subytpe=none --no-crt -clib=sdcc_iy ${CFLAGS} call-xt.asm -o call-xt -create-app -Cz"--org 0xf000 --ihex"
