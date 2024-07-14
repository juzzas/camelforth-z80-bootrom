CFLAGS=-v --list -m --c-code-in-asm


.PHONY: all clean petiteff

all: petiteff

clean:
	rm -f *.o $(TARGET)
	rm -f .o *.lis *.map *.ihx *.bin
	rm -f build/*

petiteff:
	#zcc +embedded -subytpe=none --no-crt -clib=sdcc_iy -SO3 --max-allocs-per-node200000 ${CFLAGS} petiteff.asm pff/diskio.c pff/pff.c -o petiteff -create-app -Cz"--org 0xe000 --ihex"
	zcc +embedded -subytpe=none --no-crt -clib=sdcc_iy -SO3 --max-allocs-per-node200000 ${CFLAGS} petiteff.asm pff/diskio.c pff/pff.c -o petiteff -create-app -Cz"--org 0xe000 --ihex"
