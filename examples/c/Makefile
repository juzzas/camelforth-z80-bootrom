CFLAGS=-v --list -m --c-code-in-asm


.PHONY: all clean hello

all: hello

clean:
	rm -f *.o $(TARGET)
	rm -f .o *.lis *.map *.ihx *.bin
	rm -f build/*

hello:
	zcc +rc2014 -subytpe=basic -clib=sdcc_iy ${CFLAGS} hello.c -o hello -create-app 
