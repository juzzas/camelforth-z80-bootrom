CFLAGS=-v --list -m --c-code-in-asm


TARGET=camel80

.PHONY: all clean blk $(TARGET)

all: $(TARGET) 

clean:
	rm -f *.o $(TARGET)
	rm -f .o *.lis *.map *.ihx *.bin
	rm -f build/*

$(TARGET):
	mkdir -p build
	zcc +embedded --no-crt -clib=sdcc_iy ${CFLAGS} -pragma-include:zpragma.inc @camel80.lst -o build/camel80
	z88dk-appmake +glue -v --clean --ihex --pad --exclude-banks data --exclude-sections data -b build/camel80


blk:
	gforth ./gen-blocks.fth
	dd if=build/cf-image.blk of=camelforth.ide bs=1024 seek=1 conv=notrunc
