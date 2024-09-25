CFLAGS=-v --list -m --c-code-in-asm


TARGET=camel80

.PHONY: all clean blk $(TARGET)

all: $(TARGET) blk

clean:
	rm -f *.o $(TARGET)
	rm -f .o *.lis *.map *.ihx *.bin
	rm -f build/*

$(TARGET):
	mkdir -p build
	zcc +embedded --no-crt -clib=sdcc_iy ${CFLAGS} -pragma-include:zpragma.inc @camel80.lst -o build/camel80
	z88dk-appmake +glue -v --clean --ihex --pad --exclude-banks data --exclude-sections data -b build/camel80


blk:
	./blocks.py -v -c -s 8192 -o build/cf-image.blk 
	./blocks.py -v -w -s 0 -o build/cf-image.blk extensions/00system.fth
	./blocks.py -v -w -s 9 -o build/cf-image.blk extensions/assembler.fth
	./blocks.py -v -w -s 12 -o build/cf-image.blk extensions/tload.fth
	
	./blocks.py -v -w -s 30 -o build/cf-image.blk extensions/forth2012.fth
	./blocks.py -v -w -s 33 -o build/cf-image.blk extensions/core-ext.fth
	./blocks.py -v -w -s 36 -o build/cf-image.blk extensions/facility-ext.fth
	./blocks.py -v -w -s 39 -o build/cf-image.blk extensions/double.fth
	./blocks.py -v -w -s 42 -o build/cf-image.blk extensions/string.fth
	./blocks.py -v -w -s 45 -o build/cf-image.blk extensions/tools-ext.fth
	./blocks.py -v -w -s 54 -o build/cf-image.blk extensions/memory.fth
	
	./blocks.py -v -w -s 90 -o build/cf-image.blk extensions/ttester.fth
	./blocks.py -v -w -s 99 -o build/cf-image.blk extensions/fixed.fth
	./blocks.py -v -w -s 108 -o build/cf-image.blk extensions/random.fth
	./blocks.py -v -w -s 111 -o build/cf-image.blk extensions/ansi.fth
	./blocks.py -v -w -s 117 -o build/cf-image.blk extensions/leds.fth
	
	./blocks.py -v -w -s 300 -o build/cf-image.blk extensions/testsuite.fth
	./blocks.py -v -w -s 303 -t -o build/cf-image.blk forth2012-test-suite/src/prelimtest.fth
	./blocks.py -v -w -s 315 -t -o build/cf-image.blk forth2012-test-suite/src/core.fr
	
	./blocks.py -v -w -s 600 -o build/cf-image.blk examples/hexload_test.fth
	./blocks.py -v -w -s 612 -o build/cf-image.blk examples/hexload_test2.fth
	dd if=build/cf-image.blk of=camelforth.ide bs=1024 seek=1 conv=notrunc
