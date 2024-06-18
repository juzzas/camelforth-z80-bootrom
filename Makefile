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
	zcc +embedded -startup=1 -clib=sdcc_iy ${CFLAGS} -pragma-include:zpragma.inc @camel80.lst -o build/camel80 -create-app -Cz"--romsize=0x4000"
	# dd if=build/camel80_RC2014.bin of=build/camel80.rom bs=1 seek=8150 conv=notrunc
	dd if=build/camel80_code_user_16k.bin of=build/camel80.rom bs=1 seek=8192 conv=notrunc


blk:
	./blocks.py -v -o build/ttester.blk examples/ttester.fth
	./blocks.py -v -o build/fixed.blk examples/fixed.fth
	./blocks.py -v -o build/cffs.blk examples/cffs.fth
	./blocks.py -v -o build/callxt_test.blk examples/callxt_test.fth
	./blocks.py -v -o build/hexload_test2.blk examples/hexload_test2.fth

	dd if=build/ttester.blk of=camelforth.ide bs=1024 seek=1 conv=notrunc
	dd if=build/fixed.blk of=camelforth.ide bs=1024 seek=11 conv=notrunc
	dd if=build/cffs.blk of=camelforth.ide bs=1024 seek=21 conv=notrunc
	dd if=build/callxt_test.blk of=camelforth.ide bs=1024 seek=9 conv=notrunc
	dd if=build/hexload_test2.blk of=camelforth.ide bs=1024 seek=10 conv=notrunc

