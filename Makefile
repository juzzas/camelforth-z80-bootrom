CFLAGS=-v --list -m --c-code-in-asm


TARGET=camel80

.PHONY: all clean $(TARGET)

all: $(TARGET)

clean:
	rm -f *.o $(TARGET_BIN)
	rm -f .o *.lis *.map *.ihx *.bin

$(TARGET):
	zcc +embedded -startup=1 -clib=sdcc_iy ${CFLAGS} -pragma-include:zpragma.inc @camel80.lst -o camel80 -create-app
