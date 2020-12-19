CFLAGS=-v --list -m --c-code-in-asm
#LDFLAGS=-clibansi -lndos
LDFLAGS=-lndos

DEPS = 
SRC = rc2014_map.asm z80intr.asm int32k.asm camel80.asm.m4


TARGET=camel80

all: $(TARGET)

clean:
	rm -f *.o $(TARGET_BIN)
	rm -f .o *.lis *.map *.ihx *.bin

$(TARGET): $(SRC)
	#zcc +embedded -v -m --list -o $@ -subtype=none --no-crt -Ca '-l' $^ -create-app
	zcc +embedded -v -m --list -o $@ -subtype=none --no-crt -Ca '-l' $^ -create-app -Cz"+glue --ihex --clean"

	
	#zcc +rc2014 -subtype=acia -vn -SO3 --list -m -clib=sdcc_iy --max-allocs-per-node200000 $^ -o $@ -create-app
