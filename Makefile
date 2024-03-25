# Path to 64tass executable
64TASS = 64tass

all : forth.1.hex forth.2.hex

forth.1.hex : forth.hex
	srec_cat forth.hex -Intel -crop 0x0000 0x1000 -Output forth.1.hex -Intel

forth.2.hex : forth.hex
	srec_cat forth.hex -Intel -crop 0x1000 0x2000 -Output forth.2.hex -Intel

forth.hex : forth.asm header.asm loader.asm boot.fs
	64tass --intel-hex -o forth.hex -L forth.lst loader.asm

clean :
	rm -f forth.hex forth.lst

.PHONY : clean
