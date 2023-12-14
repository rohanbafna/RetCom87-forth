# Path to 64tass executable
64TASS = 64tass

forth.hex : forth.asm header.asm
	64tass --intel-hex -o forth.hex -L forth.lst forth.asm

clean :
	rm -f forth.hex forth.lst

.PHONY : clean
