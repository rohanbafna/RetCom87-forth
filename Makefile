# Path to 64tass executable
64TASS = 64tass

forth.hex : forth.asm header.asm boot.preprocessed
	64tass --intel-hex -o forth.hex -L forth.lst forth.asm

boot.preprocessed : boot.fs
	tr '\n' ' ' <boot.fs >boot.preprocessed

clean :
	rm -f forth.hex forth.lst boot.preprocessed

.PHONY : clean
