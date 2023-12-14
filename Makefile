# Path to naken_asm executable
NAKEN_ASM = naken_asm

programs = rpn

all : $(programs:%=%.hex)

%.hex : %.asm
	naken_asm -I include -l $< -o $@

clean :
	rm -f $(programs:%=%.hex) $(programs:%=%.lst)

.PHONY : clean
