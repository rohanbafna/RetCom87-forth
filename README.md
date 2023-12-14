# RetCom87-forth
FORTH interpreter for RetCom87 in 65816 assembly (for the 64tass
assembler).  To run, use EasySXB with a W65C265SXB and JML to address
$00:0300.  FORTH will run the boot program in `boot.fs` first, and
then begin the normal text interpreter.  Currently input is taken in
from a PS/2 keyboard connected with CLK on P40 and DATA on P42, and
output is over serial.

## Building

Run `make 64TASS=<path to 64tass executable>`.
