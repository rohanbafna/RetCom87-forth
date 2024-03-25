        ;; Rohan Bafna, Mar 24 2024
        ;; This is a "flash ROM loader": it's a program that writes
        ;; another program (located at the bottom of the file) to a
        ;; flash ROM on the W65C265SXB, allowing that program to be
        ;; run after power cycling the SXB.
        ;; Assembles with 64tass.

        .cpu "65816"

        .include "header.asm"

;;; === START OF LOADER ===
        * = $0200

        ;; setup status flags: turn emulation mode off, set A reg to
        ;; 8-bit, set X/Y regs to 16-bit.
        clc
        xce
        sep #FLAGM
        rep #FLAGX
        .as
        .xl

        ;; Clear the EEPROM.
        jsr clear

        ;; Write the program to the EEPROM, starting at address $8000.
        ldx #0                  ; start at offset 0
_wloop  cpx #progend-prog       ; until size = progend-prog bytes
        beq _wend               ; have been written ...
        lda prog,x              ;   load the current byte
        jsr write               ;   write it
        inx                     ;   go to the next byte
        bra _wloop              ;   and loop

_wend   brk

;;; write: Write the byte in A to the EEPROM at the address (relative
;;; to the EEPROM) in X.  The sector containing the byte written must
;;; have been cleared first.  Clobbers the A register, but not X or Y.
write   pha
        ;; Write unlock sequence to ROM
        lda #$AA
        sta $D555
        lda #$55
        sta $AAAA
        lda #$A0
        sta $D555

        ;; Write actual byte
        pla
        sta $8000,x
        ;; Wait for completion (read byte should match written byte)
_wait   cmp $8000,x
        bne _wait

        rts

;;; clear: Clears the entire EEPROM.  Clobbers the A register, but not
;;; X or Y.
clear   lda #$AA
        sta $D555
        lda #$55
        sta $AAAA
        lda #$80
        sta $D555
        lda #$AA
        sta $D555
        lda #$55
        sta $AAAA
        lda #$10
        sta $D555

        ;; Wait for completion
_wait   lda $8000
        cmp #$FF
        bne _wait

        rts

;;; === END OF LOADER ===

        ;; We wrap the program to be written inside a "logical" block.
        ;; This is 64tass's mechanism for relocatable code: the
        ;; program is assembled as if it will be run from the address
        ;; $8000, but the actual data is written directly after the
        ;; loader program.  Only after the loader copies the program
        ;; from RAM into the EEPROM can the program be run.

        ;; Since writing to the EEPROM is not simple, programs should
        ;; use addresses in SRAM as variables and copy any initial
        ;; values of the variables to SRAM on startup.

prog    .logical $8000
        .binclude "forth.asm"
        .endlogical
progend
