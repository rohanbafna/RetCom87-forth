        ;; parser -- WIP parser for a FORTH interpreter

        ;; Reads a line, grabs space-separated words, and recognizes
        ;; numbers prefixed with !, which it then prints out in hex.

        .65816
        .include "header.inc"

        ;; Direct page variables
        .org 0xC0
tmp:    .resb 2                 ; Temporary storage
sp:     .dw stack+0x100         ; Stack pointer
lx:     .resb 2                 ; Line index
nlx:    .resb 2                 ; Next line index
done:   .resb 1                 ; Nonzero if we've finished line
neg:    .resb 1                 ; Negative number flag for parsenum
dicth:  .dw ldicth              ; Dictionary head

        ;; Main program
        .org 0x200

        clc
        xce

        ;; Get line from monitor
        jsl SEND_CR
        sep #FLAGM
        rep #FLAGX
        lda.b #$00
        ldx.w #lbuf
        jsl GET_STR

        ;; Initialize variables
        rep #FLAGM
        lda.w #-1
        sta nlx                 ; nlx starts with -1.
        sep #FLAGM
        stz done                ; done starts with 0.

        ;; On each iteration of the loop, parse a space-separated word
        ;; or number.
mainloop:
        ldx nlx                 ; x gets next index to check.

skipspaces:                     ; Skip spaces until start of word.
        inx
        lda lbuf,x
        cmp.b #' '
        beq skipspaces          ; If space, repeat
        cmp.b #'\r'
        beq end                 ; If eol, stop
        stx lx                  ; lx gets index of start of word.

grabword:                       ; Search for end of word (space or null)
        inx
        lda lbuf,x
        cmp.b #' '
        beq eval                ; If space, found the whole word
        cmp.b #'\r'
        beq setdone             ; If eol, found the end of the line
        bra grabword

setdone:                        ; Set done flag
        inc done
eval:                           ; Evaluate meaning of word.
        stz lbuf,x              ; mark end of word with 0.
        stx nlx                 ; nlx gets next index to search.

        ;; Search dictionary for word.
        ldx dicth               ; x is a pointer to dict entry

        ;; Compare word to dict entry's word.
searchloop:
        ;; If dict entry's word is null, we're at the end of the
        ;; dictionary and didn't find a match.  Attempt to parse as a
        ;; number instead.
        lda 0,x
        beq parsenum
        stx tmp                 ; save dict entry pointer
        ldy lx                  ; y is a pointer to word
wordcmploop:
        ;; On each iteration, compare current character in the word
        ;; with the corresponding character in the dict entry.  If not
        ;; equal, break (no match) and if equal and one character is
        ;; 0, break (match).  Else repeat.
        lda lbuf,y              ; load current char in word
        cmp 0,x                 ; compare with char in dict entry
        bne wordcmpfail         ; break if not equal
        lda 0,x                 ; load char in dict entry
        beq searchsuccess       ; break if zero
        inx
        iny
        bra wordcmploop         ; repeat

wordcmpfail:
        ;; The word didn't match the dictionary entry, so go to the
        ;; next one.
        rep #FLAGM
        lda tmp                 ; load saved dict entry pointer
        clc
        adc.w #-DICT_ENTRY_SIZE ; get pointer to next entry
        tax
        sep #FLAGM
        bra searchloop

searchsuccess:
        ;; We found a match.  Call subroutine.
        rep #FLAGM
        lda tmp                 ; load saved dict entry pointer
        clc
        adc.w #DICT_ENTRY_OFFSET_FUNC ; get to address of func field
        tax
        jsr (0,x)

repeat:
        sep #FLAGM
        lda done
        beq mainloop            ; if not done, repeat

end:
        brk

parsenum:                       ; Parse and print parsed number.
        ldy lx
        lda lbuf,y
        cmp.b #'-'
        beq parsenumsetnegflag
        stz neg                 ; set neg to 0
        dey

parsenumafternegcheck:
        rep #FLAGM
        lda.w #0                ; a starts as 0

parsenumloop:
        sta tmp                 ; save current number

        iny
        sep #FLAGM
        lda lbuf,y
        beq parsenumfinish      ; end of number found
        cmp.b #'0'
        bcc parsenumerror
        cmp.b #'9'+1
        bcs parsenumerror       ; bounds checking on digit

        rep #FLAGM
        lda tmp                 ; load current number
        asl
        sta tmp                 ; tmp gets num*2
        asl
        asl                     ; a gets num*8
        clc
        adc tmp                 ; a gets num*10
        sta tmp                 ; save num*10

        sep #FLAGM
        lda lbuf,y              ; load ascii digit
        rep #FLAGM
        and.w #0x00ff & ~'0'    ; convert ascii digit to integer
        adc tmp                 ; add num*10 to new digit

        bra parsenumloop        ; go to next digit

parsenumfinish:                 ; Negate parsed number (in tmp) if neg
        lda neg
        rep #FLAGM
        beq parsenumpush

        lda.w #0
        sec
        sbc tmp
        sta tmp                 ; negate tmp and store in tmp

parsenumpush:
        ;; Push the parsed number on the stack.
        lda sp
        sec
        sbc.w #2
        sta sp
        lda tmp
        sta (sp)

        bra repeat

parsenumsetnegflag:
        lda.b #1
        sta neg
        bra parsenumafternegcheck

parsenumerror:
        lda.b #0
        ldx.w #numerrm
        jsl PUT_STR             ; print numerrm message

        jmp repeat

        ;; Other data
lbuf:   .resb 0x100             ; Line buffer
stack:  .resb 0x100             ; Data stack

        ;; Dictionary

        ;; Each entry is formatted as a 0x20 byte name field followed
        ;; by a 2-byte field for the code address.  The entries are
        ;; laid out sequentially, and the address of the latest entry
        ;; is stored at dicth.  ldicth ("load-time dictionary head")
        ;; is the latest entry in the dictionary at load time/compile
        ;; time.
        .set DICT_ENTRY_SIZE=0x22
        .set DICT_ENTRY_OFFSET_FUNC=0x20

        .macro ENTRY(name, func)
        .scope
entry:  .asciiz name
        .org entry+DICT_ENTRY_OFFSET_FUNC
        .dw func
        .ends
        .endm

        ;; Sentinel entry; signals end of dictionary
        .db 0
        .resb DICT_ENTRY_SIZE-1

ldicth: ENTRY(".", print_num)

        ;; Subroutines for each Forth word.  Each subroutine is called
        ;; with JSR and returns with RTS.

print_num:
        ;; Pops the first number off the stack and prints it to the
        ;; monitor.
        sep #FLAGM
        rep #FLAGX

        ldy #1
        lda (sp),y
        jsl SEND_HEX_OUT        ; print msb
        dey
        lda (sp),y
        jsl SEND_HEX_OUT        ; print lsb

        lda.b #' '
        jsl PUT_CHR

        rep #FLAGM
        lda sp
        clc
        adc.w #2
        sta sp

        rts

        ;; Strings
numerrm:.asciiz "Error parsing number"
