;;; rpn -- WIP FORTH interpreter

;;; Basic FORTH implementation, reads and executes one line at a time.
;;; Supports the words +, -, and .

        .65816
        .include "header.inc"

        ;; Direct page variables
        .org 0xC0
tmp:
        .resb 2                 ; Temporary storage
sp:
        .dw stack+0x100         ; Stack pointer
word:
        .resb 2                 ; Index of word in linebuf.
after_word:
        .resb 2                 ; Index after word in linebuf.
done:
        .resb 1                 ; Nonzero if we've finished line
neg:
        .resb 1                 ; Negative number flag for parsenum
dict_head:
        .dw load_dict_head      ; Pointer to the latest entry in dict.

        ;; Main program
        .org 0x200

        clc
        xce

        ;; Get line from monitor
        jsl SEND_CR
        sep #FLAGM
        rep #FLAGX
        lda.b #$00
        ldx.w #linebuf
        jsl GET_STR

        ;; Initialize variables
        rep #FLAGM
        lda.w #-1
        sta after_word          ; after_word starts with -1.
        sep #FLAGM
        stz done                ; done starts with 0.

        ;; On each iteration of the loop, parse a space-separated word
        ;; or number.
        .func parse_word
        ldx after_word          ; x gets next index to check.

skip_spaces:                    ; Skip spaces until start of word.
        inx
        lda linebuf,x
        cmp.b #' '
        beq skip_spaces         ; If space, repeat
        cmp.b #'\r'
        beq end                 ; If eol, stop
        stx word                ; word gets index of start of word.

grab_word:                      ; Search for end of word (space or null)
        inx
        lda linebuf,x
        cmp.b #' '
        beq eval                ; If space, found the whole word
        cmp.b #'\r'
        beq set_done            ; If eol, found the end of the line
        bra grab_word

set_done:                       ; Set done flag
        lda.b #1
        sta done
        .endf

eval:                           ; Evaluate meaning of word.
        stz linebuf,x           ; mark end of word with 0.
        stx after_word          ; after_word gets next index to search.

        ;; Search dictionary for word.
        .scope
        ldx dict_head           ; x points to the dict entry

        ;; Compare word to dict entry's word.
loop:
        ;; If dict entry's word is null, we're at the end of the
        ;; dictionary and didn't find a match.  Attempt to parse as a
        ;; number instead.
        lda 0,x
        beq parse_num
        stx tmp                 ; save dict entry pointer
        ldy word                ; y is a pointer to word

        ;; Check if the dict entry matches the current word.
test_entry:
        ;; On each iteration, compare current character in the word
        ;; with the corresponding character in the dict entry.  If not
        ;; equal, break (no match) and if equal and one character is
        ;; 0, break (match).  Else repeat.
        lda linebuf,y           ; load current char in word
        cmp 0,x                 ; compare with char in dict entry
        bne nomatch             ; break if not equal
        lda 0,x                 ; load char in dict entry
        beq match               ; break if zero
        inx
        iny
        bra test_entry          ; repeat

nomatch:
        ;; The word didn't match the dictionary entry, so go to the
        ;; next one.
        rep #FLAGM
        lda tmp                 ; load saved dict entry pointer
        clc
        adc.w #-DICT_ENTRY_SIZE ; get pointer to next entry
        tax
        sep #FLAGM
        bra loop

match:
        ;; We found a match.  Call the subroutine.
        rep #FLAGM
        lda tmp                 ; load saved dict entry pointer
        clc
        adc.w #DICT_ENTRY_OFFSET_FUNC ; get to address of func field
        tax
        jsr (0,x)
        .ends

repeat:
        sep #FLAGM
        lda done
        beq parse_word          ; if not done, repeat

end:
        brk

        ;; Parse a number and push it on the stack.
        .func parse_num

        ldy word
        lda linebuf,y
        cmp.b #'-'
        beq set_neg
        stz neg                 ; set neg to 0
        dey

after_neg_check:
        rep #FLAGM
        lda.w #0                ; a starts as 0

loop:
        ;; Read the next digit, check if it's an ASCII digit, and add
        ;; it to the number.
        sta tmp                 ; save current number

        iny
        sep #FLAGM
        lda linebuf,y
        beq finish              ; end of number found
        cmp.b #'0'
        bcc error
        cmp.b #'9'+1
        bcs error               ; bounds checking on digit

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
        lda linebuf,y           ; load ascii digit
        rep #FLAGM
        and.w #0x00ff & ~'0'    ; convert ascii digit to integer
        adc tmp                 ; add num*10 to new digit

        bra loop                ; go to next digit

finish:
        ;; If neg is set, negate the parsed number.  The parsed number
        ;; will be stored in tmp.
        lda neg
        rep #FLAGM
        beq push

        lda.w #0
        sec
        sbc tmp
        sta tmp                 ; negate tmp and store in tmp

push:
        ;; Push the parsed number on the stack.
        lda sp
        sec
        sbc.w #2
        sta sp
        lda tmp
        sta (sp)

        bra repeat

set_neg:
        ;; Set the neg flag.
        lda.b #1
        sta neg
        bra after_neg_check

error:
        ;; Print parse_num_err_msg.
        lda.b #0
        ldx.w #parse_num_err_msg
        jsl PUT_STR

        jmp repeat
        .endf

        ;; Other data
linebuf:
        .resb 0x100             ; Line buffer
stack:
        .resb 0x100             ; Data stack

        ;; Subroutines for each Forth word.  Each subroutine is called
        ;; with JSR and returns with RTS.

print_num:
        ;; Pops a number off the stack and prints it to the monitor.
        sep #FLAGM
        rep #FLAGX

        ldy.w #1
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

add:
        ;; Pops two numbers off the stack, adds them, and pushes the
        ;; result onto the stack.
        rep #FLAGM
        rep #FLAGX

        lda (sp)                ; Get first number
        ldy.w #2
        clc
        adc (sp),y              ; Add second number
        sta (sp),y              ; Store back on stack

        lda sp
        clc
        adc.w #2
        sta sp                  ; Increase sp by 2

        rts

subtract:
        ;; Pops two numbers off the stack, subtracts them, and pushes
        ;; the result onto the stack.
        rep #FLAGM
        rep #FLAGX

        ldy.w #2
        lda (sp),y              ; Get second number
        sec
        sbc (sp)                ; Subtract first number
        sta (sp),y              ; Store back on stack

        lda sp
        clc
        adc.w #2
        sta sp                  ; Increase sp by 2

        rts

        ;; Strings
parse_num_err_msg:
        .asciiz "Error parsing number"

        ;; Dictionary

        ;; Each entry is formatted as a 0x20 byte name field followed
        ;; by a 2-byte field for the code address.  The entries are
        ;; laid out sequentially, and the address of the latest entry
        ;; is stored at dict_head.  load_dict_head is the latest entry
        ;; in the dictionary at load time/compile time.
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

        ENTRY("+", add)
        ENTRY("-", subtract)
load_dict_head:
        ENTRY(".", print_num)
