;;; rpn -- WIP FORTH interpreter

;;; Basic FORTH implementation, reads and executes one line at a time.
;;; Supports the words +, -, and .

        .65816
        .include "header.inc"

        ;; Constants
        .set TIB=0x4000         ; Terminal input buffer.
        .set INIT_RSP=0x5FFF    ; Initial return stack pointer.
        .set INIT_PSP=TIB       ; Initial parameter stack pointer.

        ;; Direct page variables
        .org 0xC0
dict_head:
        .dw load_dict_head      ; Pointer to the latest entry in dict.
cp:
        .resw 1                 ; Pointer to the next cell in dict.
toin:
        .resw 1                 ; The current offset into TIB.
tmp:                            ; Temporary storage


        ;; Main program
        .org 0x200

        clc
        xce

        jsl SEND_CR

;;         ;; Parse a number and push it on the stack.
;;         .func parse_num

;;         ;; ldy word
;;         ;; lda linebuf,y
;;         cmp.b #'-'
;;         beq set_neg
;;         stz neg                 ; set neg to 0
;;         dey

;; after_neg_check:
;;         rep #FLAGM
;;         lda.w #0                ; a starts as 0

;; loop:
;;         ;; Read the next digit, check if it's an ASCII digit, and add
;;         ;; it to the number.
;;         sta tmp                 ; save current number

;;         iny
;;         sep #FLAGM
;;         lda linebuf,y
;;         beq finish              ; end of number found
;;         cmp.b #'0'
;;         bcc error
;;         cmp.b #'9'+1
;;         bcs error               ; bounds checking on digit

;;         rep #FLAGM
;;         lda tmp                 ; load current number
;;         asl
;;         sta tmp                 ; tmp gets num*2
;;         asl
;;         asl                     ; a gets num*8
;;         clc
;;         adc tmp                 ; a gets num*10
;;         sta tmp                 ; save num*10

;;         sep #FLAGM
;;         lda linebuf,y           ; load ascii digit
;;         rep #FLAGM
;;         and.w #0x00ff & ~'0'    ; convert ascii digit to integer
;;         adc tmp                 ; add num*10 to new digit

;;         bra loop                ; go to next digit

;; finish:
;;         ;; If neg is set, negate the parsed number.  The parsed number
;;         ;; will be stored in tmp.
;;         lda neg
;;         rep #FLAGM
;;         beq push

;;         lda.w #0
;;         sec
;;         sbc tmp
;;         sta tmp                 ; negate tmp and store in tmp

;; push:
;;         ;; Push the parsed number on the stack.
;;         lda sp
;;         sec
;;         sbc.w #2
;;         sta sp
;;         lda tmp
;;         sta (sp)

;;         bra repeat

;; set_neg:
;;         ;; Set the neg flag.
;;         lda.b #1
;;         sta neg
;;         bra after_neg_check

;; error:
;;         ;; Print parse_num_err_msg.
;;         lda.b #0
;;         ldx.w #parse_num_err_msg
;;         jsl PUT_STR

;;         jmp repeat
;;         .endf

        ;; Strings
parse_num_err_msg:
        .asciiz "Error parsing number"

        ;; Helper subroutines

        ;; Pushes the Y register onto the stack.  Assumes FLAGM and
        ;; FLAGX reset.
push:   dex
        dex
        sty.b 0,x
        rts


        ;; ===========================
        ;;         Dictionary
        ;; ===========================

        ;; Each entry is formatted as a variable-length name field (as
        ;; a counted string) followed by a 2-byte field for the link,
        ;; a 2-byte field for the code pointer, and the data field.
        ;; The entries are laid out sequentially, and the address of
        ;; the latest entry is stored at dict_head.  load_dict_head is
        ;; the latest entry in the dictionary at load time/compile
        ;; time.  Each subroutine is called with JSR and returns with
        ;; RTS, and should be called with and return with FLAGM and
        ;; FLAGX reset.  The parameter stack pointer is stored in X.

        .macro ENTRY(name, word, wordlength, link, code)
        .db wordlength
        .ascii word
        .dw link
        .dw code
        .endm

        ;; ---------------------------
        ;;             +
        ;; ---------------------------
        ;; ( n1 n2 -- n1+n2 )
        ;; Pops two numbers off the stack, adds them, and pushes the
        ;; result onto the stack.
        ENTRY(plus, "+", 1, 0, plus_impl)
plus_impl:
        lda 2,x                 ; a = n1
        clc
        adc 0,x                 ; a += n2
        sta 2,x                 ; Store back on stack
        inx
        inx                     ; Increase sp by 2
        rts


        ;; ---------------------------
        ;;             -
        ;; ---------------------------
        ;; ( n1 n2 -- n1-n2 )
        ;; Pops two numbers off the stack, subtracts them, and pushes
        ;; the result onto the stack.
        ENTRY(minus, "-", 1, plus, minus_impl)
minus_impl:
        lda 2,x                 ; a = n1
        sec
        sbc 0,x                 ; a -= n2
        sta 2,x                 ; Store back on stack
        inx
        inx                     ; Increase sp by 2
        rts


        ;; ---------------------------
        ;;             .
        ;; ---------------------------
        ;; ( n -- )
        ;; Pops a number off the stack and prints it to the monitor.
        ENTRY(dot, ".", 1, minus, dot_impl)
dot_impl:
        sep #FLAGM

        lda 1,x
        jsl SEND_HEX_OUT        ; print msb
        lda 0,x
        jsl SEND_HEX_OUT        ; print lsb

        lda.b #' '
        jsl PUT_CHR

        rep #FLAGM

        inx
        inx                     ; pop off stack
        rts


        ;; ---------------------------
        ;;            QUIT
        ;; ---------------------------
        ;; ( ? -- ? )
        ;; Repeatedly resets the return stack and reads and runs a
        ;; line of code, then prints " ok".
        ENTRY(quit, "QUIT", 4, dot, quit_impl)
        .func quit_impl
        lda.w #INIT_RSP
        tcs                     ; S gets INIT_RSP.

        jsl tib_impl
        jsl accept_impl
        jsl num_tib_impl
        jsl store_impl
        stz toin                ; clear >IN
        jsl interpret_impl

        ;; Print " ok".
        stx tmp
        sep #FLAGM
        lda.b #0
        ldx.w #ok
        jsl PUT_STR
        rep #FLAGM
        ldx tmp

        bra quit_impl
ok:     .asciiz " ok\n"
        .endf


        ;; ---------------------------
        ;;           ACCEPT
        ;; ---------------------------
        ;; ( addr -- u )
        ;; Reads a line from the monitor into the buffer at addr and
        ;; returns the count of received characters in u.
        ENTRY(accept, "ACCEPT", 6, quit, accept_impl)
        .func accept_impl
        stx tmp
        ldy 0,x                 ; y contains addr
        tyx
        sep #FLAGM
        lda.b #0
        jsl GET_STR             ; Read line into addr.
        ldx tmp

        ;; Search for ending \r or \0.
loop:   lda 0,y
        beq found_end
        cmp.b #'\r'
        beq found_end
        iny
        bra loop

        ;; Found ending \r or \0, subtract current pointer from start
        ;; and store result on stack.
found_end:
        rep #FLAGM
        tya
        sec
        sbc 0,x
        sta 0,x

        rts
        .endf


        ;; ---------------------------
        ;;              !
        ;; ---------------------------
        ;; ( x a-addr -- )
        ;; Store x at a-addr.
        ENTRY(store, "!", 1, accept, store_impl)
store_impl:
        lda 2,x                 ; a = x
        sta (0,x)               ; *a-addr = x
        txa
        clc
        adc.w #4
        tax                     ; increment sp by 4
        rts


        ;; ---------------------------
        ;;             TIB
        ;; ---------------------------
        ;; ( -- tib )
        ;; Get the address of the terminal input buffer.
        ENTRY(tib, "TIB", 3, store, tib_impl)
tib_impl:
        dex
        dex
        ldy.w #TIB
        sty 0,x
        rts


        ;; ---------------------------
        ;;            #TIB
        ;; ---------------------------
        ;; ( -- addr )
        ;; Variable for the length of the terminal input buffer.
        ENTRY(num_tib, "#TIB", 4, tib, num_tib_impl)
num_tib_val:
        .resb 2
num_tib_impl:
        dex
        dex
        ldy.w #num_tib_val
        sty 0,x
        rts


        ;; ---------------------------
        ;;         INTERPRET
        ;; ---------------------------
        ;; ( -- )
        ;; Interprets each word in the line.
        ENTRY(interpret, "INTERPRET", 9, num_tib, interpret_impl)
interpret_impl:
        jsr bl_impl
        jsr word_impl
        jsr find_impl
        rts


        ;; ---------------------------
        ;;            WORD
        ;; ---------------------------
        ;; ( char -- c-addr )
        ;; Parse word delimited by char and store it in cp.
        ENTRY(word, "WORD", 4, interpret, word_impl)
        .func word_impl
        stx tmp                 ; save sp in tmp
        sep #FLAGM
        lda 0,x
        sta tmp+2               ; store delim in tmp+2

        ldy.w #0                ; y is length of word
        ldx toin                ; x gets next index to check.
        dex

        ;; Skip delims until the start of the word.
skip_delims:
        inx
        cpx num_tib_val
        beq finish              ; If end of input, stop
        lda TIB,x               ; Load the current char into A
        cmp tmp+2               ; Compare with delimiter
        beq skip_delims         ; If delim, repeat

        ;; Copy word into cp until delim is reached.
grab_word:
        iny                     ; Go to next char in output buffer
        sta (cp),y              ; Store character into cp at offset y
        inx
        cpx num_tib_val
        beq finish              ; If end of input, found whole word
        lda TIB,x               ; Get next character
        cmp tmp+2               ; Compare with delimiter
        bne grab_word           ; If delim, found whole word, else loop

        ;; Write the length of the word into the first char and
        ;; return.
finish:
        tya
        and.b #EF               ; clear msb
        sta (cp)                ; Store length into first char.

        stx toin                ; Update toin
        ldx tmp                 ; restore sp into x
        dex                     ; make space for other half of cell

        rep #FLAGM
        lda cp
        sta 0,x                 ; Put cp on stack.

        rts
        .endf


        ;; ---------------------------
        ;;             BL
        ;; ---------------------------
        ;; ( -- char )
        ;; Place an ASCII blank on the stack.
        ENTRY(bl, "BL", 2, word, bl_impl)
bl_impl:
        dex                     ; Increase stack by one char.
        sep #FLAGM
        lda.b #' '              ; Load blank into A.
        sta 0,x                 ; Push blank on stack.
        rep #FLAGM
        rts


        ;; ---------------------------
        ;;            FIND
        ;; ---------------------------
        ;; ( c-addr -- c-addr 0 | xt -1 )
        ;; Find the definition corresponding to the c-addr word and
        ;; either return c-addr and zero (if not found) or xt and -1
        ;; (if found).
        ENTRY(find, "FIND", 4, bl, find_impl)
        .func find_impl
        sep #FLAGM
        lda (0,x)               ; Get count from c-addr
        and.b #EF               ; Clear MSB (immediate bit)
        inc                     ; Increment to account for count byte
        sta tmp                 ; Store count in tmp
        stz tmp+1               ; Zero out high byte of tmp

        stx tmp+2               ; Store stack pointer in tmp+2
        ldx dict_head           ; x points to the dict entry

        ;; Compare word to dict entry's word.
loop:
        ldy.w #0                ; Y indexes to the start of word
        stx tmp+4               ; Save dict entry pointer

        ;; Check if the dict entry matches the current word.
test_entry:
        ;; On each iteration, compare current character in the word
        ;; with the corresponding character in the dict entry.  If not
        ;; equal, break (no match).  Increment Y and if y >= count,
        ;; found a match.
        lda (tmp+2),y           ; load current char in word
        cmp 0,x                 ; compare with char in dict entry
        bne nomatch             ; break if not equal (no match)
        inx                     ; go to next char in dict entry
        iny                     ; go to next char in word
        cpy tmp                 ; compare y to count
        bmi test_entry          ; if y < count, repeat

match:
        ;; We found a match.  Push return values on stack and return.
        inx
        inx                     ; x now points to xt
        txy                     ; y points to xt
        ldx tmp+2               ; restore sp to x
        sty 0,x                 ; replace c-addr with xt

        dex
        dex
        ldy.w #-1
        sty 0,x                 ; push -1 to stack

        rep #FLAGM
        rts

nomatch:
        ;; The word didn't match the dictionary entry, so go to the
        ;; next one.
        rep #FLAGM
        ldy tmp                 ; y gets count
        lda (tmp+4),y           ; a gets dict-entry at offset count
                                ; (lf field)
        sep #FLAGM
        tax                     ; x gets lf field
        ;; If dict entry's link is empty, we're at the end of the
        ;; dictionary and didn't find a match, so return.  Otherwise
        ;; try again with the next entry.
        bne loop

        ;; No match found.  Push 0 and return.
        ldx tmp+2               ; restore sp to x
        dex
        dex
        stz 0,x                 ; push 0

        rep #FLAGM
        rts

        .ends


load_dict_head:
