;;; Basic FORTH interpreter.

        .cpu "65816"
        .include "header.asm"

;;; Memory locations
tib      = $4000
init_rsp = $5FFF
init_psp = tib

;;; Direct page variables
*       = $C0
dict_head .word ?               ; Pointer to the latest entry in dict
cp      .word ?                 ; Pointer to the next cell in dict
toin_v  .word ?                 ; Current offset into TIB.
n_tib   .word ?                 ; Number of characters in TIB.
tmp                             ; Temporary storage

;;; Main program
        * = $0200

        clc
        xce

        rep #FLAGM|FLAGX
        .al
        .xl

        jmp quit.body

;;; Dictionary

        ;; Each entry is formatted as a variable-length name field (as
        ;; a counted string) followed by a 2-byte field for the link,
        ;; a 2-byte field for the code pointer, and the data field.
        ;; The entries are laid out sequentially, and the address of
        ;; the latest entry is stored at dict_head.  load_dict_head is
        ;; the latest entry in the dictionary at load time/compile
        ;; time.  Each subroutine is called with JSR and returns with
        ;; RTS, and should be called with and return with FLAGM and
        ;; FLAGX reset.  The parameter stack pointer is stored in X.

last_entry := 0

entry   .segment name, word
\name   .ptext \word
        .word last_entry
last_entry := \name
\name.body
        .endsegment


;;; BL ( -- c ) Push an ASCII space onto the stack.
        .entry bl, "BL"
        jsr lit.body
        .word ' '
        rts


;;; 0BRANCH ( flag -- ) Branch to address specified in following cell
;;; if flag is false.
        .entry zero_branch, "0BRANCH"
        lda 0,x                 ; Load flag
        inx
        inx                     ; Pop flag
        and #$FFFF              ; Set status flags from A register
        beq branch.body         ; If false, branch
        ;; If we don't branch ...
        pla
        inc a
        inc a
        pha                     ; Correct return address to after
                                ; following cell
        rts                     ; Return to that address


;;; BRANCH ( -- ) Branch to address specified in following cell
;;; always.
        ;; Note: it wasn't until after I wrote this that I realized
        ;; this is just a shitty way to write "JMP".  I'm smart.
        .entry branch, "BRANCH"
        ldy #1
        lda (1,s),y
        dec a                   ; Decrement branch target because RTS
                                ; will increment it again
        sta 1,s                 ; Correct return address to branch
                                ; target
        rts                     ; Return to that address


;;; BREAK ( -- ) Causes a software interrupt (return to monitor)
        .entry break, "BREAK"
        brk
        .byte $00
        rts


;;; COUNT ( addr -- addr u ) Returns the count and string portions
;;; from a counted string.
        .entry count, "COUNT"
        lda (0,x)               ; Get first two bytes of string
        and #$FF                ; Clear high byte
        inc 0,x                 ; Move addr past count byte
        dex
        dex
        sta 0,x                 ; Push count to stack
        rts


;;; . ( n -- ) Print out n to the screen.
        ;; TODO: Make this print out in decimal.
        .entry dot, "."
        sep #FLAGM
        .as

        lda 1,x
        jsl SEND_HEX_OUT        ; print msb
        lda 0,x
        jsl SEND_HEX_OUT        ; print lsb

        lda #' '
        jsl PUT_CHR             ; print space

        rep #FLAGM
        .al

        inx
        inx                     ; pop n
        rts


;;; DROP ( x -- ) Drop one cell from the stack.
        .entry drop, "DROP"
        inx
        inx
        rts


;;; DUP ( x -- x x ) Duplicate the top cell on the stack.
        .entry dup, "DUP"
        lda 0,x
        dex
        dex
        sta 0,x
        rts


;;; EXECUTE ( xt -- ) Execute the subroutine specified by xt.
        .entry execute, "EXECUTE"
        lda 0,x
        dec a                   ; Decrement xt because RTS will
                                ; increment it again
        inx
        inx                     ; Pop xt from parameter stack
        pha                     ; Push xt onto return stack
        rts                     ; "Return" to xt


;;; @ ( addr -- x ) Replace addr with the contents of the cell at
;;; addr.
        .entry fetch, "@"
        lda (0,x)
        sta 0,x
        rts


;;; FIND ( c-addr -- c-addr 0 | xt -1 ) Find the definition
;;; corresponding to the c-addr word and either return c-addr and zero
;;; (if not found) or xt and -1 (if found).
        .entry find, "FIND"
_count  = tmp                   ; Count of characters to compare
_sp     = tmp+2                 ; Saved stack pointer
_ep     = tmp+4                 ; Dict entry pointer
_wp     = tmp+6                 ; Word pointer

        lda 0,x
        sta _wp                 ; Save word pointer

        sep #FLAGM
        .as
        lda (0,x)               ; Get count from c-addr
        inc a                   ; Increment to account for count byte
        sta _count              ; Save count
        stz _count+1            ; Zero out high byte of tmp

        stx _sp                 ; Save stack pointer
        ldx dict_head           ; x points to the dict entry

        ;; Compare word to dict entry's word.
_loop
        ldy #0                  ; Y indexes to the start of word
        stx _ep                 ; Save dict entry pointer

        ;; Check if the dict entry matches the current word.
_test_entry
        ;; On each iteration, compare current character in the word
        ;; with the corresponding character in the dict entry.  If not
        ;; equal, break (no match).  Increment Y and if y >= count,
        ;; found a match.
        lda (_wp),y             ; load current char in word
        cmp 0,x                 ; compare with char in dict entry
        bne _nomatch            ; break if not equal (no match)
        inx                     ; go to next char in dict entry
        iny                     ; go to next char in word
        cpy _count
        bmi _test_entry         ; if y < count, repeat

_match
        ;; We found a match.  Push return values on stack and return.
        inx
        inx                     ; x now points to xt
        txy                     ; y points to xt
        ldx _sp                 ; restore sp to x
        sty 0,x                 ; replace c-addr with xt

        dex
        dex
        ldy #-1
        sty 0,x                 ; push -1 to stack

        rep #FLAGM
        .al
        rts

_nomatch
        ;; The word didn't match the dictionary entry, so go to the
        ;; next one.
        .as
        rep #FLAGM
        .al
        lda (_ep)               ; a gets count byte of entry
        and #$FF                ; clear high byte
        inc a                   ; account for count byte
        tay                     ; y gets count
        lda (_ep),y             ; a gets dict-entry at offset count
                                ; (lf field)
        sep #FLAGM
        .as
        tax                     ; x gets lf field
        ;; If dict entry's link is empty, we're at the end of the
        ;; dictionary and didn't find a match, so return.  Otherwise
        ;; try again with the next entry.
        bne _loop

        ;; No match found.  Push 0 and return.
        rep #FLAGM
        .al
        ldx _sp                 ; restore sp to x
        dex
        dex
        stz 0,x                 ; push 0

        rts


;;; INTERPRET ( -- ? ) Interpret a line of code.
        ;; BEGIN  BL WORD DUP COUNT  WHILE
        ;;    DROP FIND  IF  EXECUTE  ELSE  COUNT NUMBER  THEN
        ;; AGAIN
        ;; DROP DROP
        .entry interpret, "INTERPRET"
_loop   jsr bl.body
        jsr word.body
        jsr dup.body
        jsr count.body
        jsr zero_branch.body
        .word _end
        jsr drop.body
        jsr find.body
        jsr zero_branch.body
        .word _else
        jsr execute.body
        jmp _then
_else   jsr count.body
        jsr number.body
_then   jmp _loop
_end    jsr drop.body
        jsr drop.body
        rts


;;; LIT ( -- x ) Pushes the following cell in the thread onto the
;;; stack.
        .entry lit, "LIT"
        ply
        iny                     ; get address of next cell
        dex
        dex
        lda 0,y
        sta 0,x
        iny
        phy                     ; point return address to cell after
        rts


;;; - ( n1 n2 -- n3 ) Subtracts n2 from n1 to get n3.
        .entry minus, "-"
        lda 2,x                 ; a := n1
        sec
        sbc 0,x                 ; a -= n2
        sta 2,x                 ; n3 := a
        inx
        inx                     ; pop n2 off stack
        rts


;;; NUMBER ( c-addr u -- n ) Converts the string at c-addr to a
;;; number.
        .entry number, "NUMBER"
_num    = tmp                   ; temporary storage for number
_neg    = tmp+2                 ; negative flag
_addr   = tmp+4                 ; address of string
_count  = tmp+6                 ; count of characters in string

        lda 0,x
        sta _count              ; _count := u
        lda 2,x
        sta _addr               ; _addr := c-addr
        stz _neg                ; set neg to 0
        ldy #0                  ; y starts at 0

        sep #FLAGM
        .as
        lda (_addr)             ; a gets first character
        cmp #'-'
        rep #FLAGM
        .al
        beq _set_neg            ; if first char is -, set the neg flag

_after_neg_check
        lda #0                  ; a starts as 0

_loop
        ;; Multiply current number by 10.
        asl
        sta _num                ; num := old_num*2
        asl
        asl                     ; a := old_num*8
        clc
        adc _num                ; a := old_num*10
        sta _num                ; num := old_num*10

        ;; Do bounds checking on the next character.
        sep #FLAGM
        .as
        lda (_addr),y           ; a gets next char
        cmp #'0'
        blt _error              ; if a<'0', error
        cmp #'9'+1
        bge _error              ; if a>'9', error

        ;; Add current number to next character minus #'0'
        rep #FLAGM
        .al
        and #$FF & ~'0'         ; clear top byte and convert char to
                                ; a number d
        adc _num                ; a := num*10 + d

        iny                     ; y gets index of next char
        cpy _count
        blt _loop               ; if y<count, go to next digit

        ;; Save num.  If neg is set, negate the parsed number.
        sta _num                ; save num
        lda _neg
        beq _no_neg             ; if neg = 0, don't negate

        lda _num
        eor #$FFFF
        inc a
        sta _num                ; negate num

        ;; Push the parsed number onto the stack.
_no_neg
        inx
        inx                     ; pop c-addr
        lda _num
        sta 0,x                 ; replace top of stack with _num

        rts

        ;; Set the neg flag
_set_neg
        lda #1
        sta _neg
        iny                     ; go to next character
        bra _after_neg_check

        ;; Signal an error
_error
        sep #FLAGM
        .as
        lda #0
        ldx #_err_msg
        jsl PUT_STR
        rep #FLAGM
        .al
        jmp quit.body
_err_msg .null "Could not find word in dictionary"


;;; + ( n1 n2 -- n3 ) Adds n1 to n2 to get n3.
        .entry plus, "+"
        lda 0,x                 ; a := n2
        clc
        adc 2,x                 ; a += n1
        sta 2,x                 ; n3 := a
        inx
        inx                     ; pop n2 off stack
        rts


;;; QUIT ( * -- ) Clear the return and data stacks and repeatedly read
;;; and interpret a line of code.
        .entry quit, "QUIT"
        ;; Clear return and data stacks
        ldx #init_psp
        lda #init_rsp
        tcs

        sep #FLAGM
        jsl SEND_CR
        rep #FLAGM

_loop
        jsr refill.body
        jsr interpret.body
        jsr lit.body
        .word _ok
        jsr typen.body
        jmp _loop

_ok     .null " ok", $0D


;;; REFILL ( -- ) Get a line of characters and store it in the TIB,
;;; then set >IN to 0.
        .entry refill, "REFILL"
        sep #FLAGM
        .as

        stx tmp
        ;; Read line to TIB.
        lda #0
        ldx #tib
        jsl GET_STR

        ;; Count characters in TIB.
        ldx #0
_loop   lda tib,x
        beq _fin
        cmp #$0D
        beq _fin
        inx
        bra _loop

_fin    stx n_tib

        ldx tmp
        rep #FLAGM
        .al
        ;; Clear >IN.
        stz toin_v

        rts


;;; TYPEN ( addr -- ) Type a null-terminated string to the terminal.
        .entry typen, "TYPEN"
        lda 0,x                 ; load addr to a
        stx tmp                 ; save stack pointer
        tax                     ; load addr to x
        sep #FLAGM
        .as
        lda #0
        jsl PUT_STR
        rep #FLAGM
        .al
        ldx tmp                 ; restore stack pointer
        inx
        inx                     ; pop addr
        rts


;;; ! ( x addr -- ) Stores x into the cell at addr.
        .entry store, "!"
        lda 2,x
        sta (0,x)
        txa
        clc
        adc #4
        tax
        rts


;;; >IN ( -- addr ) Return the address of a cell that contains the
;;; >current offset into TIB.
        .entry toin, ">IN"
        jsr lit.body
        .word toin_v
        rts


;;; WORD ( char -- addr ) Parse a word delimited by char from the
;;; input stream, skipping initial occurrences of char, and store the
;;; parsed word as a counted string in the dictionary, returning the
;;; location of the word as addr.
        .entry word, "WORD"
        stx tmp                 ; save sp in tmp
        lda 0,x
        sta tmp+2               ; store delim in tmp+2

        ldy #0                  ; y is length of word
        ldx toin_v              ; x gets next index to check.
        dex

        sep #FLAGM
        .as

        ;; Skip delims until the start of the word.
_skip_delims
        inx
        cpx n_tib
        beq _finish             ; If end of input, stop
        lda TIB,x               ; Load the current char into A
        cmp tmp+2               ; Compare with delimiter
        beq _skip_delims        ; If delim, repeat

        ;; Copy word into cp until delim is reached.
_grab_word
        iny                     ; Go to next char in output buffer
        sta (cp),y              ; Store character into cp at offset y
        inx
        cpx n_tib
        beq _finish             ; If end of input, found whole word
        lda TIB,x               ; Get next character
        cmp tmp+2               ; Compare with delimiter
        bne _grab_word          ; If delim, found whole word, else loop

        ;; Write the length of the word into the first char and
        ;; return.
_finish
        tya
        and #$EF                ; clear msb
        sta (cp)                ; Store length into first char.

        stx toin_v              ; Update toin
        ldx tmp                 ; restore sp into x

        rep #FLAGM
        .al
        lda cp
        sta 0,x                 ; Put cp on stack.

        rts


cp_val  = *
*       = cp
        .word cp_val            ; Fill default code pointer

*       = dict_head
        .word last_entry        ; Fill default dict head to last entry
