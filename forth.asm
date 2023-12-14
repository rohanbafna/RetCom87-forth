;;; Basic FORTH interpreter.

        .cpu "65816"
        .include "header.asm"

;;; Constants
tib      = $4000                ; location of terminal input buffer
init_rsp = $5FFF                ; initial return stack pointer
init_psp = tib                  ; initial parameter stack pointer
precedence = $80                ; precedence bit bitmask
smudge  = $40                   ; smudge bit bitmask
noctrl  = $1F                   ; no control bits bitmask

;;; Direct page variables
*       = $C0
dict_head .word ?               ; Pointer to the latest entry in dict
cp_v    .word ?                 ; Pointer to the next cell in dict
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

;;; Each entry in the dictionary is formatted as a variable-length
;;; name field (as a counted string) followed by a 2-byte field for
;;; the link, followed by the executable code and/or data field.  The
;;; entries are laid out sequentially, and the address of the latest
;;; entry is stored at dict_head.  load_dict_head is the latest entry
;;; in the dictionary at load time/compile time.  Each subroutine is
;;; called with JSR and returns with RTS, and should be called with
;;; and return with FLAGM and FLAGX reset.  The parameter stack
;;; pointer is stored in X.

last_entry := 0

entry   .segment name, word, immediate=false
\name   .text len (\word) | (\immediate*precedence), \word
        .word last_entry
last_entry := \name
\name.body
        .endsegment


;;; --------------------------------
;;;     DATA STACK MANIPULATION
;;; --------------------------------

;;; ?DUP ( x -- 0 | x x ) Conditionally duplicate the top item on the
;;; stack if its value is non-zero.
        .entry question_dup, "?DUP"
        lda 0,x
        beq _return             ; if x == 0, return
        dex
        dex
        sta 0,x
_return rts


;;; DEPTH ( -- +n ) Return the number of single-cell values that were
;;; on the stack before this word executed.
        .entry depth, "DEPTH"
        stx tmp
        lda #init_psp
        sec
        sbc tmp
        lsr a                   ; a := (init_psp-x)/2
        dex
        dex
        sta 0,x
        rts


;;; DROP ( x -- ) Drop one cell from the stack.
        .entry drop, "DROP"
        inx
        inx
        rts


;;; 2DROP ( x1 x2 -- )
        .entry two_drop, "2DROP"
        inx
        inx
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


;;; 2DUP ( x1 x2 -- x1 x2 x1 x2 )
        .entry two_dup, "2DUP"
        lda 2,x                 ; a = x1
        ldy 0,x                 ; y = x2
        dex
        dex
        dex
        dex
        sta 2,x
        sty 0,x
        rts


;;; NIP ( x1 x2 -- x2 )
        .entry nip, "NIP"
        lda 0,x
        sta 2,x
        inx
        inx
        rts


;;; OVER ( x1 x2 -- x1 x2 x1 )
        .entry over, "OVER"
        lda 2,x
        dex
        dex
        sta 0,x
        rts


;;; 2OVER ( x1 x2 x3 x4 -- x1 x2 x3 x4 x1 x2 ) Copy cell pair x1,x2 to
;;; the top of the stack.
        .entry two_over, "2OVER"
        lda 6,x                 ; a = x1
        ldy 4,x                 ; y = x2
        dex
        dex
        dex
        dex
        sta 2,x
        sty 0,x
        rts


;;; PICK ( +n -- x ) Place a copy of the nth stack entry on top of the
;;; stack, where n=0 refers to the top of the stack.
        .entry pick, "PICK"
        lda 0,x                 ; a = n
        inc a                   ; offset n by 1 to account for n's
                                ; place on the stack
        asl a                   ; multiply a by 2 to account for cell
                                ; size
        sta tmp
        txy
        lda (tmp),y             ; a gets cell at sp+n*2
        sta 0,x
        rts


;;; ROT ( x1 x2 x3 -- x2 x3 x1 )
        .entry rot, "ROT"
        lda 4,x                 ; a = x1
        ldy 0,x                 ; y = x3
        sta 0,x
        lda 2,x                 ; a = x2
        sty 2,x
        sta 4,x
        rts


;;; SWAP ( x1 x2 -- x2 x1 )
        .entry swap, "SWAP"
        lda 0,x
        ldy 2,x
        sta 2,x
        sty 0,x
        rts


;;; 2SWAP ( x1 x2 x3 x4 -- x3 x4 x1 x2 ) Exchange the top two cell
;;; pairs.
        .entry two_swap, "2SWAP"
        lda 6,x                 ; a = x1
        ldy 2,x                 ; y = x3
        sta 2,x
        sty 6,x
        lda 4,x                 ; a = x2
        ldy 0,x                 ; y = x4
        sta 0,x
        sty 4,x
        rts


;;; TUCK ( x1 x2 -- x2 x1 x2 ) Place a copy of the top stack item
;;; below the second stack item.
        .entry tuck, "TUCK"
        lda 2,x                 ; a = x1
        ldy 0,x                 ; y = x2
        dex
        dex
        sty 4,x
        sta 2,x
        sty 0,x
        rts



;;; --------------------------------
;;;         UNSORTED WORDS
;;; --------------------------------

;;; AND ( x1 x2 -- x3 ) x3 is the bitwise and of x1 and x2.
        .entry and_, "AND"
        lda 0,x
        and 2,x
        sta 2,x
        inx
        inx
        rts


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


;;; BREAK ( -- ) Cause a software interrupt (return to monitor)
        .entry break, "BREAK"
        brk
        .byte $00
        rts


;;; C, ( char -- ) Reserve a byte of data space and store char in that
;;; byte.
        .entry c_comma, "C,"
        lda 0,x
        sep #FLAGM
        .as
        sta (cp_v)
        rep #FLAGM
        .al
        inc cp_v
        inx
        inx
        rts


;;; C@ ( addr -- char ) Fetch the character at addr.
        .entry c_fetch, "C@"
        lda (0,x)
        and #$FF
        sta 0,x
        rts


;;; C! ( c addr -- ) Store c at addr.
        .entry c_store, "C!"
        sep #FLAGM
        .as
        lda 2,x
        sta (0,x)
        rep #FLAGM
        .al
        inx
        inx
        inx
        inx
        rts


;;; : <name> ( -- ) Create a colon definition and enter compilation
;;; state.
        ;; \ Create definition stub.
        ;; BL WORD COUNT DUP smudge OR C, CP +! \ name field
        ;; LATEST @ , \ link field
        ;; 1- LATEST ! \ update latest with address of new word
        ;; \ Enter compilation mode.
        ;; ]
        .entry colon, ":"
        jsr bl.body
        jsr word.body
        jsr count.body
        jsr dup.body
        jsr lit.body
        .word smudge
        jsr or.body
        jsr c_comma.body
        jsr cp.body
        jsr plus_store.body
        jsr latest.body
        jsr fetch.body
        jsr comma.body
        jsr one_minus.body
        jsr latest.body
        jsr store.body
        jsr right_bracket.body
        rts


;;; , ( x -- ) Reserve a cell of data space and store x in that cell.
        .entry comma, ","
        lda 0,x
        sta (cp_v)
        lda cp_v
        inc a
        inc a
        sta cp_v
        inx
        inx
        rts


;;; COMPILE, ( xt -- ) Append the execution behavior of the
;;; definition represented by xt to the current definition.
        .entry compile_comma, "COMPILE,"
        jsr lit.body
        .word $20               ; opcode for JSR with absolute
                                ; addressing
        jsr c_comma.body
        jsr comma.body
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


;;; CP ( -- addr ) Return the address of CP.
        .entry cp, "CP"
        jsr lit.body
        .word cp_v
        rts


;;; . ( n -- ) Print out n to the screen.
        ;; \ If n is negative, print a - sign.
        ;; DUP <0  IF  NEGATE CHAR - EMIT  THEN
        ;; \ Add a sentinel 0 to the stack, under n.
        ;; 0 SWAP
        ;; \ Push the ASCII digits of n in reverse order onto the
        ;; stack.
        ;; BEGIN
        ;;    10 /MOD SWAP CHAR 0 + SWAP
        ;;    DUP 0=
        ;; UNTIL
        ;; DROP
        ;; \ Pop the digits of n and print them out.
        ;; BEGIN  ?DUP  WHILE  EMIT  AGAIN
        ;; \ Print a space after the number.
        ;; BL EMIT ;
        .entry dot, "."
        jsr dup.body
        jsr zero_less_than.body
        jsr zero_branch.body
        .word _then
        jsr negate.body
        jsr lit.body
        .word "-"
        jsr emit.body
_then
        jsr lit.body
        .word 0
        jsr swap.body
_begin1
        jsr lit.body
        .word 10
        jsr slash_mod.body
        jsr swap.body
        jsr lit.body
        .word "0"
        jsr plus.body
        jsr swap.body
        jsr dup.body
        jsr zero_equal.body
        jsr zero_branch.body
        .word _begin1
        jsr drop.body
_begin2
        jsr question_dup.body
        jsr zero_branch.body
        .word _again
        jsr emit.body
        jmp _begin2
_again
        jsr bl.body
        jsr emit.body
        rts


;;; .( ( "ccc<paren>" -- ) Parse and display ccc delimited by ).
        ;; : .(   $29 PARSE TYPE ;
        .entry dot_paren, ".("
        jsr lit.body
        .word $29
        jsr parse.body
        jsr type.body
        rts


;;; .S ( -- ) Display the stack, for debugging purposes.
        .entry dot_s, ".S"
        ;; Print number of entries on stack.
        jsr lit.body
        .word '<'
        jsr emit.body
        jsr bl.body
        jsr emit.body
        jsr depth.body
        jsr dot.body            ; print a
        jsr lit.body
        .word '>'
        jsr emit.body
        jsr bl.body
        jsr emit.body
        ;; Print individual entries on stack.
        ldy #init_psp-2
_loop   stx tmp
        cpy tmp
        blt _end                ; if y<sp, exit loop
        lda 0,y                 ; get item y is pointing to
        dex
        dex
        sta 0,x                 ; push it onto stack
        phy
        jsr dot.body            ; print cell
        ply
        dey
        dey                     ; go to next item
        bra _loop
_end    rts


;;; EMIT ( char -- ) Prints out char to the screen.
        .entry emit, "EMIT"
        lda 0,x
        sep #FLAGM
        .as
        jsl PUT_CHR
        rep #FLAGM
        .al
        inx
        inx                     ; pop char
        rts


;;; = ( x1 x2 -- flag ) flag is true when x1 is equal to x2.
        .entry equal, "="
        lda 0,x
        inx
        inx
        cmp 0,x
        beq _true
        stz 0,x
        rts
_true   lda #-1
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


;;; FIND ( c-addr -- c-addr 0 | xt 1 | xt -1 ) Find the definition
;;; corresponding to the c-addr word and either return c-addr and zero
;;; (if not found), xt and 1 (if found and immediate), or xt and -1
;;; (if found and not immediate).
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
        stx _ep                 ; Save dict entry pointer

        ;; Compare count bytes (we need to mask out the control bits).
        lda 0,x                 ; load count byte from entry
        bit #smudge             ; test smudge bit
        bne _nomatch            ; if smudge bit set, ignore entry
        and #noctrl             ; ignore control bits
        cmp (_wp)               ; compare with count from word
        bne _nomatch            ; break if not equal (no match)
        inx                     ; go to next char in dict entry

        ldy #1                  ; Y indexes to the start of word, past
                                ; count byte

        ;; Check if the dict entry name matches the current word.
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

        ldy #1
        lda (_ep)               ; check count byte of array
        bit #precedence         ; check precedence flag
        bne _imm                ; if set, push 1 instead of -1
        ldy #-1                 ; set y to -1
_imm    dex
        dex
        sty 0,x                 ; push 1 or -1 to stack

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
        and #noctrl             ; clear high byte and control bits
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


;;; HERE ( -- addr ) Return the next available address in the
;;; dictionary.
        .entry here, "HERE"
        dex
        dex
        lda cp_v
        sta 0,x
        rts


;;; INTERPRET ( -- ? ) Interpret or compile a line of code.
        ;; BEGIN  BL WORD DUP COUNT  WHILE
        ;;    DROP FIND ?DUP  IF
        ;;       STATE @  IF
        ;;          0>  IF  EXECUTE  ELSE  COMPILE,  THEN
        ;;       ELSE
        ;;          DROP EXECUTE
        ;;       THEN
        ;;    ELSE
        ;;       COUNT NUMBER
        ;;       STATE @  IF  LITERAL  THEN
        ;;    THEN
        ;; AGAIN
        ;; DROP DROP ;
        .entry interpret, "INTERPRET"
_loop   jsr bl.body
        jsr word.body
        jsr dup.body
        jsr count.body
        jsr zero_branch.body
        .word _end
        jsr drop.body
        jsr find.body
        jsr question_dup.body
        jsr zero_branch.body
        .word _else1
        jsr state.body
        jsr fetch.body
        jsr zero_branch.body
        .word _else2
        jsr zero_greater_than.body
        jsr zero_branch.body
        .word _else3
        jsr execute.body
        jmp _then3
_else3  jsr compile_comma.body
_then3  jmp _then2
_else2  jsr drop.body
        jsr execute.body
_then2  jmp _then1
_else1  jsr count.body
        jsr number.body
        jsr state.body
        jsr fetch.body
        jsr zero_branch.body
        .word _then4
        jsr literal.body
_then4
_then1  jmp _loop
_end    jsr drop.body
        jsr drop.body
        rts


;;; IMMEDIATE ( -- ) Mark the most recently defined word as immediate.
        ;; : IMMEDIATE    LATEST @ C@ 0x80 OR LATEST @ C! ;
        .entry immediate, "IMMEDIATE"
        jsr latest.body
        jsr fetch.body
        jsr c_fetch.body
        jsr lit.body
        .word $80               ; set precedence bit
        jsr or.body
        jsr latest.body
        jsr fetch.body
        jsr c_store.body
        rts


;;; [ ( -- ) Enter interpretation state; [ is an immediate word.
        .entry left_bracket, "[", true
        jsr lit.body
        .word 0
        jsr state.body
        jsr store.body
        rts


;;; LATEST ( -- addr ) Return a cell containing the address of the
;;; latest dictionary entry.
        .entry latest, "LATEST"
        jsr lit.body
        .word dict_head
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


;;; LITERAL ( x -- ) At runtime, push x onto the stack.
        .entry literal, "LITERAL"
        jsr lit.body
        .word lit.body
        jsr compile_comma.body
        jsr comma.body
        rts


;;; (LOOP) ( x1 x2 -- flag x1 x2' ) Increment index x2.  Return true
;;; when x2 is equal to x1 (loop is ending).
        ;; : (LOOP)   1+ 2DUP = ;
        .entry loop_runtime, "(LOOP)"
        jsr one_plus.body
        jsr two_dup.body
        jsr equal.body
        jsr rot.body
        jsr rot.body
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


;;; NEGATE ( n -- -n )
        .entry negate, "NEGATE"
        lda 0,x
        eor #$FFFF
        inc a
        sta 0,x
        rts


;;; <> ( x1 x2 -- flag ) flag is true when x1 is not equal to x2.
        .entry not_equal, "<>"
        lda 0,x
        inx
        inx
        cmp 0,x
        bne _true
        stz 0,x
        rts
_true   lda #-1
        sta 0,x
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


;;; 1- ( n1 -- n2 ) n2 is n1 - 1.
        .entry one_minus, "1-"
        dec 0,x
        rts


;;; 1+ ( n1 -- n2 ) n2 is n1 + 1.
        .entry one_plus, "1+"
        inc 0,x
        rts


;;; OR ( x1 x2 -- x3 ) x3 is the bitwise inclusive or of x1 and x2.
        .entry or, "OR"
        lda 0,x
        ora 2,x
        sta 2,x
        inx
        inx
        rts


;;; PARSE <text> Parse a string up until the next occurrence of the
;;; given delimiter and return the address and length.
        ;; : PARSE  ( char -- addr n )
        ;;    >R                \ save char to return stack
        ;;    >IN @             \ start with current value of >IN
        ;;    \ increment value on stack until delimeter found
        ;;    BEGIN  DUP TIB + C@ R@ <>  WHILE  1+  REPEAT
        ;;    R> DROP           \ no need for char now
        ;;    DUP >IN @ - >R    \ save n on return stack
        ;;    >IN @ TIB + >R    \ save addr on return stack
        ;;    1+ >IN !          \ update >IN
        ;;    R> R>             \ get n and addr from return stack
        ;; ;
        .entry parse, "PARSE"
        jsr to_r.body
        jsr toin.body
        jsr fetch.body
_begin  jsr dup.body
        jsr lit.body
        .word tib
        jsr plus.body
        jsr c_fetch.body
        jsr r_fetch.body
        jsr not_equal.body
        jsr zero_branch.body
        .word _after
        jsr one_plus.body
        jmp _begin
_after  jsr r_from.body
        jsr drop.body
        jsr dup.body
        jsr toin.body
        jsr fetch.body
        jsr minus.body
        jsr to_r.body
        jsr toin.body
        jsr fetch.body
        jsr lit.body
        .word tib
        jsr plus.body
        jsr to_r.body
        jsr one_plus.body
        jsr toin.body
        jsr store.body
        jsr r_from.body
        jsr r_from.body
        rts


;;; + ( n1 n2 -- n3 ) Adds n1 to n2 to get n3.
        .entry plus, "+"
        lda 0,x                 ; a := n2
        clc
        adc 2,x                 ; a += n1
        sta 2,x                 ; n3 := a
        inx
        inx                     ; pop n2 off stack
        rts


;;; +! ( x addr -- ) Adds x to the current value of the cell at addr.
        .entry plus_store, "+!"
        lda 2,x
        clc
        adc (0,x)
        sta (0,x)
        txa
        clc
        adc #4
        tax
        rts


;;; POSTPONE <name> Append the compilation behavior of name to
;;; the current definition.
        ;; : POSTPONE ( -- )
        ;;    BL WORD FIND DUP  IF
        ;;       0>  IF \ name is immediate
        ;;          COMPILE,
        ;;       ELSE \ name is not immediate
        ;;          LITERAL ['] COMPILE, COMPILE,
        ;;       THEN
        ;;    ELSE
        ;;       QUIT
        ;;    THEN
        ;; ; IMMEDIATE
        .entry postpone, "POSTPONE", true
        jsr bl.body
        jsr word.body
        jsr find.body
        jsr dup.body
        jsr zero_branch.body
        .word _else1
        jsr zero_greater_than.body
        jsr zero_branch.body
        .word _else2
        jsr compile_comma.body
        jmp _then2
_else2  jsr literal.body
        jsr lit.body
        .word compile_comma.body
        jsr compile_comma.body
_then2  jmp _then1
_else1  jsr quit.body
_then1  rts


;;; QUIT ( * -- ) Clear the return and data stacks and repeatedly read
;;; and interpret a line of code.
        .entry quit, "QUIT"
        ;; Clear return and data stacks.
        ldx #init_psp
        lda #init_rsp
        tcs
        ;; Set STATE to interpretation.
        jsr left_bracket.body

        sep #FLAGM
        jsl SEND_CR
        rep #FLAGM

_loop
        jsr refill.body
        jsr interpret.body
        jsr state.body
        jsr fetch.body
        jsr zero_branch.body
        .word _print_ok
        jsr lit.body
        .word _comp
        jmp _type
_print_ok
        jsr lit.body
        .word _ok
_type   jsr typen.body
        jmp _loop

_ok     .null " ok", $0D
_comp   .null " compiled", $0D


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


;;; R@ ( S: -- x ) ( R: x -- x ) Place a copy of the item on top of
;;; the return stack onto the data stack.
        .entry r_fetch, "R@"
        lda 3,s                 ; load x
        dex
        dex
        sta 0,x                 ; push x to data stack
        rts


;;; R> ( S: -- x ) ( R: x -- ) Remove the top item from the return
;;; stack and place it on the data stack.
        .entry r_from, "R>"
        pla                     ; pull return address
        ply                     ; pull x
        dex
        dex
        sty 0,x                 ; push x to data stack
        pha                     ; push return address back
        rts                     ; return


;;; ] ( -- ) Enter compilation state.
        .entry right_bracket, "]"
        jsr lit.body
        .sint -1
        jsr state.body
        jsr store.body
        rts


;;; ; ( -- ) End the current colon definition, make it visible, and
;;; ; return to the interpretation state. ; is an immediate word.
        ;; : ;   0x60 C, LATEST @ C@ 0xBF AND LATEST @ C! [ ;
        .entry semicolon, ";", true
        jsr lit.body
        .word $60               ; opcode for RTS
        jsr c_comma.body
        jsr latest.body
        jsr fetch.body
        jsr c_fetch.body
        jsr lit.body
        .word $BF               ; clear smudge bit
        jsr and_.body
        jsr latest.body
        jsr fetch.body
        jsr c_store.body
        jsr left_bracket.body
        rts


;;; /MOD ( n1 n2 -- n3 n4 ) Divide n1 by n2, leaving the remainder n3
;;; and quotient n4.
        .entry slash_mod, "/MOD"
_dividend = tmp
_divisor = tmp+2
_quotient = tmp+4
_neg    = tmp+6                 ; negative flag (msb)

        ldy #0                  ; holds negative flag
        lda 2,x                 ; get dividend
        sta _dividend           ; save dividend
        bpl _dividend_pos       ; if positive, don't negate
        eor #$FFFF
        inc a
        sta _dividend           ; save -dividend
        tya
        eor #$8000              ; toggle negative flag
        tay
_dividend_pos
        lda 0,x                 ; get divisor
        sta _divisor            ; save divisor
        bpl _divisor_pos        ; if positive, don't negate
        eor #$FFFF
        inc a
        sta _divisor            ; save -divisor
        tya
        eor #$8000              ; toggle negative flag
        tay
_divisor_pos
        sty _neg                ; save negative flag

        ;; Shift the divisor leftwards, storing the amount shifted in
        ;; y.  Stop when divisor >= dividend (unsigned); since
        ;; dividend is at maximum $80, a nonzero divisor will always
        ;; be greater than or equal to it when shifted enough without
        ;; losing information.
        ldy #0                  ; number of shifts
        lda _divisor
_shift_loop
        cmp _dividend
        bge _end_shift          ; if divisor >= dividend, no need to
                                ; shift more
        asl a                   ; shift divisor left once
        iny                     ; count the shifts
        bra _shift_loop         ; try to shift again

_end_shift
        sta _divisor            ; save shifted divisor
        stz _quotient           ; initialize quotient to 0

        ;; Perform long division with y+1 places.
_div_loop
        lda _dividend           ; load current dividend
        sec
        sbc _divisor            ; subtract the shifted divisor, set
                                ; carry bit if dividend >= divisor
        php
        rol _quotient           ; shift carry bit into quotient (1 if
                                ; dividend >= divisor, 0 otherwise)
        plp                     ; get carry bit from subtraction
        blt _no_sub             ; if dividend<divisor, don't save the
                                ; subtracted dividend
        sta _dividend           ; save subtracted dividend
_no_sub lsr _divisor            ; shift divisor right once
        dey                     ; count shift
        bpl _div_loop           ; if y is not negative, do another
                                ; iteration

        ;; Return quotient and remainder, negated if _neg is set.
        lda _dividend           ; load remainder
        bit _neg
        bpl _no_neg_remainder   ; if _neg not set, don't negate
                                ; remainder
        eor #$FFFF
        inc a                   ; negate remainder
_no_neg_remainder
        sta 2,x                 ; store in n3
        lda _quotient           ; load quotient
        bit _neg
        bpl _no_neg_quotient    ; if _neg not set, don't negate
                                ; quotient
        eor #$FFFF
        inc a                   ; negate quotient
_no_neg_quotient
        sta 0,x                 ; store in n4

        rts


;;; STATE ( -- addr ) Returns the address of the STATE variable (flag
;;; that is true on compilation and false on interpretation).
        .entry state, "STATE"
        dex
        dex
        ldy #_val
        sty 0,x
        rts
_val    .word 0


;;; ! ( x addr -- ) Stores x into the cell at addr.
        .entry store, "!"
        lda 2,x
        sta (0,x)
        txa
        clc
        adc #4
        tax
        rts


;;; >R ( S: x -- ) ( R: -- x ) Pop the top item from the data stack
;;; and place it on the return stack.
        .entry to_r, ">R"
        ply                     ; pull return address
        lda 0,x
        inx
        inx                     ; pop x from data stack
        pha                     ; place x on return stack
        phy                     ; push return address back
        rts                     ; return


;;; 2R@ ( S: -- x1 x2 ) ( R: x1 x2 -- x1 x2 )
        .entry two_r_fetch, "2R@"
        dex
        dex
        dex
        dex
        lda 5,s                 ; a = x2
        sta 2,x
        lda 3,s                 ; a = x1
        sta 0,x
        rts


;;; 2R> ( S: -- x1 x2 ) ( R: x1 x2 -- )
        .entry two_r_from, "2R>"
        dex
        dex
        dex
        dex
        ply                     ; pull return address
        pla
        sta 0,x
        pla
        sta 2,x
        phy                     ; push return address back
        rts


;;; 2>R ( S: x1 x2 -- ) ( R: -- x1 x2 )
        .entry two_to_r, "2>R"
        ply                     ; pull return address
        lda 2,x                 ; a = x1
        pha
        lda 0,x                 ; a = x2
        pha
        phy                     ; push return address back
        inx
        inx
        inx
        inx
        rts


;;; TYPE ( c-addr u -- ) If u is greater than zero, display the
;;; character string specified by c-addr and u.
        ;; : TYPE   0 ?DO  DUP C@ EMIT 1+  LOOP  DROP ;
        ;; Expanded:
        ;; : TYPE   0 2DUP =  IF
        ;;       2DROP
        ;;    ELSE
        ;;       2>R (DO)  DUP C@ EMIT 1+  2R> (LOOP) 2>R (0BRANCH) 2R> 2DROP
        ;;    THEN  DROP ;
        .entry type, "TYPE"
        jsr lit.body
        .word 0
        jsr two_dup.body
        jsr equal.body
        jsr zero_branch.body
        .word _else
        jsr two_drop.body
        jmp _end
_else   jsr two_to_r.body
_loop   jsr dup.body
        jsr c_fetch.body
        jsr emit.body
        jsr one_plus.body
        jsr two_r_from.body
        jsr loop_runtime.body
        jsr two_to_r.body
        jsr zero_branch.body
        .word _loop
        jsr two_r_from.body
        jsr two_drop.body
_end    jsr drop.body
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


;;; >IN ( -- addr ) Return the address of a cell that contains the
;;; current offset into TIB.
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
        sta (cp_v),y            ; Store character into cp_v at offset
                                ; y
        inx
        cpx n_tib
        beq _finish             ; If end of input, found whole word
        lda TIB,x               ; Get next character
        cmp tmp+2               ; Compare with delimiter
        bne _grab_word          ; If delim, found whole word, else loop
        inx                     ; Move past delim if present

        ;; Write the length of the word into the first char and
        ;; return.
_finish
        tya
        and #$EF                ; clear msb
        sta (cp_v)              ; Store length into first char.

        stx toin_v              ; Update toin
        ldx tmp                 ; restore sp into x

        rep #FLAGM
        .al
        lda cp_v
        sta 0,x                 ; Put cp_v on stack.

        rts


;;; 0= ( n -- flag ) Return true when n == 0.
        .entry zero_equal, "0="
        lda 0,x
        beq _true
        stz 0,x                 ; if n!=0, flag = 0
        rts
_true   lda #-1
        sta 0,x                 ; if n==0, flag = -1
        rts


;;; 0< ( n -- flag ) Return true when n < 0.
        .entry zero_less_than, "0<"
        lda 0,x
        bmi _true
        stz 0,x                 ; if n>=0, flag = 0
        rts
_true   lda #-1
        sta 0,x                 ; if n<0, flag = -1
        rts


;;; 0> ( n -- flag ) Return true when n > 0.
        .entry zero_greater_than, "0>"
        jsr negate.body
        jsr zero_less_than.body ; inefficient, but easy
        rts


;;; 0<> ( n -- flag ) Return true when n is not equal to 0.
        .entry zero_not_equal, "0<>"
        lda 0,x
        bne _true
        stz 0,x                 ; if n==0, flag = 0
        rts
_true   lda #-1
        sta 0,x                 ; if n!=0, flag = -1
        rts


cp_val  = *
*       = cp_v
        .word cp_val            ; Fill default code pointer

*       = dict_head
        .word last_entry        ; Fill default dict head to last entry

