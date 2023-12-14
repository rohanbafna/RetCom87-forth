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
kbbuf   = $0200                 ; keyboard scancode buffer
kblshf  = $01                   ; left shift modifier

;;; Direct page variables
*       = $C0
cp_v    .word ?                 ; Pointer to the next cell in dict
toin_v  .word ?                 ; Current offset into TIB.
kbstate .byte ?                 ; Current state of keyboard handler
kbwoff  .byte ?                 ; Write offset of keyboard circular
                                ; buffer
kbroff  .byte ?                 ; Read offset of keyboard circular
                                ; buffer
kbpar   .byte ?                 ; Keyboard parity store
tmp                             ; Temporary storage

;;; Register keyboard interrupt handler
        * = $0104               ; Non maskable interrupt vector
        jmp kbint

;;; Main program
        * = $0300

        clc
        xce

        rep #FLAGM|FLAGX
        .al
        .xl

        ;; Reset dictionary.
        lda #cp_val
        sta cp_v
        lda #load_last_entry
        sta dict_head

        ;; Clear return and data stacks.
        ldx #init_psp
        lda #init_rsp
        tcs
        ;; Set STATE to interpretation.
        jsr left_bracket.body

        ;; Clear keyboard state.
        sep #FLAGM
        .as
        stz kbroff
        stz kbwoff
        stz kbpar
        stz kbstate

        ;; Enable keyboard interrupt by setting BCR6 to 1 and BCR5 to
        ;; 0.
        lda BCR
        ora #BCR6
        and #~BCR5
        sta BCR
        rep #FLAGM
        .al

        ;; Load boot program.
        lda #boot_t
        sta s_addr
        lda #len(boot)
        sta n_tib
        stz toin_v
        jsr interpret.body

        jmp quit.body

;;; Keyboard interrupt handler
        ;; Both X and Y *must* be saved because we are going to enter
        ;; 8-bit index mode, so if we don't save these two registers
        ;; and this interrupt was called while X was unset (i.e. most
        ;; of the time) then the upper bytes of X and Y would be lost.
        ;; I didn't save Y originally when I wrote this and it caused
        ;; lots of strange bugs!
kbint   phy
        phx
        pha
        php

        sep #FLAGM|FLAGX        ; 8-bit memory and index mode
        .as
        .xs

        ldx kbstate             ; load current state into X
        lda PD4                 ; load port 4 data into A
        and #$04                ; data bit is bit 2

        ;; Jump to handler depending on current state, with status
        ;; register based on data bit.
        jmp (_jtable,x)

        ;; Start bit: checks that data bit is 0.  If so, advances the
        ;; state; otherwise signals an error.
_start  bne _error              ; if bit read is not 0, error
        bra _next

        ;; Data bit: shifts bit read into current byte in buffer.
_data   beq _noflip             ; if data bit is 1, flip parity
        inc kbpar
_noflip txy
        ldx kbwoff              ; load write offset into X
        adc #-$04               ; C set iff data bit is 1
        ror kbbuf,x             ; rotate data bit into current byte
        tyx
        bra _next

        ;; Parity bit: errors if parity bit does not match expected
        ;; parity.
_parity adc #-$04               ; C set iff parity bit is 1
        lda kbpar
        adc #0                  ; A = parity bit + kbpar
        ;; The parity bit should be set iff an even number of data
        ;; bits were set.  Thus, counting the parity bit, an odd
        ;; number of bits should have been set, meaning that we expect
        ;; the LSB of A to be 1.
        bit #1                  ; test LSB of A
        beq _error              ; if 0, error

        ;; Go to the next bit and return.
_next   inx
        inx
        stx kbstate             ; increment state by 2 since addresses
                                ; are 2 bytes long
_ret    plp
        pla
        plx
        ply
        rti

        ;; Stop bit: errors if bit is not 1, otherwise finishes
        ;; writing the current byte.
_stop   beq _error              ; if bit read is 0, discard packet
        inc kbwoff
        ;; Fall through to error since we need to clear the next byte
        ;; and state anyways.

        ;; On an error, discard current byte and reset state.
_error  ldx kbwoff
        stz kbbuf,x             ; discard current byte
        stz kbstate             ; reset state
        stz kbpar               ; reset parity
        bra _ret                ; return from interrupt

        ;; The jump table used to select the proper handler.
_jtable .word _start
        .word _data
        .word _data
        .word _data
        .word _data
        .word _data
        .word _data
        .word _data
        .word _data
        .word _parity
        .word _stop

        .al
        .xl

;;; Load boot string into memory, convert \n to ' '.
boot    = binary("boot.fs")
boot_t  .for i:=0, i<len(boot), i+=1
        .if boot[i]==10
        .byte ' '
        .else
        .byte boot[i]
        .endif
        .endfor

;;; Each entry in the dictionary is formatted as a variable-length
;;; name field (as a counted string) followed by a 2-byte field for
;;; the link, followed by the executable code and/or data field.  The
;;; entries are laid out sequentially, and the address of the latest
;;; entry is stored in LATEST.  Each subroutine is called with JSR and
;;; returns with RTS, and should be called with and return with FLAGM
;;; and FLAGX reset.  The parameter stack pointer is stored in X.

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
;;;    RETURN STACK MANIPULATION
;;; --------------------------------

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


;;; R@ ( S: -- x ) ( R: x -- x ) Place a copy of the item on top of
;;; the return stack onto the data stack.
        .entry r_fetch, "R@"
        lda 3,s                 ; load x
        dex
        dex
        sta 0,x                 ; push x to data stack
        rts


;;; UNLOOP ( -- ) ( R: x1 x2 -- ) Drops the top two items from the
;;; return stack.
        .entry unloop, "UNLOOP"
        pla
        ply
        ply
        pha
        rts


;;; --------------------------------
;;;     PROGRAMMER CONVENIENCES
;;; --------------------------------

;;; .S ( -- ) Display the stack, for debugging purposes.
        .entry dot_s, ".S"
        ;; Print number of entries on stack.
        jsr lit.body
        .word '<'
        jsr emit.body
        jsr bl.body
        jsr emit.body
        jsr depth.body
        jsr dot.body
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


;;; ? ( addr -- ) Fetch the contents of the given address and display
;;; the result according to the current base.
        .entry question, "?"
        jsr fetch.body
        jsr dot.body
        rts


;;; --------------------------------
;;;      ARITHMETIC OPERATIONS
;;; --------------------------------

;;; + ( n1 n2 -- n3 ) Adds n1 to n2 to get n3.
        .entry plus, "+"
        lda 0,x                 ; a := n2
        clc
        adc 2,x                 ; a += n1
        sta 2,x                 ; n3 := a
        inx
        inx                     ; pop n2 off stack
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


;;; / ( n1 n2 -- n3 ) Divide n1 by n2, leaving the quotient n3.
        .entry slash, "/"
        jsr slash_mod.body
        jsr nip.body
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


;;; 1+ ( n1 -- n2 ) Add one to n1, leaving n2.
        .entry one_plus, "1+"
        inc 0,x
        rts


;;; 1- ( n1 -- n2 ) Subtract one from n1, leaving n2.
        .entry one_minus, "1-"
        dec 0,x
        rts


;;; 2+ ( n1 -- n2 ) Add two to n1, leaving n2.
        .entry two_plus, "2+"
        lda 0,x
        inc a
        inc a
        sta 0,x
        rts


;;; 2- ( n1 -- n2 ) Subtract two from n1, leaving n2.
        .entry two_minus, "2-"
        lda 0,x
        dec a
        dec a
        sta 0,x
        rts


;;; 2* ( x1 -- x2 ) Return x2, the result of shifting x1 one bit
;;; toward the most-significant bit, filling the least significant bit
;;; with zero.
        .entry two_star, "2*"
        asl 0,x
        rts


;;; 2/ ( x1 -- x2 ) Return x2, the result of shifting x1 one bit
;;; toward the least-significant bit, leaving the most-significant bit
;;; unchanged.
        .entry two_slash, "2/"
        ;; Set C flag depending on msb of x1.
        lda 0,x
        bmi _neg
        clc
        bra _shift
_neg    sec
        ;; Shift x1.
_shift  ror a
        sta 0,x
        rts


;;; LSHIFT ( x1 u -- x2 ) Perform a logical left shift of u places on
;;; x1, giving x2.  Fill the vacated least-significant bits with
;;; zeroes.
        .entry lshift, "LSHIFT"
        lda 2,x                 ; a = x1
        ldy 0,x                 ; y = u
        beq _end
_loop   asl a
        dey
        bne _loop
        sta 2,x
_end    inx
        inx
        rts


;;; MOD ( n1 n2 -- n3 ) Divide n1 by n2, giving the remainder n3.
        .entry mod, "MOD"
        jsr slash_mod.body
        jsr drop.body
        rts


;;; RSHIFT ( x1 u -- x2 ) Perform a logical right shift of u places on
;;; x1, giving x2.  Fill the vacated most-significant bits with
;;; zeroes.
        .entry rshift, "RSHIFT"
        lda 2,x                 ; a = x1
        ldy 0,x                 ; y = u
        beq _end
_loop   lsr a
        dey
        bne _loop
        sta 2,x
_end    inx
        inx
        rts


;;; --------------------------------
;;;       LOGICAL OPERATIONS
;;; --------------------------------

;;; ABS ( n -- +n ) Replace the top stack item with its absolute
;;; value.
        .entry abs, "ABS"
        lda 0,x
        bpl _end
        eor #$FFFF
        inc a
        sta 0,x
_end    rts


;;; AND ( x1 x2 -- x3 ) x3 is the bitwise and of x1 and x2.
        .entry and_, "AND"
        lda 0,x
        and 2,x
        sta 2,x
        inx
        inx
        rts


;;; INVERT ( x1 -- x2 ) Invert all bits of x1, giving its logical
;;; inverse x2.
        .entry invert, "INVERT"
        lda 0,x
        eor #$FFFF
        sta 0,x
        rts


;;; MAX ( n1 n2 -- n3 ) Return n3, the greater of n1 and n2.
        .entry max, "MAX"
        lda 2,x                 ; a = n1
        cmp 0,x                 ; compare with n2
        bpl _greater
        lda 0,x
        sta 2,x
_greater
        inx
        inx
        rts


;;; MIN ( n1 n2 -- n3 ) Return n3, the lesser of n1 and n2.
        .entry min, "MIN"
        lda 2,x                 ; a = n1
        cmp 0,x                 ; compare with n2
        bmi _less
        lda 0,x
        sta 2,x
_less   inx
        inx
        rts


;;; NEGATE ( n -- -n ) Change the sign of the top stack value.
        .entry negate, "NEGATE"
        lda 0,x
        eor #$FFFF
        inc a
        sta 0,x
        rts


;;; OR ( x1 x2 -- x3 ) x3 is the bitwise inclusive or of x1 and x2.
        .entry or, "OR"
        lda 0,x
        ora 2,x
        sta 2,x
        inx
        inx
        rts


;;; WITHIN ( x1 x2 x3 -- flag ) Return true if x1 is greater than or
;;; equal to x2 and less than x3.  The values may all be either
;;; unsigned integers or signed integers, but must all be the same
;;; type.
        .entry within, "WITHIN"
        ;; To accomodate both signednesses, use the formula x1-x2 <
        ;; x3-x2 with an unsigned comparison.
        lda 0,x                 ; a = x3
        sec
        sbc 2,x                 ; a = x3-x2
        sta tmp                 ; save a in tmp
        lda 4,x                 ; a = x1
        sec
        sbc 2,x                 ; a = x1-x2
        inx
        inx
        inx
        inx
        cmp tmp                 ; compare x1-x2 to x3-x2
        blt _less
        stz 0,x
        rts
_less   lda #-1
        sta 0,x
        rts


;;; XOR ( x1 x2 -- x3 ) x3 is the bitwise exclusive or of x1 and x2.
        .entry xor, "XOR"
        lda 0,x
        eor 2,x
        sta 2,x
        inx
        inx
        rts



;;; --------------------------------
;;;       MEMORY OPERATIONS
;;; --------------------------------

;;; ! ( x addr -- ) Stores x into the cell at addr.
        .entry store, "!"
        lda 2,x
        sta (0,x)
        txa
        clc
        adc #4
        tax
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


;;; 2! ( x1 x2 addr -- ) Store the cell pair x1 x2 in the two cells
;;; beginning at addr, removing three cells from the stack.  The order
;;; of the two cells in memory is the same as on the stack.
        .entry two_store, "2!"
        lda 0,x                 ; load addr into a
        sta tmp                 ; save addr into tmp
        lda 2,x                 ; load x2 into a
        sta (tmp)               ; save x2 into addr
        ldy #2
        lda 4,x                 ; load x1 into a
        sta (tmp),y             ; store x1 into addr+2
        txa
        clc
        adc #6
        tax                     ; pop 3 cells off stack
        rts


;;; 2@ ( addr -- x1 x2 ) Push the cell pair x1 x2 at addr onto the top
;;; of the stack.
        .entry two_fetch, "2@"
        lda 0,x                 ; load addr into a
        sta tmp                 ; save addr into tmp
        dex
        dex                     ; leave space for x2
        lda (tmp)               ; load x2
        sta 0,x                 ; store on stack
        ldy #2
        lda (tmp),y             ; load x1
        sta 2,x                 ; store on stack
        rts


;;; @ ( addr -- x ) Replace addr with the contents of the cell at
;;; addr.
        .entry fetch, "@"
        lda (0,x)
        sta 0,x
        rts


;; ;;; BLANK ( addr u -- ) Set a region of memory, at address addr of
;; ;;; length u, to ASCII blanks.
;;         .entry blank, "BLANK"
;;         lda 2,x
;;         sta tmp                 ; move addr to tmp
;;         ldy #0                  ; y stores index
;; _loop   cpy 0,x                 ; compare y to u
;;         beq _end
;;         stz (tmp),y
;;         iny
;;         bra _loop
;; _end    inx
;;         inx
;;         inx
;;         inx
;;         rts



;;; --------------------------------
;;;             STRINGS
;;; --------------------------------

;;; For the interpreter, it's easier to just write a definition to
;;; check if two strings are equal.

;; ;;; COMPARE ( addr1 u1 addr2 u2 -- n ) Compare the string specified by
;; ;;; addr1 u1 to the string specified by addr2 u2 and return a result
;; ;;; code n, which is 0 if the strings are equal, -1 if the first
;; ;;; string is less than the second string, and 1 if the second string
;; ;;; is less than the first string.
;;         ;; : COMPARE ( addr1 u1 addr2 u2 -- n )
;;         ;;    BEGIN  2 PICK ( u1 ) 0<> OVER ( u2 ) 0<> AND  WHILE
;;         ;;       3 PICK ( addr1 ) C@   ( addr1 u1 addr2 u2 c1 )
;;         ;;       2 PICK ( addr2 ) C@   ( addr1 u1 addr2 u2 c1 c2 )
;;         ;;       2DUP <  IF  
;;         .entry compare, "COMPARE"
;; _addr1  = tmp
;; _u1     = tmp+2
;; _addr2  = tmp+4
;; _u2     = tmp+6
;;         lda 6,x
;;         sta _addr1
;;         lda 4,x
;;         sta _u1
;;         lda 2,x
;;         sta _addr2
;;         lda 0,x
;;         sta 


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


;;; CMOVE ( addr1 addr2 u -- ) If u is greater than zero, copy u
;;; consecutive characters from the data space starting at addr1 to
;;; that starting at addr2, proceeding character-by-character from
;;; lower addresses to higher addresses.
        .entry cmove, "CMOVE"
        stx tmp                 ; save stack pointer
        ldy 2,x                 ; load y with addr2
        lda 4,x
        tax                     ; load x with addr1
        lda (tmp)               ; load a with u
        dec a                   ; decrement a (needed for mvn to work)
        mvn 0,0                 ; move a+1 bytes from (x) to (y)
        lda tmp
        clc
        adc #6
        tax                     ; restore x with tmp+6 (pops 3 cells
                                ; off the stack)
        rts


;;; CMOVE> ( addr1 addr2 u -- ) If u is greater than zero, copy u
;;; consecutive characters from the data space starting at addr1 to
;;; that starting at addr2, proceeding character-by-character from
;;; higher addresses to lower addresses.
        .entry cmove_up, "CMOVE>"
        stx tmp                 ; save stack pointer
        dec 0,x                 ; decrement u (needed for mvp to work)
        lda 2,x
        clc
        adc 0,x
        tay                     ; load y with addr2+u-1
        lda 4,x
        clc
        adc 0,x
        tax                     ; load x with addr1+u-1
        lda (tmp)               ; load a with u-1
        mvp 0,0                 ; move a+1 bytes from (x) to (y)
        lda tmp
        clc
        adc #6
        tax                     ; restore x with tmp+6 (pops 3 cells
                                ; off the stack)
        rts


;;; =STRING ( addr1 addr2 n -- flag ) Compare the two strings at addr1
;;; and addr2, both of length n, and return flag, which is true if the
;;; strings are equal and false if not
        .entry equal_string, "=STRING"
_str1   = tmp
_str2   = tmp+2
        lda 4,x
        sta _str1
        lda 2,x
        sta _str2
        ldy 0,x
        inx
        inx
        inx
        inx
        dey
        sep #FLAGM
        .as
_loop   lda (_str1),y
        cmp (_str2),y
        bne _not_equal
_test   dey
        bpl _loop
        ;; If fell out of loop, flag is -1.
        rep #FLAGM
        .al
        lda #-1
        sta 0,x
        rts
_not_equal
        ;; If branched to here, flag is 0.
        rep #FLAGM
        stz 0,x
        rts


;;; /STRING ( addr1 u1 n -- addr2 u2 ) Adjust the character string at
;;; addr1 u1 by n characters to get addr2 u2.
        ;; : /STRING ( addr1 u1 n -- addr2 u2 )
        ;;    DUP ROT SWAP   ( addr1 n u1 n )
        ;;    - >R + R> ;
        .entry slash_string, "/STRING"
        jsr dup.body
        jsr rot.body
        jsr swap.body
        jsr minus.body
        jsr to_r.body
        jsr plus.body
        jsr r_from.body
        rts


;;; --------------------------------
;;;           DICTIONARY
;;; --------------------------------

;;; FIND-NAME ( addr u -- addr u 0 | xt 1 | xt -1 ) Find the word
;;; given by addr u in the dictionary.  If found, return the execution
;;; token and 1 if immediate, else -1.  Otherwise return addr u and 0.
        ;; : FIND-NAME
        ;;    LATEST @   ( word len entry )
        ;;    BEGIN
        ;;       >R 2DUP R@   ( word len word len entry R: entry )
        ;;       MATCHES? R@ VISIBLE? AND  IF   ( word len R: entry )
        ;;          2DROP R@ >BODY   ( xt R: entry )
        ;;          R> NOT-IMMEDIATE? 2* 1+ EXIT   ( xt -1 | xt 1 )
        ;;       THEN   ( word len R: entry )
        ;;       R> PREV-ENTRY DUP 0=   ( word len prev flag )
        ;;    UNTIL ;   ( word len 0 )
        .entry find_name, "FIND-NAME"
        jsr latest.body
        jsr fetch.body
_begin  jsr to_r.body
        jsr two_dup.body
        jsr r_fetch.body
        jsr matches_question.body
        jsr r_fetch.body
        jsr visible_question.body
        jsr and_.body
        jsr zero_branch.body
        .word _then
        jsr two_drop.body
        jsr r_fetch.body
        jsr to_body.body
        jsr r_from.body
        jsr not_immediate_question.body
        jsr two_star.body
        jsr one_plus.body
        rts
_then   jsr r_from.body
        jsr prev_entry.body
        jsr dup.body
        jsr zero_equal.body
        jsr zero_branch.body
        .word _begin
        rts


;;; NOT-IMMEDIATE? ( addr -- flag ) Return 0 if the dictionary entry
;;; given by addr is immediate or -1 otherwise.
        ;; : NOT-IMMEDIATE?   C@ PRECEDENCE AND 0= ;
        .entry not_immediate_question, "NOT-IMMEDIATE?"
        jsr c_fetch.body
        jsr lit.body
        .word precedence
        jsr and_.body
        jsr zero_equal.body
        rts


;;; MATCHES? ( addr1 u addr2 -- flag ) Compare the string given by
;;; addr1 u to the dictionary entry given by addr2.  Flag is -1 if
;;; they match and 0 if not.
        ;; : MATCHES?
        ;;    COUNT NOCTRL AND   ( addr1 u addr2' u2 )
        ;;    ROT OVER =  IF   ( addr1 addr2' u2 )
        ;;       =STRING   ( flag )
        ;;    ELSE   ( addr1 addr2' u2 )
        ;;       2DROP DROP 0   ( 0 )
        ;;    THEN ;   ( flag )
        .entry matches_question, "MATCHES?"
        jsr count.body
        jsr lit.body
        .word noctrl
        jsr and_.body
        jsr rot.body
        jsr over.body
        jsr equal.body
        jsr zero_branch.body
        .word _else
        jsr equal_string.body
        jmp _then
_else   jsr two_drop.body
        jsr drop.body
        jsr lit.body
        .sint 0
_then   rts


;;; LATEST ( -- addr ) Return a cell containing the address of the
;;; latest dictionary entry.
        .entry latest, "LATEST"
        jsr lit.body
        .word dict_head
        rts
dict_head .word 0


;;; PREV-ENTRY ( addr -- addr' | 0 ) Replace addr, which points to a
;;; dictionary entry, with a pointer to the previous dictionary entry,
;;; or 0 if addr was the first dictionary entry.
        ;; : PREV-ENTRY   COUNT NOCTRL AND + @ ;
        .entry prev_entry, "PREV-ENTRY"
        jsr count.body
        jsr lit.body
        .word noctrl
        jsr and_.body
        jsr plus.body
        jsr fetch.body
        rts


;;; >BODY ( addr -- addr ) Replace addr, which points to a dictionary
;;; entry, with a pointer to the body of the entry.
        ;; : >BODY   COUNT NOCTRL AND + 2 + ;
        .entry to_body, ">BODY"
        jsr count.body
        jsr lit.body
        .word noctrl
        jsr and_.body
        jsr plus.body
        jsr lit.body
        .sint 2
        jsr plus.body
        rts


;;; VISIBLE? ( addr -- flag ) Return 0 if the dictionary entry
;;; given by addr is smudged or -1 otherwise.
        ;; : VISIBLE?   C@ SMUDGE AND 0= ;
        .entry visible_question, "VISIBLE?"
        jsr c_fetch.body
        jsr lit.body
        .word smudge
        jsr and_.body
        jsr zero_equal.body
        rts


;;; --------------------------------
;;;              I/O
;;; --------------------------------

;;; EKEY ( -- x ) Waits for a keyboard event.
        .entry ekey, "EKEY"
        stx tmp
        sep #FLAGM|FLAGX
        .as
        .xs
        ldy kbroff              ; Y gets the read offset

        ;; If the write offset is not equal to the read offset, then
        ;; there is a new key event to be read.
_loop   cpy kbwoff
        bne _newev
        wai                     ; wait for interrupt
        bra _loop

        ;; Read the new key event.
_newev  lda kbbuf,y             ; A gets the latest key event
        iny
        sty kbroff              ; Increment read offset

        ;; Push the key event on the stack.
        rep #FLAGM|FLAGX
        .al
        .xl
        ldx tmp
        dex
        dex
        and #$FF
        sta 0,x
        rts

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


;;; KEY ( -- char ) Wait for keyboard input and return the character
;;; corresponding to the key pressed.
        .entry key, "KEY"
        sep #FLAGM
        .as
        jsl GET_CHR
        rep #FLAGM
        .al
        and #$FF
        dex
        dex
        sta 0,x
        rts


;;; KEYMOD ( -- addr ) Returns the address of a cell containing the
;;; current modifier bits for the keyboard.  (Only left-shift is used
;;; right now.)
        .entry keymod, "KEYMOD"
        jsr lit.body
        .word _val
        rts
_val    .word 0


;;; PS2KEY ( -- char ) Wait for PS/2 keyboard input and return the
;;; character corresponding to the key pressed.
        ;; : PS2KEY
        ;;    BEGIN
        ;;       EKEY
        ;;       \ if shift, set modifier bit
        ;;       DUP $12 =  IF  KEYMOD @ 1 OR KEYMOD ! DROP 0  THEN
        ;;       \ if F0 (release), read next scancode and unset
        ;;       \ modifier bit or ignore
        ;;       DUP $F0 =  IF
        ;;          DROP EKEY $12 =  IF  KEYMOD @ $FE AND KEYMOD !  THEN 0
        ;;       THEN
        ;;       KEYMOD @ 1 AND  IF  SSCODE  ELSE  SCODE  THEN  + C@ ?DUP
        ;;    UNTIL ;
        .entry pstwo_key, "PS2KEY"
_begin  jsr ekey.body
        jsr dup.body
        jsr lit.body
        .word $12
        jsr equal.body
        jsr zero_branch.body
        .word _then1
        jsr keymod.body
        jsr fetch.body
        jsr lit.body
        .sint 1
        jsr or.body
        jsr keymod.body
        jsr store.body
        jsr drop.body
        jsr lit.body
        .word 0
_then1  jsr dup.body
        jsr lit.body
        .word $F0
        jsr equal.body
        jsr zero_branch.body
        .word _then2
        jsr drop.body
        jsr ekey.body
        jsr lit.body
        .word $12
        jsr equal.body
        jsr zero_branch.body
        .word _then3
        jsr keymod.body
        jsr fetch.body
        jsr lit.body
        .word $FE
        jsr and_.body
        jsr keymod.body
        jsr store.body
_then3  jsr lit.body
        .sint 0
_then2  jsr keymod.body
        jsr fetch.body
        jsr lit.body
        .word 1
        jsr and_.body
        jsr zero_branch.body
        .word _else4
        jsr lit.body
        .word _sscode
        jmp _then4
_else4  jsr lit.body
        .word _scode
_then4  jsr plus.body
        jsr c_fetch.body
        jsr question_dup.body
        jsr zero_branch.body
        .word _begin
        rts

        ;; Table mapping PS/2 scancodes to ASCII.  0 indicates an
        ;; unmapped key.
        ;;     0   1   2   3   4   5   6   7   8   9   A   B   C   D   E   F
_scode  .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$09,'`',$00 ; 0
        .byte $00,$00,$00,$00,$00,'q','1',$00,$00,$00,'z','s','a','w','2',$00 ; 1
        .byte $00,'c','x','d','e','4','3',$00,$00,' ','v','f','t','r','5',$00 ; 2
        .byte $00,'n','b','h','g','y','6',$00,$00,$00,'m','j','u','7','8',$00 ; 3
        .byte $00,',','k','i','o','0','9',$00,$00,'.','/','l',';','p','-',$00 ; 4
        .byte $00,$00,$27,$00,'[','=',$00,$00,$00,$00,$0D,']',$00,'\',$00,$00 ; 5
        .byte $00,$00,$00,$00,$00,$00,$08,$00,$00,$00,$00,$00,$00,$00,$00,$00 ; 6
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,'-','*',$00,$00,$00 ; 7
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00 ; 8
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00 ; 9
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00 ; A
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00 ; B
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00 ; C
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00 ; D
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00 ; E
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00 ; F
        ;; Equivalent table for Shift
        ;;     0   1   2   3   4   5   6   7   8   9   A   B   C   D   E   F
_sscode .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$09,'~',$00 ; 0
        .byte $00,$00,$00,$00,$00,'Q','!',$00,$00,$00,'Z','S','A','W','@',$00 ; 1
        .byte $00,'C','X','D','E','$','#',$00,$00,' ','V','F','T','R','5',$00 ; 2
        .byte $00,'N','B','H','G','Y','^',$00,$00,$00,'M','J','U','&','*',$00 ; 3
        .byte $00,'<','K','I','O',')','(',$00,$00,'>','?','L',':','P','_',$00 ; 4
        .byte $00,$00,'"',$00,'{','+',$00,$00,$00,$00,$0D,'}',$00,'|',$00,$00 ; 5"
        .byte $00,$00,$00,$00,$00,$00,$08,$00,$00,$00,$00,$00,$00,$00,$00,$00 ; 6
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,'-','*',$00,$00,$00 ; 7
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00 ; 8
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00 ; 9
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00 ; A
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00 ; B
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00 ; C
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00 ; D
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00 ; E
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00 ; F

;;; REFILL ( -- ) Get a line of characters and store it in the TIB,
;;; then set >IN to 0.
        ;; : REFILL
        ;;    TIB DUP   ( tib addr )
        ;;    BEGIN   ( tib addr )
        ;;       KEY DUP 13 <>   ( tib addr char flag )
        ;;    WHILE   ( tib addr char )
        ;;       DUP EMIT DUP 8 =   ( tib addr char flag )
        ;;       IF  DROP 1- TIB MAX  ELSE  OVER ! 1+  THEN   ( tib addr' )
        ;;    REPEAT   ( tib addr char )
        ;;    DROP OVER - (SOURCE) 2! ( )
        ;;    0 >IN ! ;
        .entry refill, "REFILL"
        jsr lit.body
        .word tib
        jsr dup.body
_begin  jsr pstwo_key.body
        jsr dup.body
        jsr lit.body
        .sint 13
        jsr not_equal.body
        jsr zero_branch.body
        .word _end
        jsr dup.body
        jsr emit.body
        jsr dup.body
        jsr lit.body
        .sint 8
        jsr equal.body
        jsr zero_branch.body
        .word _else
        jsr drop.body
        jsr one_minus.body
        jsr lit.body
        .word tib
        jsr max.body
        jmp _then
_else   jsr over.body
        jsr store.body
        jsr one_plus.body
_then   jmp _begin
_end    jsr drop.body
        jsr over.body
        jsr minus.body
        jsr source_.body
        jsr two_store.body
        jsr lit.body
        .sint 0
        jsr toin.body
        jsr store.body
        rts


;;; --------------------------------
;;;          INTERPRETER
;;; --------------------------------

;;; ABORT ( i * x -- ) ( R: j * x -- ) Empty the data stack and call
;;; quit.
        .entry abort, "ABORT"
        ldx #init_psp
        jsr quit.body

;;; BL ( -- c ) Push an ASCII space onto the stack.
        .entry bl, "BL"
        jsr lit.body
        .word ' '
        rts


;;; INTERPRET ( -- ? ) Interpret or compile a line of code.
        ;; BEGIN  PARSE-NAME ?DUP  WHILE
        ;;    FIND-NAME ?DUP  IF
        ;;       STATE @  IF
        ;;          0>  IF  EXECUTE  ELSE  COMPILE,  THEN
        ;;       ELSE
        ;;          DROP EXECUTE
        ;;       THEN
        ;;    ELSE
        ;;       NUMBER
        ;;       STATE @  IF  POSTPONE LITERAL  THEN
        ;;    THEN
        ;; AGAIN
        ;; DROP ;
        .entry interpret, "INTERPRET"
_loop   jsr parse_name.body
        jsr question_dup.body
        jsr zero_branch.body
        .word _end
        jsr find_name.body
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
_else1  jsr number.body
        jsr state.body
        jsr fetch.body
        jsr zero_branch.body
        .word _then4
        jsr literal.body
_then4
_then1  jmp _loop
_end    jsr drop.body
        rts


;;; PARSE ( <text> char -- addr n ) Parse a string up until the next
;;; occurrence of the given delimiter and return the address and
;;; length.
        .entry parse, "PARSE"
_src    = tmp
_savey  = tmp+2
_char   = tmp+4
        lda 0,x                 ; load char
        sta _char               ; save char
        lda s_addr              ; load source address
        sta _src                ; save source address
        ldy toin_v              ; load >IN value to y
        sty _savey              ; save y
        clc
        adc _savey
        sta 0,x                 ; store _src+y on stack
        dey
        sep #FLAGM              ; 8-bit memory mode
        .as
_parseloop
        iny                     ; go to next char in source
        cpy n_tib
        bge _parseend           ; if last char in source, stop
        lda (_src),y            ; read current character
        cmp _char
        bne _parseloop          ; if char has not been reached, loop
        iny                     ; go to character after char
        sty toin_v              ; store in >IN
        dey                     ; go back
        bra _n                  ; don't store y in >IN again
_parseend
        sty toin_v              ; store last index into >IN
_n
        rep #FLAGM              ; back to 16-bit memory mode
        .al
        dex
        dex
        tya
        sec
        sbc tmp+2
        sta 0,x                 ; push current value of y minus old
                                ; value of y onto stack
        rts


;;; PARSE-NAME ( <text> -- addr n ) Parse a space-separated name and
;;; return the address and length.
        .entry parse_name, "PARSE-NAME"
        ;; Look for first character in string != ' '
        lda s_addr
        sta tmp
        ldy toin_v
        dey
        sep #FLAGM
        .as
_skipspaceloop
        iny
        cpy n_tib
        bge _skipspaceend
        lda (tmp),y
        cmp #' '
        beq _skipspaceloop
_skipspaceend
        rep #FLAGM
        .al
        sty toin_v              ; Update >IN so PARSE knows where to
                                ; start
        ;; Call PARSE to finish the job.
        jsr bl.body
        jsr parse.body
        rts


;;; QUIT ( * -- ) Clear the return and data stacks and repeatedly read
;;; and interpret a line of code.
        .entry quit, "QUIT"
        ;; Clear return stack.
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

_ok     .null "  ok", $0D
_comp   .null "  compiled", $0D


;;; SOURCE ( -- addr u ) Return the address and length of the input
;;; buffer.
        ;; : SOURCE   (SOURCE) 2@ ;
        .entry source, "SOURCE"
        jsr source_.body
        jsr two_fetch.body
        rts


;;; (SOURCE) ( -- addr ) Return the address of two cells with the
;;; first containing the length of the input buffer and the second
;;; containing the address of the input buffer.
        .entry source_, "(SOURCE)"
        jsr lit.body
        .word _data
        rts
_data
n_tib   .word 0
s_addr  .word tib


;;; >IN ( -- addr ) Return the address of a cell that contains the
;;; current offset into TIB.
        .entry toin, ">IN"
        jsr lit.body
        .word toin_v
        rts



;;; --------------------------------
;;;         UNSORTED WORDS
;;; --------------------------------


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
        ;; : :
        ;;    HERE   ( old-cp )
        ;;    PARSE-NAME ( old-cp word len )
        ;;    \ Create definition stub.
        ;;    DUP smudge OR C,   \ count byte ( old-cp word len )
        ;;    >R HERE R@ CMOVE   \ name field ( old-cp R: len )
        ;;    R> CP +!   \ move cp forwards to account for name ( old-cp )
        ;;    LATEST @ , \ link field ( old-cp )
        ;;    LATEST ! \ update latest with address of new word ( )
        ;;    \ Enter compilation mode.
        ;;    ] ;   ( )
        .entry colon, ":"
        jsr here.body
        jsr parse_name.body
        jsr dup.body
        jsr lit.body
        .word smudge
        jsr or.body
        jsr c_comma.body
        jsr to_r.body
        jsr here.body
        jsr r_fetch.body
        jsr cmove.body
        jsr r_from.body
        jsr cp.body
        jsr plus_store.body
        jsr latest.body
        jsr fetch.body
        jsr comma.body
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


;;; CP ( -- addr ) Return the address of CP.
        .entry cp, "CP"
        jsr lit.body
        .word cp_v
        rts


;;; . ( n -- ) Print out n to the screen.
        ;; \ Print a space before the number.
        ;; BL EMIT
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
        ;; BEGIN  ?DUP  WHILE  EMIT  AGAIN ;
        .entry dot, "."
        jsr bl.body
        jsr emit.body
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
_again  rts


;;; .( ( "ccc<paren>" -- ) Parse and display ccc delimited by ).
        ;; : .(   $29 PARSE TYPE ;
        .entry dot_paren, ".("
        jsr lit.body
        .word $29
        jsr parse.body
        jsr type.body
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


;;; HERE ( -- addr ) Return the next available address in the
;;; dictionary.
        .entry here, "HERE"
        dex
        dex
        lda cp_v
        sta 0,x
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


;;; < ( n1 n2 -- flag ) Compare n1 and n2.  If n1 is less than n2 then
;;; flag is -1, otherwise flag is 0.
        .entry less_than, "<"
        jsr minus.body
        jsr zero_less_than.body
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
        .entry literal, "LITERAL", true
        jsr lit.body
        .word lit.body
        jsr compile_comma.body
        jsr comma.body
        rts


;;; (LOOP) ( x1 x2 -- x1 x2' flag ) Increment index x2.  Return true
;;; when x2 is equal to x1 (loop is ending).
        ;; : (LOOP)   1+ 2DUP = ;
        .entry loop_runtime, "(LOOP)"
        jsr two_dup.body
        jsr equal.body
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
        jsr abort.body
_err_msg .null "Could not find word in dictionary"


;;; POSTPONE <name> Append the compilation behavior of name to
;;; the current definition.
        ;; : POSTPONE ( -- )
        ;;    PARSE-NAME FIND-NAME DUP  IF
        ;;       0>  IF \ name is immediate
        ;;          COMPILE,
        ;;       ELSE \ name is not immediate
        ;;          POSTPONE LITERAL ['] COMPILE, COMPILE,
        ;;       THEN
        ;;    ELSE
        ;;       QUIT
        ;;    THEN
        ;; ; IMMEDIATE
        .entry postpone, "POSTPONE", true
        jsr parse_name.body
        jsr find_name.body
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


;;; (SLITERAL) ( -- addr u ) Takes the following data in the current
;;; thread to be a counted string, and pushes the address and length
;;; onto the stack.
        .entry sliteral_, "(SLITERAL)"
        lda 1,s
        inc a
        dex
        dex
        sta 0,x                 ; push address of counted string onto
                                ; stack.
        jsr count.body
        pla
        inc a
        clc
        adc 0,x
        pha                     ; increment return address by count+1
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


;;; TYPE ( c-addr u -- ) If u is greater than zero, display the
;;; character string specified by c-addr and u.
        ;; : TYPE   0 ?DO  DUP C@ EMIT 1+  LOOP  DROP ;
        .entry type, "TYPE"
        jsr lit.body
        .word 0
        jmp _loop
_do     jsr two_to_r.body
        jsr dup.body
        jsr c_fetch.body
        jsr emit.body
        jsr one_plus.body
        jsr two_r_from.body
        jsr one_plus.body
_loop   jsr loop_runtime.body
        jsr zero_branch.body
        .word _do
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


cp_val  = *                     ; Current pointer at load time
load_last_entry = last_entry    ; Last entry at load time
