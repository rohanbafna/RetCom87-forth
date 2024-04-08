: CHAR   PARSE-NAME DROP C@ ;
: [CHAR]   CHAR POSTPONE LITERAL ; IMMEDIATE

: (   [CHAR] ) PARSE 2DROP ; IMMEDIATE

( At compile-time, compile the string specified by addr1 u to the
current definition.  At run-time, addr2 u specifies the compiled
string. )
: SLITERAL ( ct: addr1 u --  rt: -- addr2 u )
   POSTPONE (SLITERAL)
   DUP C,
   >R HERE R@ CMOVE
   R> CP +!
; IMMEDIATE

( Parse and compile ccc delimited by ".  At run-time, addr u specifies
the compiled string. )
: S" ( "ccc<quote>" -- addr u )
   [CHAR] " PARSE POSTPONE SLITERAL
; IMMEDIATE

: ."   POSTPONE S" POSTPONE VDP-TYPE ; IMMEDIATE

: IF   POSTPONE 0BRANCH HERE 0 , ; IMMEDIATE
: THEN   HERE SWAP ! ; IMMEDIATE
: ELSE   POSTPONE BRANCH HERE 0 , SWAP POSTPONE THEN ; IMMEDIATE

: DO   HERE POSTPONE 2>R ; IMMEDIATE
: LOOP   POSTPONE 2R> POSTPONE 1+ POSTPONE (LOOP) POSTPONE 0BRANCH ,
   POSTPONE 2DROP ; IMMEDIATE
: I   POSTPONE R@ ; IMMEDIATE

( write ASCII table to screen )
: WRITEASCII   256 0 DO  I VDPDATA C!  LOOP ;

CLEAR-VRAM LOAD-FONT VDP-INIT 0 VDP-POS

: CLEAR   0 VDP-POS  960 0 DO  BL EMIT  LOOP  0 VDP-POS ;

CLEAR .( FORTH booted successfully ) CR QUIT