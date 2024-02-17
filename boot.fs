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

: ."   POSTPONE S" POSTPONE TYPE ; IMMEDIATE

: IF   POSTPONE 0BRANCH HERE 0 , ; IMMEDIATE
: THEN   HERE SWAP ! ; IMMEDIATE
: ELSE   POSTPONE BRANCH HERE 0 , SWAP POSTPONE THEN ; IMMEDIATE

: DO   HERE POSTPONE 2>R ; IMMEDIATE
: LOOP   POSTPONE 2R> POSTPONE 1+ POSTPONE (LOOP) POSTPONE 0BRANCH ,
   POSTPONE 2DROP ; IMMEDIATE
: I   POSTPONE R@ ; IMMEDIATE

: VDPREG   57281 ;
: VDPDATA  57280 ;

( clear vdp ram -- just for the part used by text mode )

: CLEAR-VRAM   0 VDPREG C!  64 VDPREG C!
   3008 0 DO  0 VDPDATA C!  LOOP ;
CLEAR-VRAM

( initialize vdp registers )

: VDPREG-INIT
   0   VDPREG C!  128 VDPREG C!
   208 VDPREG C!  129 VDPREG C!
   2   VDPREG C!  130 VDPREG C!
   128 VDPREG C!  131 VDPREG C!
   0   VDPREG C!  132 VDPREG C!
   32  VDPREG C!  133 VDPREG C!
   0   VDPREG C!  134 VDPREG C!
   240 VDPREG C!  135 VDPREG C! ;
VDPREG-INIT

: LOAD-FONT  0 VDPREG C!  65 VDPREG C!
   FONT 0 DO  DUP C@ VDPDATA C! 1+  LOOP DROP ;
LOAD-FONT

: VDP-POS ( pos -- )  18432 +  DUP VDPREG C!  8 RSHIFT VDPREG C! ;

( write ASCII table to screen )
: WRITEASCII   256 0 DO  I VDPDATA C!  LOOP ;

: VDP-TYPE ( addr u -- )   0 DO DUP C@ VDPDATA C! 1+ LOOP ;

: CR   10 EMIT ;
CR
.( FORTH booted successfully )
