;W65C265 equates -- using dollar sign prefix to indicate hex
;Adapted from the datasheet and monitor ROM manual by Aaron Lanterman, Oct. 19, 2021
;Updated to include status flag masks by Rohan Bafna, Oct. 27, 2023
;This file dedicated to the public domain

;2.4.1 Emulation Mode Priority Encoded Interrupt Vector Module
;NOTE YOU PROBABLY DON'T WANT EMULATION MODE (this is NOT "65C02" emulation)
EIRQBRK      =    $FFFE     ;BRK - Software Interrupt
EIRQRES      =    $FFFC     ;RES - "REStart" Interrupt  
EIRQNMI      =    $FFFA     ;Non-Maskable Interrupt
EIABORT      =    $FFF8     ;ABORT Interrupt
EIRQCOP      =    $FFF4     ;COP Software Interrupt
EIRQAT3      =    $FFEE     ;UART3 Transmitter Interrupt 
EIRQAR3      =    $FFEC     ;UART3 Receiver Interrupt 
EIRQAT2      =    $FFEA     ;UART2 Transmitter Interrupt
EIRQAR2      =    $FFE8     ;UART2 Receiver Interrupt
EIRQAT1      =    $FFE6     ;UART1 Transmitter Interrupt
EIRQAR1      =    $FFE4     ;UART1 Receiver Interrupt
EIRQAT0      =    $FFE2     ;UART0 Transmitter Interrupt
EIRQAR0      =    $FFE0     ;UART0 Receiver Interrupt
EIRQ         =    $FFDE     ;IRQ Level Interrupt
EIRQPIB      =    $FFDC     ;Parallel Interface Bus (PIB) 
EIRNE66      =    $FFDA     ;Interrupt Negative Edge Interrupt on P66 
EIRNE64      =    $FFD8     ;Negative Edge Interrupt on P64
EIRPE62      =    $FFD6     ;Positive Edge Interrupt on P62 for PWM 
EIRPE60      =    $FFD4     ;Positive Edge Interrupt on P60 
EIRNE57      =    $FFD2     ;Negative Edge Interrupt on P57 
EIRPE56      =    $FFD0     ;Positive Edge Interrupt on P56 
EIRQT7       =    $FFCE     ;Timer 7 Interrupt
EIRQT6       =    $FFCC     ;Timer 6 Interrupt
EIRQT5       =    $FFCA     ;Timer 5 Interrupt
EIRQT4       =    $FFC8     ;Timer 4 Interrupt
EIRQT3       =    $FFC6     ;Timer 3 Interrupt
EIRQT2       =    $FFC4     ;Timer 2 Interrupt
EIRQT1       =    $FFC2     ;Timer 1 Interrupt
EIRQT0       =    $FFC0     ;Timer 0 Interrupt

;2.4.2 Native Mode Priority Encoded Interrupt Vector Module
;NOTE YOU PROBABLY WANT NATIVE MODE ("native" here does NOT indicate "65C816" mode,
;the 65C02/65C816 selection is something else entirely)
IRQNMI      =     $FFBA     ;Non-Maskable Interrupt
IABORT      =     $FFB8     ;ABORT Interrupt
IRQBRK      =     $FFB6     ;BRK Software Interrupt 
IRQCOP      =     $FFB4     ;COP Software Interrupt 
IRQAT3      =     $FFAE     ;UART3 Transmitter Interrupt 
IRQAR3      =     $FFAC     ;UART3 Receiver Interrupt
IRQAT2      =     $FFAA     ;UART2 Transmitter Interrupt 
IRQAR2      =     $FFA8     ;UART2 Receiver Interrupt
IRQAT1      =     $FFA6     ;UART1 Transmitter Interrupt
IRQAR1      =     $FFA4     ;UART1 Receiver Interrupt
IRQAT0      =     $FFA2     ;UART0 Transmitter Interrupt
IRQAR0      =     $FFA0     ;UART0 Receiver Interrupt
IRQ         =     $FF9E     ;IRQ Level Interrupt
IRQPIB      =     $FF9C     ;Parallel Interface Bus (PIB) Interrupt 
IRNE66      =     $FF9A     ;Negative Edge Interrupt on P66 
IRNE64      =     $FF98     ;Negative Edge Interrupt on P64 
IRPE62      =     $FF96     ;Positive Edge Interrupt on P62 
IRPE60      =     $FF94     ;Positive Edge Interrupt on P60
IRNE57      =     $FF92     ;Negative Edge Interrupt on P57 
IRPE56      =     $FF90     ;Positive Edge Interrupt on P56 
IRQT7       =     $FF8E     ;Timer 6 Interrupt
IRQT6       =     $FF8C     ;Timer 6 Interrupt
IRQT5       =     $FF8A     ;Timer 5 Interrupt
IRQT4       =     $FF88     ;Timer 4 Interrupt
IRQT3       =     $FF86     ;Timer 3 Interrupt
IRQT2       =     $FF84     ;Timer 2 Interrupt
IRQT1       =     $FF82     ;Timer 1 Interrupt
IRQT0       =     $FF80     ;Timer 0 Interrupt

;2.5.1 Communication Register Memory Map
PIR7        =     $DF7F   ;Parallel Interface Register 7 (uninit)
PIR6        =     $DF7E   ;Parallel Interface Register 6 (uninit)
PIR5        =     $DF7D   ;Parallel Interface Register 5 (uninit)
PIR4        =     $DF7C   ;Parallel Interface Register 4 (uninit)
PIR3        =     $DF7B   ;Parallel Interface Register 3 (uninit)
PIR2        =     $DF7A   ;Parallel Interface Register 2 (uninit)
PIBER       =     $DF79   ;Parallel Interface Enable Register ($00)
PIBFR       =     $DF78   ;Parallel Interface Flag Register ($00)
ARTD3       =     $DF77   ;UART 3 Data Register (uninit) 
ACSR3       =     $DF76   ;UART 3 Control/Status Register ($00)
ARTD2       =     $DF75   ;UART 2 Data Register (uninit) 
ACSR2       =     $DF74   ;UART 2 Control/Status Register ($00)
ARTD1       =     $DF73   ;UART 1 Data Register (uninit) 
ACSR1       =     $DF72   ;UART 1 Control/Status Register ($00)
ARTD0       =     $DF71   ;UART 0 Data Register (uninit)
ACSRO       =     $DF70   ;UART 0 Control/Status Register ($00)

;2.5.2 Timer Register Memory Map (all uninit)
T7CH        =     $DF6F   ;Timer 7 Counter High 
T7CL        =     $DF6E   ;Timer 7 Counter Low 
T6CH        =     $DF6D   ;Timer 6 Counter High 
T6CL        =     $DF6C   ;Timer 6 Counter Low 
T5CH        =     $DF6B   ;Timer 5 Counter High
T5CL        =     $DF6A   ;Timer 5 Counter Low 
T4CH        =     $DF69   ;Timer 4 Counter High
T4CL        =     $DF68   ;Timer 4 Counter Low
T3CH        =     $DF67   ;Timer 3 Counter High
T3CL        =     $DF66   ;Timer 3 Counter Low
T2CH        =     $DF65   ;Timer 2 Counter High 
T2CL        =     $DF64   ;Timer 2 Counter Low
T1CH        =     $DF63   ;Timer 1 Counter High
T1CL        =     $DF62   ;Timer 1 Counter Low
T0CH        =     $DF61   ;Timer 0 Counter High
T0CL        =     $DF60   ;Timer 0 Counter Low
T7LH        =     $DF5F   ;Timer 7 Latch High 
T7LL        =     $DF5E   ;Timer 7 Latch Low 
T6LH        =     $DF5D   ;Timer 6 Latch High 
T6LL        =     $DF5C   ;Timer 6 Latch Low
T5LH        =     $DF5B   ;Timer 5 Latch High 
T5LL        =     $DF5A   ;Timer 5 Latch Low
T4LH        =     $DF59   ;Timer 4 Latch High 
T4LL        =     $DF58   ;Timer 4 Latch Low
T3LH        =     $DF57   ;Timer 3 Latch High 
T3LL        =     $DF56   ;Timer 3 Latch Low
T2LH        =     $DF55   ;Timer 2 Latch High 
T2LL        =     $DF54   ;Timer 2 Latch Low
T1LH        =     $DF53   ;Timer 1 Latch High 
T1LL        =     $DF52   ;Timer 1 Latch Low
T0LH        =     $DF51   ;Timer 0 Latch High 
T0LL        =     $DF50   ;Timer 0 Latch Low

;2.5.3 Control and Status Register Memory Map
UIER        =     $DF49   ;UART Interrupt Enable Register ($00)
UIFR        =     $DF48   ;UART Interrupt Flag Register ($00)
EIER        =     $DF47   ;Edge Interrupt Enable Register ($00)
TIER        =     $DF46   ;Timer Interrupt Enable Register ($00)
EIFR        =     $DF45   ;Edge Interrupt Flag Register ($00)
TIFR        =     $DF44   ;Timer Interrupt Flag Register ($00)
TER         =     $DF43   ;Timer Enable Register ($00)
TCR         =     $DF42   ;Timer Control Register ($00)
SSCR        =     $DF41   ;System Speed Control Register ($00)
BCR         =     $DF40   ;Bus Control Register ($00/$89)

;2.5.4 I/O Register Memory Map
PCS7        =     $DF27   ;Port 7 Chip Select
PDD6        =     $DF26   ;Port 6 Data Direction Register
PDD5        =     $DF25   ;Port 5 Data Direction Register
PDD4        =     $DF24   ;Port 4 Data Direction Register
PD7         =     $DF23   ;Port 7 Data Register
PD6         =     $DF22   ;Port 6 Data Register
PD5         =     $DF21   ;Port 5 Data Register
PD4         =     $DF20   ;Port 4 Data Register
PDD3        =     $DF07   ;Port 3 Data Direction Register
PDD2        =     $DF06   ;Port 2 Data Direction Register
PDD1        =     $DF05   ;Port 1 Data Direction Register
PDD0        =     $DF04   ;Port 0 Data Direction Register
PD3         =     $DF03   ;Port 3 Data Register
PD2         =     $DF02   ;Port 2 Data Register
PD1         =     $DF01   ;Port 1 Data Register
PD0         =     $DF00   ;Port 0 Data Register

;2.6.1 Bus Control Register (BCR) Description - BIT FIELD
BCR7        =     $80     ;1 = External ROM (BE controls shown in Table 1-1)
                            ;0 = Internal ROM (BE Controls shown in Table 1-1)
BCR6        =     $40     ;1 = Enable NMIB on P40 Input is level sensitive,
                            ;NIMB and ABORTB cannot both be enabled at the same time.
                            ;0 = Disable NMIB
BCR5        =     $20     ;1 = Enable ABORTB on P40 Input is level sensitive, 
                            ;NMIB and ABORTB cannot both be enabled at the same time.
                            ;0 = Disable ABORTB
BCR4        =     $10     ;1 = Watch Dog Enabled, 0 = Watch Dog Disabled
BCR3        =     $08     ;1 = Emulation Mode, RUN=Run, BA=BA/1 All on-chip addressed 
                            ;memory or I/O for reads or writes are output on the data 
                            ;bus (this is the emulation mode of operation)
                            ;0 = Normal Operation RUN=RUN, BA=BA
BCR2        =     $04     ;1 = Enable TG1, 0 = Disable TG1
BCR1        =     $02     ;1 = Enable TG0, 0 = Disable TG0
BCR0        =     $01     ;1 = Ports 0,1,2,3 are address and data bus pins
                            ;0 = Ports 0,1,2,3 are I/O pins

;2.7.2 Timer Control Register (TCR) Description - BIT FIELD
TCR7        =     $80     ;1 = UART3 Timer 4 Selected, 1 = UART3 Timer 3 Selected
TCR6        =     $40     ;1 = UART2 Timer 4 Selected, 1 = UART2 Timer 3 Selected
TCR5        =     $20     ;1 = UART1 Timer 4 Selected, 1 = UART1 Timer 3 Selected
TCR4        =     $10     ;1 = UART0 Timer 4 Selected, 1 = UART0 Timer 3 Selected
TCR3        =     $08     ;1 = PWM Measurement on P62 Negative Edges 
TCR2        =     $04     ;1 = PWM Measurement on P62 Positive Edges 
                            ;can set both TCR2 and TCR3 to "1" to measure on BOTH edges
TCR1        =     $02     ;1 = Enable Timer 4 on P61, 0 = Disable Timer 4 on P61
TCR0        =     $01     ;1 = Timer 4 Clock Selected on P60, 0 = Timer 4 FCLK Selected

;2.7.3 Timer Enable Register (TER) Description - BIT FIELD
TER7        =     $80     ;1 = Timer 7 Enabled, 0 = Timer 7 Disabled
TER6        =     $40     ;1 = Timer 6 Enabled, 0 = Timer 6 Disabled
TER5        =     $20     ;1 = Timer 5 Enabled, 0 = Timer 5 Disabled
TER4        =     $10     ;1 = Timer 4 Enabled, 0 = Timer 4 Disabled
TER3        =     $08     ;1 = Timer 3 Enabled, 0 = Timer 3 Disabled
TER2        =     $04     ;1 = Timer 2 Enabled, 0 = Timer 2 Disabled
TER1        =     $02     ;1 = Timer 1 Enabled, 0 = Timer 1 Disabled
TER0        =     $01     ;1 = Timer 0 Enabled, 0 = Timer 0 Disabled

;2.8.1 Timer Interrupt Enable Register (TIER) Description - BIT FIELD
TIER7       =     $80     ;1 = Timer 7 Enabled, 0 = Timer 7 Disabled
TIER6       =     $40     ;1 = Timer 6 Enabled, 0 = Timer 6 Disabled
TIER5       =     $20     ;1 = Timer 5 Enabled, 0 = Timer 5 Disabled
TIER4       =     $10     ;1 = Timer 4 Enabled, 0 = Timer 4 Disabled
TIER3       =     $08     ;1 = Timer 3 Enabled, 0 = Timer 3 Disabled
TIER2       =     $04     ;1 = Timer 2 Enabled, 0 = Timer 2 Disabled
TIER1       =     $02     ;1 = Timer 1 Enabled, 0 = Timer 1 Disabled
TIER0       =     $01     ;1 = Timer 0 Enabled, 0 = Timer 0 Disabled

;2.8.2 Timer Interrupt Flag Register (TIFR) Description -- BIT FIELD
;For each bit, writing a 1 clears that interrupt flag
;TIFR7       =     $80     ;Read: 1 = Timer 7 Interrupted, 0 = Timer 7 Did not interrupt
TIFR6       =     $40     ;Read: 1 = Timer 6 Interrupted, 0 = Timer 6 Did not interrupt
TIFR5       =     $20     ;Read: 1 = Timer 5 Interrupted, 0 = Timer 5 Did not interrupt
TIFR4       =     $10     ;Read: 1 = Timer 4 Interrupted, 0 = Timer 4 Did not interrupt
TIFR3       =     $08     ;Read: 1 = Timer 3 Interrupted, 0 = Timer 3 Did not interrupt
TIFR2       =     $04     ;Read: 1 = Timer 2 Interrupted, 0 = Timer 2 Did not interrupt
TIFR1       =     $02     ;Read: 1 = Timer 1 Interrupted, 0 = Timer 1 Did not interrupt
TIFR0       =     $01     ;Read: 1 = Timer 0 Interrupted, 0 = Timer 0 Did not interrupt

;2.8.3 Edge Interrupt Enable Register (EIER) Description -- BIT FIELD
TIFR7       =     $80     ;1 = Enable, 0 = Disable
PIBE        =     $40     ;1 = Enable, 0 = Disable
NE66E       =     $20     ;1 = Enable, 0 = Disable
NE64E       =     $10     ;1 = Enable, 0 = Disable
PWME        =     $08     ;1 = Enable, 0 = Disable
PE60E       =     $04     ;1 = Enable, 0 = Disable
NE57E       =     $02     ;1 = Enable, 0 = Disable
PE56E       =     $01     ;1 = Enable, 0 = Disable

;2.8.4 Edge Interrupt Flag Register (EIFR) Description -- BIT FIELD
;For each bit, writing a 1 clears that interrupt flag
IRQB        =     $80     ;1 = Interrupted, 0 = Did not interrupt
PIB         =     $40     ;1 = Interrupted, 0 = Did not interrupt
NE66        =     $20     ;1 = Interrupted, 0 = Did not interrupt
NE64        =     $10     ;1 = Interrupted, 0 = Did not interrupt
PWM         =     $08     ;1 = Interrupted, 0 = Did not interrupt
PE60        =     $04     ;1 = Interrupted, 0 = Did not interrupt
NE57        =     $02     ;1 = Interrupted, 0 = Did not interrupt
PE56        =     $01     ;1 = Interrupted, 0 = Did not interrupt

;2.8.5 UART Interrupt Enable Register (UIER) Description -- BIT FIELD
U3TE        =     $80     ;1 = Enable, 0 = Disable
U3RE        =     $40     ;1 = Enable, 0 = Disable
U2TE        =     $20     ;1 = Enable, 0 = Disable
U2RE        =     $10     ;1 = Enable, 0 = Disable
U1TE        =     $08     ;1 = Enable, 0 = Disable
U1RE        =     $04     ;1 = Enable, 0 = Disable
U0TE        =     $02     ;1 = Enable, 0 = Disable
U0RE        =     $01     ;1 = Enable, 0 = Disable

;2.8.6 UART Interrupt Flag Register (UIFR) Description -- BIT FIELD
;For each bit, writing a 1 clears that interrupt flag
U3TF        =     $80     ;1 = Interrupted, 0 = Did not interrupt
U3RF        =     $40     ;1 = Interrupted, 0 = Did not interrupt
U2TF        =     $20     ;1 = Interrupted, 0 = Did not interrupt
U2RF        =     $10     ;1 = Interrupted, 0 = Did not interrupt
U1TF        =     $08     ;1 = Interrupted, 0 = Did not interrupt
U1RF        =     $04     ;1 = Interrupted, 0 = Did not interrupt
U0TF        =     $02     ;1 = Interrupted, 0 = Did not interrupt
U0RF        =     $01     ;1 = Interrupted, 0 = Did not interrupt

;2.9.7 Asynchronous Control and Status Registers (ACSRx) Description -- BIT FIELD
ACSRx7      =     $80     ;1 = Receiver Error Flag
                            ;Cleared by writing a 1; writing a 0 has 
ACSRx6      =     $40     ;Software Semaphore
ACSRx5      =     $20     ;1 = Enable Receiver, 0 = Disable Receiver
ACSRx4      =     $10     ;1 = Even Parity, 0 = Odd Parity
ACSRx3      =     $08     ;1 = Enable Parity, 0 = Disable Parity
ACSRx2      =     $04     ;1 = 8-bit data, 0 = 7-bit data
ACSRx1      =     $02     ;1 = Transmitter Interrupt occurs due to both the Transmitter
                            ;Data and Shift register empty condition (end-of-message 
                            ;transmission). ACSRx0 = 0 R/W clears this bit if set.
                            ;0 = Transmitter Interrupt occurs due to a Transmitter Data 
                            ;Register Empty condition (end-of-byte transmission).
ACSRx0      =     $01     ;1 = Enable Transmitter, Transmitter Interrupt, and TXDx on Port 6
                            ;0 = Disable Transmitter,Transmitter Interrupt, and TXDx on Port 6

;2.10.1 PIB Enable Register (PIBER) Description -- BIT FIELD
PIBER7      =     $80     ;1 = Enable Automatic Handshake Input Data in PIR7 Interrupt 
PIBER6      =     $40     ;1 = Enable Automatic Handshake Output Data in PIR7 Interrupt 
PIBER5      =     $20     ;1 = Enable Manual Handshake from Host
PIBER4      =     $10     ;1 = Enable Manual Handshake from Processor
PIBER3      =     $08     ;1 = Enable Automatic Handshake Input Data in PIR3 Interrupt 
PIBER2      =     $04     ;1 = Enable Automatic Handshake Output Data in PIR3 Interrupt
PIBER1      =     $02     ;1 = Enable RDB and WRB
PIBER0      =     $01     ;1 = Enable PIB

;2.10.2 PIB Flag Register (PIBFR) Description -- BIT FIELD
PIBFR7      =     $80     ;1 = Host Write to PIR7 and Interrupt Processor
                            ;0 = Cleared by Processor Read of PIR7
PIBFR6      =     $40     ;1 = Processor Write to PIR7 and Interrupt Host 
                            ;0 = Cleared by Host Read of PIR7
PIBFR5      =     $20     ;Write from Host and Interrupt Processor
PIBFR4      =     $10     ;Write from Processor and Interrupt Host
PIBFR3      =     $08     ;1 = Host Write to PIR3 and Interrupt Processor
                            ;0 = Processor Read of PIR3 
PIBFR2      =     $04     ;1 = Processor Write to PIR3 and Interrupt Host 
                            ;0 = Host Read of PIR3 
PIBFR1      =     $02     ;1 = Enable RDB and WRB 
PIBFR0      =     $01     ;1 = Enable P4B 

;2.12 System Speed Control Register (SSCR) -- BIT FIELD
SSCR7       =     $80     ;CS7 Speed Select (1 = FCLK, 0 = FCLK/4)
SSCR6       =     $40     ;CS6 Speed Select (1 = FCLK, 0 = FCLK/4)
SSCR5       =     $20     ;CS5 Speed Select (1 = FCLK, 0 = FCLK/4)
SSCR4       =     $10     ;CS4 Speed Select (1 = FCLK, 0 = FCLK/4) 
SSCR3       =     $08     ;System (CS0B-CS7B) Speed Select (1 = FCLK, 0 = FCLK/4) 
SSCR2       =     $04     ;1 = External RAM 0x0000-01FF, 0 = Internal RAM 0x0000-01FF
SSCR1       =     $02     ;PHI2 clock source (1 = FCLK/4 or FCLK, 0 = CLK)
SSCR0       =     $01     ;1 = Start FCLK, 0 = Stop FCLK

;Mensch Monitor Subroutines (Appendix B in the monitor manual)
ALTER_MEMORY                = $00E000 ; Support Subroutine For ‘M’ Command.
BACKSPACE                   = $00E003 ; Outputs a <BACKSPACE> ($08) Character.
CONTROL_TONES               = $00E009 ; Configures tone generators: TG0 & TG1.
DO_LOW_POWER_PGM            = $00E00C ; Vector to force LOW POWER MODE on system.
DUMPREGS                    = $00E00F ; Support Subroutine For ‘R’ Command.
DUMP528                     = $00E012 ; Support Subroutine For ‘R’ Command
DUMP_1_LINE_TO_OUTPUT       = $00E015 ; Requests/Accepts Starting Address & dumps 8 bytes.
DUMP_1_LINE_TO_SCREEN       = $00E018 ; Requests/Accepts Starting Address & dumps 8 bytes.
DUMP_TO_OUTPUT              = $00E01B ; Dumps specified range of memory data to port #3.
DUMP_TO_PRINTER             = $00E01E ; Dumps specified range of memory data to port #3.
DUMP_TO_SCREEN              = $00E021 ; Dumps specified range of memory data to port #3.
DUMP_TO_SCREEN_ASCII        = $00E024 ; Dumps memory block in ASCII format.
DUMP_IT                     = $00E027 ; Custom dump support.
FILL_MEMORY                 = $00E02A ; Support Subroutine For ‘F’ Command.
GET_3BYTE_ADDR              = $00E02D ; Accepts 3-byte address as six ASCII Hex digits.
GET_ALARM_STATUS            = $00E030 ; Retrieves & resets current system alarm status.
GET_BYTE_FROM_PC            = $00E033 ; Reads next available input from serial port #3.
GET_CHR                     = $00E036 ; Accepts on character from serial port #3.
GET_HEX                     = $00E039 ; Accepts two ASCII Hex digits via serial port #3.
GET_PUT_CHR                 = $00E03C ; Accepts & echoes one character via serial port #3.
GET_STR                     = $00E03F ; Uses GET_PUT_CHR to build string buffer.
GET_ADDRESS                 = $00E042 ; Requests/Accepts 3-byte address as ASCII Hex digits.
GET_E_ADDRESS               = $00E045 ; Requests/Accepts HIGHEST ADDRESS via serial port #3.
GET_S_ADDRESS               = $00E048 ; Requests/Accepts LOWEST ADDRESS via serial port #3.
PUT_CHR                     = $00E04B ; Output one character to serial console port #3.
PUT_STR                     = $00E04E ; Output specified string of characters to serial port #3.
READ_ALARM                  = $00E051 ; Reads current system Alarm setting.
READ_DATE                   = $00E054 ; Reads current system Date setting.
READ_TIME                   = $00E057 ; Reads current system Time setting.
RESET_ALARM                 = $00E05A ; Resets the system Alarm function.
SBREAK                      = $00E05D ; Invokes the software breakpoint logic.
SELECT_COMMON_BAUD_RATE     = $00E060 ; Configures baud rate generator for serial port #3.
SEND_BYTE_TO_PC             = $00E063 ; Outputs byte to serial port #3.
SEND_CR                     = $00E066 ; Outputs ASCII ENTER ($0D) character.
SEND_SPACE                  = $00E069 ; Outputs ASCII SPACE ($20) character.
SEND_HEX_OUT                = $00E06C ; Outputs byte as two ASCII Hex characters.
SET_ALARM                   = $00E06F ; Set the System Alarm time from specified string.
SET_BREAKPOINT              = $00E072 ; Sets a breakpoint (BRK) instruction at address.
SET_DATE                    = $00E075 ; Sets the System Time-Of-Day Clock: Date
SET_TIME                    = $00E078 ; Sets the System Time-Of-Day Clock: Time
VERSION                     = $00E07B ; Gets firmware (W65C265 ROM) version info.
WR_3_ADDRESS                = $00E07E ; Outputs a 3-byte address as ASCII Hex characters
XS28IN                      = $00E081 ; Accepts & loads Motorola type “S28” records.
RESET                       = $00E084 ; Invokes the Master Start-Up vector to Reset system.
ASCBIN                      = $00E087 ; Converts two ASCII Hex characters to binary byte.
BIN2DEC                     = $00E08B ; Converts binary byte to packed BCD digits.
BINASC                      = $00E08F ; Converts binary byte to ASCII Hexadecimal characters.
HEXIN                       = $00E093 ; Converts ASCII Hexadecimal character to Binary byte.
IFASC                       = $00E097 ; Checks for displayable ASCII character.
ISDECIMAL                   = $00E09B ; Checks character for ASCII Decimal Digit.
ISHEX                       = $00E09F ; Checks character for ASCII Hexadecimal Digit
UPPER_CASE                  = $00E0A3 ; Converts lower-case ASCII alpha chars to upper-case.

FLAGN                       = $80
FLAGV                       = $40
FLAGM                       = $20
FLAGX                       = $10
FLAGD                       = $08
FLAGI                       = $04
FLAGZ                       = $02
FLAGC                       = $01
