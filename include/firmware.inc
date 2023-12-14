;W65C265 equates -- using 0x prefix to indicate hex
;Adapted from the datasheet and monitor ROM manual by Aaron Lanterman, Oct. 19, 2021
;This file dedicated to the public domain

;Use this macro with ca65
;.define EQU =

;2.4.1 Emulation Mode Priority Encoded Interrupt Vector Module
;NOTE YOU PROBABLY DON'T WANT EMULATION MODE (this is NOT "65C02" emulation)
EIRQBRK      EQU    0xFFFE     ;BRK - Software Interrupt
EIRQRES      EQU    0xFFFC     ;RES - "REStart" Interrupt  
EIRQNMI      EQU    0xFFFA     ;Non-Maskable Interrupt
EIABORT      EQU    0xFFF8     ;ABORT Interrupt
EIRQCOP      EQU    0xFFF4     ;COP Software Interrupt
EIRQAT3      EQU    0xFFEE     ;UART3 Transmitter Interrupt 
EIRQAR3      EQU    0xFFEC     ;UART3 Receiver Interrupt 
EIRQAT2      EQU    0xFFEA     ;UART2 Transmitter Interrupt
EIRQAR2      EQU    0xFFE8     ;UART2 Receiver Interrupt
EIRQAT1      EQU    0xFFE6     ;UART1 Transmitter Interrupt
EIRQAR1      EQU    0xFFE4     ;UART1 Receiver Interrupt
EIRQAT0      EQU    0xFFE2     ;UART0 Transmitter Interrupt
EIRQAR0      EQU    0xFFE0     ;UART0 Receiver Interrupt
EIRQ         EQU    0xFFDE     ;IRQ Level Interrupt
EIRQPIB      EQU    0xFFDC     ;Parallel Interface Bus (PIB) 
EIRNE66      EQU    0xFFDA     ;Interrupt Negative Edge Interrupt on P66 
EIRNE64      EQU    0xFFD8     ;Negative Edge Interrupt on P64
EIRPE62      EQU    0xFFD6     ;Positive Edge Interrupt on P62 for PWM 
EIRPE60      EQU    0xFFD4     ;Positive Edge Interrupt on P60 
EIRNE57      EQU    0xFFD2     ;Negative Edge Interrupt on P57 
EIRPE56      EQU    0xFFD0     ;Positive Edge Interrupt on P56 
EIRQT7       EQU    0xFFCE     ;Timer 7 Interrupt
EIRQT6       EQU    0xFFCC     ;Timer 6 Interrupt
EIRQT5       EQU    0xFFCA     ;Timer 5 Interrupt
EIRQT4       EQU    0xFFC8     ;Timer 4 Interrupt
EIRQT3       EQU    0xFFC6     ;Timer 3 Interrupt
EIRQT2       EQU    0xFFC4     ;Timer 2 Interrupt
EIRQT1       EQU    0xFFC2     ;Timer 1 Interrupt
EIRQT0       EQU    0xFFC0     ;Timer 0 Interrupt

;2.4.2 Native Mode Priority Encoded Interrupt Vector Module
;NOTE YOU PROBABLY WANT NATIVE MODE ("native" here does NOT indicate "65C816" mode,
;the 65C02/65C816 selection is something else entirely)
IRQNMI      EQU     0xFFBA     ;Non-Maskable Interrupt
IABORT      EQU     0xFFB8     ;ABORT Interrupt
IRQBRK      EQU     0xFFB6     ;BRK Software Interrupt 
IRQCOP      EQU     0xFFB4     ;COP Software Interrupt 
IRQAT3      EQU     0xFFAE     ;UART3 Transmitter Interrupt 
IRQAR3      EQU     0xFFAC     ;UART3 Receiver Interrupt
IRQAT2      EQU     0xFFAA     ;UART2 Transmitter Interrupt 
IRQAR2      EQU     0xFFA8     ;UART2 Receiver Interrupt
IRQAT1      EQU     0xFFA6     ;UART1 Transmitter Interrupt
IRQAR1      EQU     0xFFA4     ;UART1 Receiver Interrupt
IRQAT0      EQU     0xFFA2     ;UART0 Transmitter Interrupt
IRQAR0      EQU     0xFFA0     ;UART0 Receiver Interrupt
IRQ         EQU     0xFF9E     ;IRQ Level Interrupt
IRQPIB      EQU     0xFF9C     ;Parallel Interface Bus (PIB) Interrupt 
IRNE66      EQU     0xFF9A     ;Negative Edge Interrupt on P66 
IRNE64      EQU     0xFF98     ;Negative Edge Interrupt on P64 
IRPE62      EQU     0xFF96     ;Positive Edge Interrupt on P62 
IRPE60      EQU     0xFF94     ;Positive Edge Interrupt on P60
IRNE57      EQU     0xFF92     ;Negative Edge Interrupt on P57 
IRPE56      EQU     0xFF90     ;Positive Edge Interrupt on P56 
IRQT7       EQU     0xFF8E     ;Timer 6 Interrupt
IRQT6       EQU     0xFF8C     ;Timer 6 Interrupt
IRQT5       EQU     0xFF8A     ;Timer 5 Interrupt
IRQT4       EQU     0xFF88     ;Timer 4 Interrupt
IRQT3       EQU     0xFF86     ;Timer 3 Interrupt
IRQT2       EQU     0xFF84     ;Timer 2 Interrupt
IRQT1       EQU     0xFF82     ;Timer 1 Interrupt
IRQT0       EQU     0xFF80     ;Timer 0 Interrupt

;2.5.1 Communication Register Memory Map
PIR7        EQU     0xDF7F   ;Parallel Interface Register 7 (uninit)
PIR6        EQU     0xDF7E   ;Parallel Interface Register 6 (uninit)
PIR5        EQU     0xDF7D   ;Parallel Interface Register 5 (uninit)
PIR4        EQU     0xDF7C   ;Parallel Interface Register 4 (uninit)
PIR3        EQU     0xDF7B   ;Parallel Interface Register 3 (uninit)
PIR2        EQU     0xDF7A   ;Parallel Interface Register 2 (uninit)
PIBER       EQU     0xDF79   ;Parallel Interface Enable Register (0x00)
PIBFR       EQU     0xDF78   ;Parallel Interface Flag Register (0x00)
ARTD3       EQU     0xDF77   ;UART 3 Data Register (uninit) 
ACSR3       EQU     0xDF76   ;UART 3 Control/Status Register (0x00)
ARTD2       EQU     0xDF75   ;UART 2 Data Register (uninit) 
ACSR2       EQU     0xDF74   ;UART 2 Control/Status Register (0x00)
ARTD1       EQU     0xDF73   ;UART 1 Data Register (uninit) 
ACSR1       EQU     0xDF72   ;UART 1 Control/Status Register (0x00)
ARTD0       EQU     0xDF71   ;UART 0 Data Register (uninit)
ACSRO       EQU     0xDF70   ;UART 0 Control/Status Register (0x00)

;2.5.2 Timer Register Memory Map (all uninit)
T7CH        EQU     0xDF6F   ;Timer 7 Counter High 
T7CL        EQU     0xDF6E   ;Timer 7 Counter Low 
T6CH        EQU     0xDF6D   ;Timer 6 Counter High 
T6CL        EQU     0xDF6C   ;Timer 6 Counter Low 
T5CH        EQU     0xDF6B   ;Timer 5 Counter High
T5CL        EQU     0xDF6A   ;Timer 5 Counter Low 
T4CH        EQU     0xDF69   ;Timer 4 Counter High
T4CL        EQU     0xDF68   ;Timer 4 Counter Low
T3CH        EQU     0xDF67   ;Timer 3 Counter High
T3CL        EQU     0xDF66   ;Timer 3 Counter Low
T2CH        EQU     0xDF65   ;Timer 2 Counter High 
T2CL        EQU     0xDF64   ;Timer 2 Counter Low
T1CH        EQU     0xDF63   ;Timer 1 Counter High
T1CL        EQU     0xDF62   ;Timer 1 Counter Low
T0CH        EQU     0xDF61   ;Timer 0 Counter High
T0CL        EQU     0xDF60   ;Timer 0 Counter Low
T7LH        EQU     0xDF5F   ;Timer 7 Latch High 
T7LL        EQU     0xDF5E   ;Timer 7 Latch Low 
T6LH        EQU     0xDF5D   ;Timer 6 Latch High 
T6LL        EQU     0xDF5C   ;Timer 6 Latch Low
T5LH        EQU     0xDF5B   ;Timer 5 Latch High 
T5LL        EQU     0xDF5A   ;Timer 5 Latch Low
T4LH        EQU     0xDF59   ;Timer 4 Latch High 
T4LL        EQU     0xDF58   ;Timer 4 Latch Low
T3LH        EQU     0xDF57   ;Timer 3 Latch High 
T3LL        EQU     0xDF56   ;Timer 3 Latch Low
T2LH        EQU     0xDF55   ;Timer 2 Latch High 
T2LL        EQU     0xDF54   ;Timer 2 Latch Low
T1LH        EQU     0xDF53   ;Timer 1 Latch High 
T1LL        EQU     0xDF52   ;Timer 1 Latch Low
T0LH        EQU     0xDF51   ;Timer 0 Latch High 
T0LL        EQU     0xDF50   ;Timer 0 Latch Low

;2.5.3 Control and Status Register Memory Map
UIER        EQU     0xDF49   ;UART Interrupt Enable Register (0x00)
UIFR        EQU     0xDF48   ;UART Interrupt Flag Register (0x00)
EIER        EQU     0xDF47   ;Edge Interrupt Enable Register (0x00)
TIER        EQU     0xDF46   ;Timer Interrupt Enable Register (0x00)
EIFR        EQU     0xDF45   ;Edge Interrupt Flag Register (0x00)
TIFR        EQU     0xDF44   ;Timer Interrupt Flag Register (0x00)
TER         EQU     0xDF43   ;Timer Enable Register (0x00)
TCR         EQU     0xDF42   ;Timer Control Register (0x00)
SSCR        EQU     0xDF41   ;System Speed Control Register (0x00)
BCR         EQU     0xDF40   ;Bus Control Register (0x00/0x89)

;2.5.4 I/O Register Memory Map
PCS7        EQU     0xDF27   ;Port 7 Chip Select
PDD6        EQU     0xDF26   ;Port 6 Data Direction Register
PDD5        EQU     0xDF25   ;Port 5 Data Direction Register
PDD4        EQU     0xDF24   ;Port 4 Data Direction Register
PD7         EQU     0xDF23   ;Port 7 Data Register
PD6         EQU     0xDF22   ;Port 6 Data Register
PD5         EQU     0xDF21   ;Port 5 Data Register
PD4         EQU     0xDF20   ;Port 4 Data Register
PDD3        EQU     0xDF07   ;Port 3 Data Direction Register
PDD2        EQU     0xDF06   ;Port 2 Data Direction Register
PDD1        EQU     0xDF05   ;Port 1 Data Direction Register
PDD0        EQU     0xDF04   ;Port 0 Data Direction Register
PD3         EQU     0xDF03   ;Port 3 Data Register
PD2         EQU     0xDF02   ;Port 2 Data Register
PD1         EQU     0xDF01   ;Port 1 Data Register
PD0         EQU     0xDF00   ;Port 0 Data Register

;2.6.1 Bus Control Register (BCR) Description - BIT FIELD
BCR7        EQU     0x80     ;1 = External ROM (BE controls shown in Table 1-1)
                            ;0 = Internal ROM (BE Controls shown in Table 1-1)
BCR6        EQU     0x40     ;1 = Enable NMIB on P40 Input is level sensitive,
                            ;NIMB and ABORTB cannot both be enabled at the same time.
                            ;0 = Disable NMIB
BCR5        EQU     0x20     ;1 = Enable ABORTB on P40 Input is level sensitive, 
                            ;NMIB and ABORTB cannot both be enabled at the same time.
                            ;0 = Disable ABORTB
BCR4        EQU     0x10     ;1 = Watch Dog Enabled, 0 = Watch Dog Disabled
BCR3        EQU     0x08     ;1 = Emulation Mode, RUN=Run, BA=BA/1 All on-chip addressed 
                            ;memory or I/O for reads or writes are output on the data 
                            ;bus (this is the emulation mode of operation)
                            ;0 = Normal Operation RUN=RUN, BA=BA
BCR2        EQU     0x04     ;1 = Enable TG1, 0 = Disable TG1
BCR1        EQU     0x02     ;1 = Enable TG0, 0 = Disable TG0
BCR0        EQU     0x01     ;1 = Ports 0,1,2,3 are address and data bus pins
                            ;0 = Ports 0,1,2,3 are I/O pins

;2.7.2 Timer Control Register (TCR) Description - BIT FIELD
TCR7        EQU     0x80     ;1 = UART3 Timer 4 Selected, 1 = UART3 Timer 3 Selected
TCR6        EQU     0x40     ;1 = UART2 Timer 4 Selected, 1 = UART2 Timer 3 Selected
TCR5        EQU     0x20     ;1 = UART1 Timer 4 Selected, 1 = UART1 Timer 3 Selected
TCR4        EQU     0x10     ;1 = UART0 Timer 4 Selected, 1 = UART0 Timer 3 Selected
TCR3        EQU     0x08     ;1 = PWM Measurement on P62 Negative Edges 
TCR2        EQU     0x04     ;1 = PWM Measurement on P62 Positive Edges 
                            ;can set both TCR2 and TCR3 to "1" to measure on BOTH edges
TCR1        EQU     0x02     ;1 = Enable Timer 4 on P61, 0 = Disable Timer 4 on P61
TCR0        EQU     0x01     ;1 = Timer 4 Clock Selected on P60, 0 = Timer 4 FCLK Selected

;2.7.3 Timer Enable Register (TER) Description - BIT FIELD
TER7        EQU     0x80     ;1 = Timer 7 Enabled, 0 = Timer 7 Disabled
TER6        EQU     0x40     ;1 = Timer 6 Enabled, 0 = Timer 6 Disabled
TER5        EQU     0x20     ;1 = Timer 5 Enabled, 0 = Timer 5 Disabled
TER4        EQU     0x10     ;1 = Timer 4 Enabled, 0 = Timer 4 Disabled
TER3        EQU     0x08     ;1 = Timer 3 Enabled, 0 = Timer 3 Disabled
TER2        EQU     0x04     ;1 = Timer 2 Enabled, 0 = Timer 2 Disabled
TER1        EQU     0x02     ;1 = Timer 1 Enabled, 0 = Timer 1 Disabled
TER0        EQU     0x01     ;1 = Timer 0 Enabled, 0 = Timer 0 Disabled

;2.8.1 Timer Interrupt Enable Register (TIER) Description - BIT FIELD
TIER7       EQU     0x80     ;1 = Timer 7 Enabled, 0 = Timer 7 Disabled
TIER6       EQU     0x40     ;1 = Timer 6 Enabled, 0 = Timer 6 Disabled
TIER5       EQU     0x20     ;1 = Timer 5 Enabled, 0 = Timer 5 Disabled
TIER4       EQU     0x10     ;1 = Timer 4 Enabled, 0 = Timer 4 Disabled
TIER3       EQU     0x08     ;1 = Timer 3 Enabled, 0 = Timer 3 Disabled
TIER2       EQU     0x04     ;1 = Timer 2 Enabled, 0 = Timer 2 Disabled
TIER1       EQU     0x02     ;1 = Timer 1 Enabled, 0 = Timer 1 Disabled
TIER0       EQU     0x01     ;1 = Timer 0 Enabled, 0 = Timer 0 Disabled

;2.8.2 Timer Interrupt Flag Register (TIFR) Description -- BIT FIELD
;For each bit, writing a 1 clears that interrupt flag
TIFR7       EQU     0x80     ;Read: 1 = Timer 7 Interrupted, 0 = Timer 7 Did not interrupt
TIFR6       EQU     0x40     ;Read: 1 = Timer 6 Interrupted, 0 = Timer 6 Did not interrupt
TIFR5       EQU     0x20     ;Read: 1 = Timer 5 Interrupted, 0 = Timer 5 Did not interrupt
TIFR4       EQU     0x10     ;Read: 1 = Timer 4 Interrupted, 0 = Timer 4 Did not interrupt
TIFR3       EQU     0x08     ;Read: 1 = Timer 3 Interrupted, 0 = Timer 3 Did not interrupt
TIFR2       EQU     0x04     ;Read: 1 = Timer 2 Interrupted, 0 = Timer 2 Did not interrupt
TIFR1       EQU     0x02     ;Read: 1 = Timer 1 Interrupted, 0 = Timer 1 Did not interrupt
TIFR0       EQU     0x01     ;Read: 1 = Timer 0 Interrupted, 0 = Timer 0 Did not interrupt

;2.8.3 Edge Interrupt Enable Register (EIER) Description -- BIT FIELD
;TIFR7       EQU     0x80     ;1 = Enable, 0 = Disable
PIBE        EQU     0x40     ;1 = Enable, 0 = Disable
NE66E       EQU     0x20     ;1 = Enable, 0 = Disable
NE64E       EQU     0x10     ;1 = Enable, 0 = Disable
PWME        EQU     0x08     ;1 = Enable, 0 = Disable
PE60E       EQU     0x04     ;1 = Enable, 0 = Disable
NE57E       EQU     0x02     ;1 = Enable, 0 = Disable
PE56E       EQU     0x01     ;1 = Enable, 0 = Disable

;2.8.4 Edge Interrupt Flag Register (EIFR) Description -- BIT FIELD
;For each bit, writing a 1 clears that interrupt flag
IRQB        EQU     0x80     ;1 = Interrupted, 0 = Did not interrupt
PIB         EQU     0x40     ;1 = Interrupted, 0 = Did not interrupt
NE66        EQU     0x20     ;1 = Interrupted, 0 = Did not interrupt
NE64        EQU     0x10     ;1 = Interrupted, 0 = Did not interrupt
PWM         EQU     0x08     ;1 = Interrupted, 0 = Did not interrupt
PE60        EQU     0x04     ;1 = Interrupted, 0 = Did not interrupt
NE57        EQU     0x02     ;1 = Interrupted, 0 = Did not interrupt
PE56        EQU     0x01     ;1 = Interrupted, 0 = Did not interrupt

;2.8.5 UART Interrupt Enable Register (UIER) Description -- BIT FIELD
U3TE        EQU     0x80     ;1 = Enable, 0 = Disable
U3RE        EQU     0x40     ;1 = Enable, 0 = Disable
U2TE        EQU     0x20     ;1 = Enable, 0 = Disable
U2RE        EQU     0x10     ;1 = Enable, 0 = Disable
U1TE        EQU     0x08     ;1 = Enable, 0 = Disable
U1RE        EQU     0x04     ;1 = Enable, 0 = Disable
U0TE        EQU     0x02     ;1 = Enable, 0 = Disable
U0RE        EQU     0x01     ;1 = Enable, 0 = Disable

;2.8.6 UART Interrupt Flag Register (UIFR) Description -- BIT FIELD
;For each bit, writing a 1 clears that interrupt flag
U3TF        EQU     0x80     ;1 = Interrupted, 0 = Did not interrupt
U3RF        EQU     0x40     ;1 = Interrupted, 0 = Did not interrupt
U2TF        EQU     0x20     ;1 = Interrupted, 0 = Did not interrupt
U2RF        EQU     0x10     ;1 = Interrupted, 0 = Did not interrupt
U1TF        EQU     0x08     ;1 = Interrupted, 0 = Did not interrupt
U1RF        EQU     0x04     ;1 = Interrupted, 0 = Did not interrupt
U0TF        EQU     0x02     ;1 = Interrupted, 0 = Did not interrupt
U0RF        EQU     0x01     ;1 = Interrupted, 0 = Did not interrupt

;2.9.7 Asynchronous Control and Status Registers (ACSRx) Description -- BIT FIELD
ACSRx7      EQU     0x80     ;1 = Receiver Error Flag
                            ;Cleared by writing a 1; writing a 0 has 
ACSRx6      EQU     0x40     ;Software Semaphore
ACSRx5      EQU     0x20     ;1 = Enable Receiver, 0 = Disable Receiver
ACSRx4      EQU     0x10     ;1 = Even Parity, 0 = Odd Parity
ACSRx3      EQU     0x08     ;1 = Enable Parity, 0 = Disable Parity
ACSRx2      EQU     0x04     ;1 = 8-bit data, 0 = 7-bit data
ACSRx1      EQU     0x02     ;1 = Transmitter Interrupt occurs due to both the Transmitter
                            ;Data and Shift register empty condition (end-of-message 
                            ;transmission). ACSRx0 = 0 R/W clears this bit if set.
                            ;0 = Transmitter Interrupt occurs due to a Transmitter Data 
                            ;Register Empty condition (end-of-byte transmission).
ACSRx0      EQU     0x01     ;1 = Enable Transmitter, Transmitter Interrupt, and TXDx on Port 6
                            ;0 = Disable Transmitter,Transmitter Interrupt, and TXDx on Port 6

;2.10.1 PIB Enable Register (PIBER) Description -- BIT FIELD
PIBER7      EQU     0x80     ;1 = Enable Automatic Handshake Input Data in PIR7 Interrupt 
PIBER6      EQU     0x40     ;1 = Enable Automatic Handshake Output Data in PIR7 Interrupt 
PIBER5      EQU     0x20     ;1 = Enable Manual Handshake from Host
PIBER4      EQU     0x10     ;1 = Enable Manual Handshake from Processor
PIBER3      EQU     0x08     ;1 = Enable Automatic Handshake Input Data in PIR3 Interrupt 
PIBER2      EQU     0x04     ;1 = Enable Automatic Handshake Output Data in PIR3 Interrupt
PIBER1      EQU     0x02     ;1 = Enable RDB and WRB
PIBER0      EQU     0x01     ;1 = Enable PIB

;2.10.2 PIB Flag Register (PIBFR) Description -- BIT FIELD
PIBFR7      EQU     0x80     ;1 = Host Write to PIR7 and Interrupt Processor
                            ;0 = Cleared by Processor Read of PIR7
PIBFR6      EQU     0x40     ;1 = Processor Write to PIR7 and Interrupt Host 
                            ;0 = Cleared by Host Read of PIR7
PIBFR5      EQU     0x20     ;Write from Host and Interrupt Processor
PIBFR4      EQU     0x10     ;Write from Processor and Interrupt Host
PIBFR3      EQU     0x08     ;1 = Host Write to PIR3 and Interrupt Processor
                            ;0 = Processor Read of PIR3 
PIBFR2      EQU     0x04     ;1 = Processor Write to PIR3 and Interrupt Host 
                            ;0 = Host Read of PIR3 
PIBFR1      EQU     0x02     ;1 = Enable RDB and WRB 
PIBFR0      EQU     0x01     ;1 = Enable P4B 

;2.12 System Speed Control Register (SSCR) -- BIT FIELD
SSCR7       EQU     0x80     ;CS7 Speed Select (1 = FCLK, 0 = FCLK/4)
SSCR6       EQU     0x40     ;CS6 Speed Select (1 = FCLK, 0 = FCLK/4)
SSCR5       EQU     0x20     ;CS5 Speed Select (1 = FCLK, 0 = FCLK/4)
SSCR4       EQU     0x10     ;CS4 Speed Select (1 = FCLK, 0 = FCLK/4) 
SSCR3       EQU     0x08     ;System (CS0B-CS7B) Speed Select (1 = FCLK, 0 = FCLK/4) 
SSCR2       EQU     0x04     ;1 = External RAM 0x0000-01FF, 0 = Internal RAM 0x0000-01FF
SSCR1       EQU     0x02     ;PHI2 clock source (1 = FCLK/4 or FCLK, 0 = CLK)
SSCR0       EQU     0x01     ;1 = Start FCLK, 0 = Stop FCLK

;Mensch Monitor Subroutines (Appendix B in the monitor manual)
ALTER_MEMORY                EQU 0x00E000 ; Support Subroutine For ‘M’ Command.
BACKSPACE                   EQU 0x00E003 ; Outputs a <BACKSPACE> (0x08) Character.
CONTROL_TONES               EQU 0x00E009 ; Configures tone generators: TG0 & TG1.
DO_LOW_POWER_PGM            EQU 0x00E00C ; Vector to force LOW POWER MODE on system.
DUMPREGS                    EQU 0x00E00F ; Support Subroutine For ‘R’ Command.
DUMP528                     EQU 0x00E012 ; Support Subroutine For ‘R’ Command
DUMP_1_LINE_TO_OUTPUT       EQU 0x00E015 ; Requests/Accepts Starting Address & dumps 8 bytes.
DUMP_1_LINE_TO_SCREEN       EQU 0x00E018 ; Requests/Accepts Starting Address & dumps 8 bytes.
DUMP_TO_OUTPUT              EQU 0x00E01B ; Dumps specified range of memory data to port #3.
DUMP_TO_PRINTER             EQU 0x00E01E ; Dumps specified range of memory data to port #3.
DUMP_TO_SCREEN              EQU 0x00E021 ; Dumps specified range of memory data to port #3.
DUMP_TO_SCREEN_ASCII        EQU 0x00E024 ; Dumps memory block in ASCII format.
DUMP_IT                     EQU 0x00E027 ; Custom dump support.
FILL_MEMORY                 EQU 0x00E02A ; Support Subroutine For ‘F’ Command.
GET_3BYTE_ADDR              EQU 0x00E02D ; Accepts 3-byte address as six ASCII Hex digits.
GET_ALARM_STATUS            EQU 0x00E030 ; Retrieves & resets current system alarm status.
GET_BYTE_FROM_PC            EQU 0x00E033 ; Reads next available input from serial port #3.
GET_CHR                     EQU 0x00E036 ; Accepts on character from serial port #3.
GET_HEX                     EQU 0x00E039 ; Accepts two ASCII Hex digits via serial port #3.
GET_PUT_CHR                 EQU 0x00E03C ; Accepts & echoes one character via serial port #3.
GET_STR                     EQU 0x00E03F ; Uses GET_PUT_CHR to build string buffer.
GET_ADDRESS                 EQU 0x00E042 ; Requests/Accepts 3-byte address as ASCII Hex digits.
GET_E_ADDRESS               EQU 0x00E045 ; Requests/Accepts HIGHEST ADDRESS via serial port #3.
GET_S_ADDRESS               EQU 0x00E048 ; Requests/Accepts LOWEST ADDRESS via serial port #3.
PUT_CHR                     EQU 0x00E04B ; Output one character to serial console port #3.
PUT_STR                     EQU 0x00E04E ; Output specified string of characters to serial port #3.
READ_ALARM                  EQU 0x00E051 ; Reads current system Alarm setting.
READ_DATE                   EQU 0x00E054 ; Reads current system Date setting.
READ_TIME                   EQU 0x00E057 ; Reads current system Time setting.
RESET_ALARM                 EQU 0x00E05A ; Resets the system Alarm function.
SBREAK                      EQU 0x00E05D ; Invokes the software breakpoint logic.
SELECT_COMMON_BAUD_RATE     EQU 0x00E060 ; Configures baud rate generator for serial port #3.
SEND_BYTE_TO_PC             EQU 0x00E063 ; Outputs byte to serial port #3.
SEND_CR                     EQU 0x00E066 ; Outputs ASCII ENTER (0x0D) character.
SEND_SPACE                  EQU 0x00E069 ; Outputs ASCII SPACE (0x20) character.
SEND_HEX_OUT                EQU 0x00E06C ; Outputs byte as two ASCII Hex characters.
SET_ALARM                   EQU 0x00E06F ; Set the System Alarm time from specified string.
SET_BREAKPOINT              EQU 0x00E072 ; Sets a breakpoint (BRK) instruction at address.
SET_DATE                    EQU 0x00E075 ; Sets the System Time-Of-Day Clock: Date
SET_TIME                    EQU 0x00E078 ; Sets the System Time-Of-Day Clock: Time
VERSION                     EQU 0x00E07B ; Gets firmware (W65C265 ROM) version info.
WR_3_ADDRESS                EQU 0x00E07E ; Outputs a 3-byte address as ASCII Hex characters
XS28IN                      EQU 0x00E081 ; Accepts & loads Motorola type “S28” records.
RESET                       EQU 0x00E084 ; Invokes the Master Start-Up vector to Reset system.
ASCBIN                      EQU 0x00E087 ; Converts two ASCII Hex characters to binary byte.
BIN2DEC                     EQU 0x00E08B ; Converts binary byte to packed BCD digits.
BINASC                      EQU 0x00E08F ; Converts binary byte to ASCII Hexadecimal characters.
HEXIN                       EQU 0x00E093 ; Converts ASCII Hexadecimal character to Binary byte.
IFASC                       EQU 0x00E097 ; Checks for displayable ASCII character.
ISDECIMAL                   EQU 0x00E09B ; Checks character for ASCII Decimal Digit.
ISHEX                       EQU 0x00E09F ; Checks character for ASCII Hexadecimal Digit
UPPER_CASE                  EQU 0x00E0A3 ; Converts lower-case ASCII alpha chars to upper-case.
