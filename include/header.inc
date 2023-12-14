        ;; Rohan Bafna, Sep 11 2023
        ;; Additions to the W65C265 equates.

        .include "firmware.inc"

        ;; Processor status bitmasks
        .define FLAGN (1 << 7)
        .define FLAGV (1 << 6)
        .define FLAGM (1 << 5)
        .define FLAGX (1 << 4)
        .define FLAGD (1 << 3)
        .define FLAGI (1 << 2)
        .define FLAGZ (1 << 1)
        .define FLAGC (1 << 0)

        ;; Mensch Monitor RAM location
        .define UBRK 0x100
        .define UNMI 0x104
        .define UCOP 0x10C
