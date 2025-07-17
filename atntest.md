# ATN Test

by Troy Fullwood

## Introduction

The Commodore 64 implements a single-tasking operating system called the `KERNAL` -- legendarily due to the designer misspelling the word in their notebooks and this getting copied verbatim into the documentation. By single-tasking OS, I mean a Hardware Interface Layer, which is all that really means. A multitasking operating system also provides a resource multiplexor; many different processes competing for the screen, RAM, hard drive, keyboard, etc etc.

As a single-tasking operating system, none of the I/O routines provided by the `KERNAL` are asynchrynous; they all "block" until the call is completed. In the case of Commodore programs that, for instance, play music while loading, they often use a custom fast-loader and add interrupts that will call the music in the background. Even the C64 version of the Geck/OS system -- which supports multitasking -- will pause whenever accessing the disk drive.

The disk drive -- as well as most other peripherals not on the user port -- is connected daisy-chain style via the Commodore Serial Bus. The Serial protocol allows for, at any time, one talker and one or more listeners. First, the talker advertises it has a byte to send,. The listeners all acknowledge they're ready to receive -- taking as long as they want to do so. This is so that a printer could have time to empty its buffer, or a disk drive have time to flush to disk. Then, the talker acknowledges -- and we're now LOCKED IN to timing critical code to transfer 8 bits. And the process begins again.

If the talker takes more than 256 microseconds to respond when the listeners are ready, then this byte is the last of the stream, following which is end of file. If the talker times out at more than 512 microseconds to respond, then the stream is assumed to be empty. A reasonable response to a device not being on the lin

Unfortunately for us, this timing setup means that it's not possible to use the Commodore Serial Bus as a non-blocking I/O system.

Except, for the ATN line.

Only the computer has control over the ATN line. Whenever it activates it -- *even mid byte transfer* -- all devices on the bus are supposed to respond within 1000 microseconds. Now, the computer is the talker, and will transfer bytes as long as the ATN line is held down. These bytes are sent as normal, with the computer indicating it has a byte to send, listeners taking as long as they please to reply OK, and the talker then sending the byte.

The question is, how will a device on the receiving end of an ATN command respond to being left on the hook for, say, several seconds. They're supposed to ignore the 256-512 microsecond "end of stream" delay indicator, since the command end is indicated by dropping the ATN line. Glancing at the 1541 drive ROM source, it appears to me that the system will completely ignore any such delays, no matter how long.

If serial bus devices *do* completely ignore the delay, I believe I could implement a non-blocking asynchronous serial bus library. I could have a routine called by a regular interrupt, such as the system clock. I could transfer a few bytes, using the VIC-II scanline counter as a way to tell how long I've been transferring. Once I'm out of itme, I could send an ATN command to pause the stream. If they took too long to respond to the ATN command, I just hold them on the line and not send the byte til the next interrupt.

I want to test this, both with a disk drive on serial bus 8, and a printer on serial bus 4.

### ATN Commands

There are several kinds of ATN commands which may be sent.

- $20-$3E: `LISTEN`. Commands the device numbered in the lower five bits to become a listener.
- $3F: `UNLISTEN`. Commands all listeners to stop listening.
- $40-$5E: `TALK`. Commands the device numbered in the lower five bits to become a talker. Then, the computer could swap over to a listener after the ATN stream. This allows, for instance, a disk file to be `TALK`'ed by the 1541, and the computer to `LISTEN` it into it's RAM.
- $5F: `UNTALK`. All talkers are commanded to shush up.
- $60-$7E: `SECOND`. Send the secondary address in the lower 5 bits. Can optionally be send after a `TALK`/`LISTEN`.
- $E0-$EF: `CLOSE`. Prefix this with a `LISTEN` and follow it with an `UNLISTEN` in the same ATN command. This will command a file associated with the secondary address in the lower 4 bits to be closed.
- $F0-$FF: `OPEN`. Prefix this with a `LISTEN` in the same ATN command. Associate a named file with the secondary address in the lower 4 bits. The filename is sent as part of the ATN command after the `OPEN`, followed by an `UNLISTEN`.

## The Test

Let's write our own set of serial routines. Let's set-up our high level interrupt routine.

```{.asm6502 #irq}
    .code
    .export MyIrq
.proc MyIrq
    lda vic_scanline
    sta start_line
    clc
    adc #LINES_FOR_XFER
    sta target_line

<<transfer_bytes>>

<<wrapup_irq>>
.endproc
```

We need to define the scanline register.

```{.asm6502 #constants}
vic_scanline = $D012
```

And the number of lines we intend to use this go-round.

```{.asm6502 #constants}
CYCLES_PER_LINE=65 ; on NTSC, PAL is 63
TARGET_MICROSECONDS = 6000
LINES_FOR_XFER = TARGET_MICROSECONDS / CYCLES_PER_LINE
```

Plus those line variables.

```{.asm6502 #variables}
    .bss
start_line:  .res 2
target_line: .res 2
```

### The Transfer

To transfer the bytes, we need to know what bytes to transfer. We either want to read *n* bytes from the device, write *n* bytes to the device, or send an ATN command. The ATN command preempts the actual transfer, since the transfer itself might send an ATN command to pause.

```{.asm6502 #constants}
READ = $FF
WRITE = $00
```

```{.asm6502 #variables}
    .bss
atn_bytes: .res 1 ; indicates the number of bytes we want to transfer
    .export atn_bytes
atn_index: .res 1 ; indicates the current index into the ATN transfer
    .export atn_index
atn_buffer: .res MAX_ATN_BYTES ; holds the actual ATN bytes to send
    .export atn_buffer
ser_rw: .res 1 ; indicates a read or a write
    .export ser_rw
ser_bytes: .res 1 ; indicates the number of bytes to read/write over serial
    .export ser_bytes
ser_index: .res 1 ; indicates the current index into the serial transfer
    .export ser_index
    .zeropage
ser_pointer: .res 2 ; pointer to wherever we want to send/receive data
    .export ser_pointer
ser_eof: .res 1 ; flag for the end of the current transfer is also end of file
```

```{.asm6502 #constants}
MAX_ATN_BYTES = $FF
```

Now, let's transfer some bytes!

```{.asm6502 #transfer_bytes}
XferLoop:
    lda atn_bytes   ; check if we wanna send an ATN command
    beq SendAtn
    lda ser_bytes   ; check if we wanna send/receive normal data
    beq XferDone
<<read_or_write>>
    jmp XferDone
SendAtn:
<<send_atn>>
```

#### Sending ATN's

Let's get into the ATN stuff, since that's what we care about.

First, let's make sure the ATN line is actually on.

```{.asm6502 #send_atn}
    bit atn_on_flag
    bmi SkipTurnOn
    jsr AtnOn
    jsr ClkOn
    jsr DataOff
    jsr Wait1kUs
    lda #$FF
    sta atn_on_flag
SkipTurnOn:
```

```{.asm6502 #variables}
    .bss
atn_on_flag: .res 1
    .export atn_on_flag
```

Now, we grab the byte and try to send.

```{.asm6502 #send_atn}
    ldy atn_index
    lda atn_buffer,y
    sta byte_buffer
    jsr TryWrite
```

```{.asm6502 #variables}
    .bss
byte_buffer: .res 1
    .export byte_buffer
```

TrySendByte returns carry set if succeeded, carry clear if failed. Failure would be due to a timeout.

```{.asm6502 #send_atn}
    bcc XferDone
```

If we succeeded, increment our buffer index. If we wrote all the bytes, then end the ATN command.

```{.asm6502 #send_atn}
    ldx atn_index
    inx
    cpx atn_bytes
    beq AtnDone
    stx atn_index
    jmp XferLoop
AtnDone:
    jsr AtnOff
    lda #0
    sta atn_bytes
    sta atn_on_flag
    jmp XferDone
```

#### Regular Serial Transfer

##### Serial Writes

We check if we're doing a read or a write.

```{.asm6502 #read_or_write}
    .assert READ>=$80 && WRITE<$80,error,"expect to BIT a r/w flag"
    bit ser_rw
    bmi SerRead
SerWrite:
<<ser_write>>
SerRead:
<<ser_read>>
```

To start our write, we must output a TALK ATN command.

```{.asm6502 #ser_write}
    bit ser_online_flag
    bmi WriteOnline
    lda #LISTEN
    clc
    adc ser_dev
    sta atn_buffer
    lda #SECOND
    clc
    adc ser_second
    sta atn_buffer+1
    lda #2
    sta atn_bytes
    lda #0
    sta atn_index
    lda #$FF
    sta ser_online_flag
WriteOnline:
```

```{.asm6502 #variables}
    .bss
ser_online_flag: .res 1
    .export ser_online_flag
ser_dev: .res 1
    .export ser_dev
ser_second: .res 1
    .export ser_second
```

```{.asm6502 #constants}
LISTEN = $20
SECOND = $60
```

Once *ser_online_flag* is set, the target device is listening. It doesn't matter if the ATN command was paused in the middle, and we returned from our interrupt; the device kept listening to the ATN command for as long as we wished, and at the end it set itself up as listening.

Now to grab a byte and try to send it.

```{.asm6502 #ser_write}
    ldy ser_index
    lda (ser_pointer),y
    sta byte_buffer
    jsr TryWrite
```

As for the ATN command, TryWrite returns carry clear if there was a timeout, carry set otherwise.

If we timed out, then we want to send an ATN command to have the device stop listening. This ATN command will also time out, which is fine.

```{.asm6502 #ser_write}
    bcs GoodWrite
    lda #UNLISTEN
    jsr AtnOne
    jmp XferLoop
```

```{.asm6502 #subrs}
    .code
    .export AtnOne
.proc AtnOne
    sta atn_buffer
    lda #1
    sta atn_bytes
    lda #0
    sta atn_index
    rts
.endproc
```

```{.asm6502 #constants}
UNLISTEN = $3F
```

If the byte sent successfully, then we increment our index, and kill the write if needbe.

```{.asm6502 #ser_write}
GoodWrite:
    ldx ser_index
    inx
    cpx ser_bytes
    beq WriteDone
    stx ser_index
    jmp XferLoop
```

If we finished the write, then we also need to send the `UNLISTEN` command to indicate final transfer, in addition to clearing out the serial xfer.

```{.asm6502 #ser_write}
    lda #0
    sta ser_bytes
    lda #UNLISTEN
    jsr AtnOne
    jmp XferLoop
```

Now, let's handle the actual TryWrite routine.

```{.asm6502 #subrs}
    .export TryWrite
.proc TryWrite
<<try_write>>
WriteDone:
    rts
.endproc
```

First, we signal we're ready to send a byte.

```{.asm6502 #try_write}
    jsr ClkOff
```

Now, we wait for the readers to become available, keeping in mind if we timeout, we should exit now.

This is handled via a subroutine, that conveniently also returns the timeout condition with carry clear.

```{.asm6502 #try_write}
    jsr WaitWrite
    bcc WriteDone
```

If this is the end-of-stream, we should delay 256 microseconds here.

```{.asm6502 #try_write}
    bit ser_eof
    bpl NoEof
    lda ser_index
    cpx ser_bytes
    bne NoEof
    jsr Wait256Us
NoEof:
```

Now, send each bit!

```{.asm6502 #try_write}
    ldx #8
WriteLoop:
    jsr ClkOn
    jsr DataOff
    lsr byte_buffer
    bcc WriteZero
    jsr DataOn
WriteZero:
    jsr Wait60Us
    jsr ClkOff
    jsr Wait60Us
    dex
    bne WriteLoop
    jsr ClkOn
    jsr DataOff
    jsr Wait1kUs
```

And, we're done!

##### Serial Reads

```{.asm6502 #ser_read}

```

#### Entry and Exit

We want to install our IRQ handler, and make sure that whatever IRQ handler that *was* there previous will still be called.

```{.asm6502 #setup_irq}
    .code
    .export SetupIrq
.proc SetupIrq
    php
    sei
    lda irq_vector
    sta old_irq
    lda irq_vector+1
    sta old_irq+1
    lda #<MyIrq
    sta irq_vector
    lda #>MyIrq
    sta irq_vector
    plp
    rts
.endproc
```

```{.asm6502 #wrapup_irq}
XferDone:
    jmp (old_irq)
```

The 6502 has a bug: if we jump thru a variable, and that variable straddles a page -- the hibyte in $xxFF and the lobyte in $xy00 -- then it'll jump to the wrong address. We'll align the variable just to be safe.

```{.asm6502 #variables}
    .bss
    .align 2 ; avoid indirect jump bug
old_irq: .res 2
    .export old_irq
```

#### Low Level Routines

Let's start with the easy stuff, the Wait routines. They wait in microseconds, and the CPU clock is conveniently measured in those! Althought, it takes 12 cycles/microseconds to get in and out of a subroutine.

```{.asm6502 #subrs}
    .export Wait60Us
.proc Wait60Us
    ; 6 cycles to JSR here
    ldy #9      ; +2 cycles=8
loop:
    dey         ; +2 cyles
    bne loop    ; +3 cyles while taken, +2 when falling thru
    ; we ran the loop 9 times, the first 8 took 5 cycles, the last took 4.
    ; 8+8*5+4=52
    nop         ; +2 cycles=54
    rts         ; +6 cycles = 60
.endproc
```

## Building

Let's put together our whole source file.

```{.asm6502 file=atntest.s}

<<constants>>

<<variables>>

<<irq>>

<<setup_irq>>

<<subrs>>
```

CA65 requires a linker config file.

```{.cfg file=atntest.cfg}
MEMORY {
    PRG_ADDR: start = $0000, size = 2;
    RAM: start = $801, size = $97FF;
    ZEROPAGE: start = $FB, size = 4, file = "";
}

SEGMENTS {
    ZEROPAGE: load = ZEROPAGE, type = zp;
    PRG_ADDR: load = PRG_ADDR, type = ro;
    BASIC_HEADER: load = RAM, type = ro;
    CODE: load = RAM, type = ro;
    RODATA: load = RAM, type = ro;
    DATA: load = RAM, type = rw;
    BSS: load = RAM, type = bss, define = yes;
}
```

We need to insert the boot header.

```{.asm6502 file=boot.s}
    .segment "PRG_ADDR"
    .word $801
    .segment "BASIC_HEADER"
    .word null_line
    .word 10
    .byte $9E,"2061",0
null_line:
    .word 0
    .assert *=2061,error,"bad BASIC header"
    .import Start
    jmp Start
```

Let's make our Makefile:

```{.make file=Makefile}
OBJECTS = boot.o atntest.o

all: atntest.prg

clean:
	rm -f atntest.prg atntest.ll $(OBJECTS)

atntest.prg atntest.ll: $(OBJECTS) atntest.cfg
	ld65 -o atntest.prg -C atntest.cfg $(OBJECTS)

%.o: %.s
	ca65 -o $@ $*.s
```

But, just for fun, we'll add a make target to generate a nice PDF via pandoc.

```{.make file=Makefile}
atntest.pdf: atntest.md
	pandoc -o atntest.pdf --filter pandoc-annotate-codeblocks atntest.md
```

## Conclusion
