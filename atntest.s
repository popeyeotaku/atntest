; ~/~ begin <<atntest.md#atntest.s>>[init]

; ~/~ begin <<atntest.md#constants>>[init]
vic_scanline = $D012
; ~/~ end
; ~/~ begin <<atntest.md#constants>>[1]
CYCLES_PER_LINE=65 ; on NTSC, PAL is 63
TARGET_MICROSECONDS = 6000
LINES_FOR_XFER = TARGET_MICROSECONDS / CYCLES_PER_LINE
; ~/~ end
; ~/~ begin <<atntest.md#constants>>[2]
READ = $FF
WRITE = $00
; ~/~ end
; ~/~ begin <<atntest.md#constants>>[3]
MAX_ATN_BYTES = $FF
; ~/~ end
; ~/~ begin <<atntest.md#constants>>[4]
LISTEN = $20
SECOND = $60
; ~/~ end
; ~/~ begin <<atntest.md#constants>>[5]
UNLISTEN = $3F
; ~/~ end

; ~/~ begin <<atntest.md#variables>>[init]
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
; ~/~ end
; ~/~ begin <<atntest.md#variables>>[1]
    .bss
atn_on_flag: .res 1
    .export atn_on_flag
; ~/~ end
; ~/~ begin <<atntest.md#variables>>[2]
    .bss
byte_buffer: .res 1
    .export byte_buffer
; ~/~ end
; ~/~ begin <<atntest.md#variables>>[3]
    .bss
ser_online_flag: .res 1
    .export ser_online_flag
ser_dev: .res 1
    .export ser_dev
ser_second: .res 1
    .export ser_second
; ~/~ end
; ~/~ begin <<atntest.md#variables>>[4]
    .bss
    .align 2 ; avoid indirect jump bug
old_irq: .res 2
    .export old_irq
; ~/~ end

; ~/~ begin <<atntest.md#irq>>[init]
    .code
    .export MyIrq
.proc MyIrq
    lda vic_scanline
    clc
    adc #LINES_FOR_XFER
    sta target_line

; ~/~ begin <<atntest.md#transfer_bytes>>[init]
XferLoop:
    lda atn_bytes   ; check if we wanna send an ATN command
    beq SendAtn
    lda ser_bytes   ; check if we wanna send/receive normal data
    beq XferDone
; ~/~ begin <<atntest.md#read_or_write>>[init]
    .assert READ>=$80 && WRITE<$80,error,"expect to BIT a r/w flag"
    bit ser_rw
    bmi SerRead
SerWrite:
; ~/~ begin <<atntest.md#ser_write>>[init]
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
; ~/~ end
; ~/~ begin <<atntest.md#ser_write>>[1]
    ldy ser_index
    lda (ser_pointer),y
    sta byte_buffer
    jsr TryWrite
; ~/~ end
; ~/~ begin <<atntest.md#ser_write>>[2]
    bcs GoodWrite
    lda #UNLISTEN
    jsr AtnOne
    jmp XferLoop
; ~/~ end
; ~/~ begin <<atntest.md#ser_write>>[3]
GoodWrite:
    ldx ser_index
    inx
    cpx ser_bytes
    beq WriteDone
    stx ser_index
    jmp XferLoop
; ~/~ end
; ~/~ begin <<atntest.md#ser_write>>[4]
    lda #0
    sta ser_bytes
    lda #UNLISTEN
    jsr AtnOne
    jmp XferLoop
; ~/~ end
SerRead:
; ~/~ begin <<atntest.md#ser_read>>[init]
; ~/~ end
; ~/~ end
    jmp XferDone
SendAtn:
; ~/~ begin <<atntest.md#send_atn>>[init]
    bit atn_on_flag
    bmi SkipTurnOn
    jsr AtnOn
    jsr ClkOn
    jsr DataOff
    jsr Wait1kUs
    lda #$FF
    sta atn_on_flag
SkipTurnOn:
; ~/~ end
; ~/~ begin <<atntest.md#send_atn>>[1]
    ldy atn_index
    lda atn_buffer,y
    sta byte_buffer
    jsr TryWrite
; ~/~ end
; ~/~ begin <<atntest.md#send_atn>>[2]
    bcc XferDone
; ~/~ end
; ~/~ begin <<atntest.md#send_atn>>[3]
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
; ~/~ end
; ~/~ end

; ~/~ begin <<atntest.md#wrapup_irq>>[init]
XferDone:
    jmp (old_irq)
; ~/~ end
.endproc
; ~/~ end

; ~/~ begin <<atntest.md#setup_irq>>[init]
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
; ~/~ end

; ~/~ begin <<atntest.md#subrs>>[init]
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
; ~/~ end
; ~/~ begin <<atntest.md#subrs>>[1]
    .export TryWrite
.proc TryWrite
; ~/~ begin <<atntest.md#try_write>>[init]
    jsr ClkOff
; ~/~ end
; ~/~ begin <<atntest.md#try_write>>[1]
    jsr WaitWrite
    bcc WriteDone
; ~/~ end
; ~/~ begin <<atntest.md#try_write>>[2]
    bit ser_eof
    bpl NoEof
    lda ser_index
    cpx ser_bytes
    bne NoEof
    jsr Wait256Us
NoEof:
; ~/~ end
; ~/~ begin <<atntest.md#try_write>>[3]
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
; ~/~ end
WriteDone:
    rts
.endproc
; ~/~ end
; ~/~ begin <<atntest.md#subrs>>[2]
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
; ~/~ end
; ~/~ end
