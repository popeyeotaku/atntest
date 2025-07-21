; ~/~ begin <<atntest.md#atntest.s>>[init]
; ~/~ begin <<atntest.md#constants>>[init]
VIC_SCANLINE = $D012
; ~/~ end
; ~/~ begin <<atntest.md#constants>>[1]
CYCLES_PER_LINE=65 ; on NTSC, PAL is 63
TARGET_MICROSECONDS = 10000
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
; ~/~ begin <<atntest.md#constants>>[6]
IRQ_VECTOR = $0314 ; pointer to IRQ service routine
; ~/~ end
; ~/~ begin <<atntest.md#constants>>[7]
TRUE = $FF
FALSE = 0
ACTIVE_1_OUT = TRUE
ACTIVE_1_IN = FALSE
; ~/~ end
; ~/~ begin <<atntest.md#constants>>[8]
CIA_PORT = $DD00
; ~/~ end
; ~/~ begin <<atntest.md#constants>>[9]
ATN_OUT = 1<<3
CLK_OUT = 1<<4
DATA_OUT = 1<<5
CLK_IN = 1<<6
DATA_IN = 1<<7
; ~/~ end
; ~/~ begin <<atntest.md#constants>>[10]
WRITE_ON_DATA_LO = !ACTIVE_1_IN
; ~/~ end
; ~/~ begin <<atntest.md#constants>>[11]
CARRIAGE_RETURN = $0D
; ~/~ end
; ~/~ begin <<atntest.md#constants>>[12]
CHROUT = $FFD2
; ~/~ end

; ~/~ begin <<atntest.md#macros>>[init]
.if ACTIVE_1_OUT = TRUE
ACTIVE_HI_IN = TRUE
    .mac bit_on b
        lda CIA_PORT
        ora #b
        sta CIA_PORT
    .endmac
    .mac bit_off b
        lda CIA_PORT
        and #<~b
        sta CIA_PORT
    .endmac
.else
    .mac bit_on b
        lda CIA_PORT
        and #<~b
        sta CIA_PORT
    .endmac
    .mac bit_off b
        lda CIA_PORT
        ora #b
        sta CIA_PORT
    .endmac
.endif
; ~/~ end

    .zeropage
; ~/~ begin <<atntest.md#zeropage>>[init]
ser_pointer: .res 2 ; pointer to wherever we want to send/receive data
    .export ser_pointer
; ~/~ end

    .bss
; ~/~ begin <<atntest.md#variables>>[init]
start_line:  .res 2
target_line: .res 2
    .export start_line,target_line
; ~/~ end
; ~/~ begin <<atntest.md#variables>>[1]
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
ser_eof: .res 1 ; flag for the end of the current transfer is also end of file
    .export ser_eof
; ~/~ end
; ~/~ begin <<atntest.md#variables>>[2]
atn_on_flag: .res 1
    .export atn_on_flag
; ~/~ end
; ~/~ begin <<atntest.md#variables>>[3]
byte_buffer: .res 1
    .export byte_buffer
; ~/~ end
; ~/~ begin <<atntest.md#variables>>[4]
ser_online_flag: .res 1
    .export ser_online_flag
ser_dev: .res 1
    .export ser_dev
ser_second: .res 1
    .export ser_second
; ~/~ end
; ~/~ begin <<atntest.md#variables>>[5]
    .align 2 ; avoid indirect jump bug
old_irq: .res 2
    .export old_irq
; ~/~ end

    .code
; ~/~ begin <<atntest.md#main>>[init]
    .export Start
.proc Start
; ~/~ begin <<atntest.md#init-test>>[init]
; ~/~ begin <<atntest.md#clear-bss>>[init]
    lda #<__BSS_LOAD__
    sta ser_pointer
    lda #>__BSS_LOAD__
    sta ser_pointer+1
    ldy #0
BssLoop:
    lda ser_pointer+1
    cmp #>(__BSS_LOAD__+__BSS_SIZE__)
    bne BssClear
    lda ser_pointer
    cmp #<(__BSS_LOAD__+__BSS_SIZE__)
    beq BssDone
BssClear:
    tya                     ; Y=0
    sta (ser_pointer),y
    inc ser_pointer
    bne BssLoop
    inc ser_pointer+1
    jmp BssLoop
BssDone:
    ldx #0
    tya
ZpLoop:
    sta (<__ZEROPAGE_LOAD__),y
    inx
    cpx #<__ZEROPAGE_SIZE__
    bne ZpLoop
    .import __BSS_LOAD__,__BSS_SIZE__,__ZEROPAGE_LOAD__,__ZEROPAGE_SIZE__
; ~/~ end
    jsr SetupIrq
; ~/~ end
; ~/~ begin <<atntest.md#run-test>>[init]
RunTest:
    .export RunTest
; ~/~ end
; ~/~ begin <<atntest.md#run-test>>[1]
    lda #<print_msg
    sta ser_pointer
    lda #>print_msg
    sta ser_pointer+1
    lda #4
    sta ser_dev
    lda #0
    sta ser_index
    lda #$FF
    sta ser_eof
    lda #print_msg_end-print_msg
    sta ser_bytes
; ~/~ end
; ~/~ begin <<atntest.md#run-test>>[2]
    jsr PrintScreen
; ~/~ end
; ~/~ begin <<atntest.md#run-test>>[3]
; ~/~ begin <<atntest.md#wait-for-ser>>[init]
:
    lda ser_bytes
    bne :-
; ~/~ end
; ~/~ begin <<atntest.md#wait-for-atn>>[init]
:
    lda atn_bytes
    bne :-
; ~/~ end
; ~/~ end
; ~/~ begin <<atntest.md#run-test>>[4]
    jmp *
; ~/~ end
.endproc
; ~/~ end

; ~/~ begin <<atntest.md#irq>>[init]
    .export MyIrq
.proc MyIrq
    lda VIC_SCANLINE
    sta start_line
    clc
    adc #LINES_FOR_XFER
    sta target_line

; ~/~ begin <<atntest.md#transfer_bytes>>[init]
XferLoop:
    lda atn_bytes   ; check if we have any ATN command bytes we wanna send
    bne SendAtn
    lda ser_bytes   ; check if we have any normal data bytes we wanna send
    bne :+
    jmp XferDone
:
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
    ldx #1
    lda ser_second  ; send secondary address, if any
    beq :+
    clc
    adc #SECOND
    adc ser_second
    sta atn_buffer+1
    inx
:
    stx atn_bytes
    lda #0
    sta atn_index
    lda #$FF
    sta ser_online_flag
    bne XferLoop            ; Always taken. This will proceed with sending the ATN LISTEN command.
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
    lda #0
    sta ser_online_flag
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
WriteDone:
    lda #0
    sta ser_bytes
    sta ser_index
    lda #UNLISTEN
    jsr AtnOne
    jmp XferLoop
; ~/~ end
SerRead:
; ~/~ begin <<atntest.md#ser_read>>[init]
    brk ; TODO!
; ~/~ end
; ~/~ end
    jmp XferLoop
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
    sta atn_index
    sta atn_on_flag
    jmp XferLoop
; ~/~ end
; ~/~ end

; ~/~ begin <<atntest.md#wrapup_irq>>[init]
XferDone:
    jmp (old_irq)           ; run the regular IRQ handler now that we're finished!
; ~/~ end
.endproc
; ~/~ end

; ~/~ begin <<atntest.md#setup_irq>>[init]
    .export SetupIrq
.proc SetupIrq
    php                     ; save interrupt flag
    sei                     ; disable interrupts
    lda IRQ_VECTOR          ; save the current IRQ handler
    sta old_irq
    lda IRQ_VECTOR+1
    sta old_irq+1
    lda #<MyIrq             ; store ours!
    sta IRQ_VECTOR
    lda #>MyIrq
    sta IRQ_VECTOR+1
    plp                     ; restore interrupt flags, since they're now safe to occur
    rts
.endproc
; ~/~ end

; ~/~ begin <<atntest.md#subrs>>[init]
    .export AtnOne
.proc AtnOne
    sta atn_buffer
    lda #0
    sta atn_index
    lda #1
    sta atn_bytes
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
    lda #0          ; clear EOF flag for next transfer
    sta ser_eof
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
    jsr Wait1kUs        ; for LISTENers to have time to respond "not ready"
    jsr Wait100Us       ; minimum "between bytes time"
    sec
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
Loop:
    dey         ; +2 cyles
    bne Loop    ; +3 cyles while taken, +2 when falling thru
    ; we ran the loop 9 times, the first 8 took 5 cycles, the last took 4.
    ; 8+8*5+4=52
    nop         ; +2 cycles=54
    rts         ; +6 cycles = 60
.endproc
; ~/~ end
; ~/~ begin <<atntest.md#subrs>>[3]
    .export Wait100Us
.proc Wait100Us
    ; 6 cycles to JSR here
    ldy #17         ; +2
Loop:
    dey             ; +2
    bne Loop        ; +3 cycles while taken, +2 when falling thru
    ; last loop thru took 4 cycles, rest took 5
    ; 8+5*(y-1)+4=100-6
    ; 5*(y-1)=94-8-4
    ; y-1=82/5
    ; y=16.4+1
    ; 8+5*(17-1)+4=92
    nop             ; 92+2=94
    rts             ; 96+6=100
.endproc
; ~/~ end
; ~/~ begin <<atntest.md#subrs>>[4]
    .export Wait256Us
.proc Wait256Us
    ; 6 cycles to JSR here
    ldy #48        ; +2
Loop:
    dey             ; +2
    bne Loop        ; +3 cycles while taken, +2 when falling thru
    ; last loop thru took 4 cycles, rest took 5
    ; 8+5*(y-1)+4=250
    ; 5*(y-1)=250-8-4
    ; y-1=238/5
    ; y=47.6+1
    ; y=48
    ; 8+5*47+4=247
    nop             ; +2 cycles=249
    rts             ; +6 cycles=255, which is close enough
.endproc
; ~/~ end
; ~/~ begin <<atntest.md#subrs>>[5]
    .export Wait1kUs
.proc Wait1kUs
    ; 6 cycles to JSR here
    ; 1000/255=3.9
    jsr Wait256Us   ; +255=261
    jsr Wait256Us   ; +255=516
    jsr Wait256Us   ; +255=771
    ; 229 cycles remaining
    ldy #74        ; +2 cycles=773
Loop:
    dey             ; +2 cycles
    bne Loop        ; +3 cycles every loop thru until last, which takes +2
    ; 773+3*(y-1)+2=1000-6
    ; 3*(y-1)=1000-6-773-2
    ; y-1=219/3
    ; y=73+1
    ; 773+3*(74-1)+2=994
    rts             ; +6 cycles on exit
.endproc
; ~/~ end
; ~/~ begin <<atntest.md#subrs>>[6]
AtnOn:
    bit_on ATN_OUT
    rts
AtnOff:
    bit_off ATN_OUT
    rts
ClkOn:
    bit_on CLK_OUT
    rts
ClkOff:
    bit_off CLK_OUT
    rts
DataOn:
    bit_on DATA_OUT
    rts
DataOff:
    bit_off DATA_OUT
    rts
.export AtnOn,AtnOff,ClkOn,ClkOff,DataOn,DataOff
; ~/~ end
; ~/~ begin <<atntest.md#subrs>>[7]
.export WaitWrite
WaitWrite:
    .assert DATA_IN = $80,error,"DATA_IN isn't in bit7"
    jsr DataOff         ; ensure we're getting our DATA from the LISTENers
WaitLoop:
    jsr CheckScanline
    bcc :+
    bit CIA_PORT
    .if WRITE_ON_DATA_LO = TRUE
        bmi WaitLoop
    .else
        bpl WaitLoop
    .endif
    sec ; we're good to write!
:
    rts
; ~/~ end
; ~/~ begin <<atntest.md#subrs>>[8]
; Return carry clear if we've timed out on our xfer time, carry set otherwise.
.export CheckScanline
.proc CheckScanline
    lda target_line
    cmp start_line
    bcc OrCheck
AndCheck:
    lda VIC_SCANLINE
    cmp start_line
    bcc TimeOut
    cmp target_line
    bcc TimeIn
TimeOut:
    clc
    rts
OrCheck:
    lda VIC_SCANLINE
    cmp start_line
    bcs TimeIn
    cmp target_line
    bcs TimeOut
TimeIn:
    sec
    rts
.endproc
; ~/~ end
; ~/~ begin <<atntest.md#subrs>>[9]
    .export PrintScreen
.proc PrintScreen
    ldx #0
PrintLoop:
    lda print_msg,x
    beq PrintDone
    jsr CHROUT
    inx
    bne PrintLoop
PrintDone:
    rts
.endproc
; ~/~ end
    .rodata
; ~/~ begin <<atntest.md#data>>[init]
print_msg:
    .byte CARRIAGE_RETURN,"HELLO, WORLD",CARRIAGE_RETURN,"NICE TO MEET YOU!",CARRIAGE_RETURN,0
print_msg_end:
    .export print_msg, print_msg_end
; ~/~ end
; ~/~ end
