; ~/~ begin <<atntest.md#boot.s>>[init]
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
; ~/~ end