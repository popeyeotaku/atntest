# ~/~ begin <<atntest.md#Makefile>>[init]
.PHONY: all clean pdf

all: atntest.pdf atntest.prg

clean:
	rm -f atntest.prg atntest.ll $(OBJECTS) atntest.pdf

pdf: atntest.pdf

atntest.pdf: atntest.md
	pandoc -o atntest.pdf --filter pandoc-annotate-codeblocks atntest.md

# ~/~ begin <<atntest.md#makefile>>[init]
OBJECTS = boot.o atntest.o

atntest.prg atntest.ll: $(OBJECTS) atntest.cfg
	ld65 -Ln atntest.ll -o atntest.prg -C atntest.cfg $(OBJECTS)

%.o: %.s
	ca65 -o $@ $*.s
# ~/~ end
# ~/~ end
