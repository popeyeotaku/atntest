# ~/~ begin <<atntest.md#Makefile>>[init]
OBJECTS = boot.o atntest.o

all: atntest.prg

clean:
	rm -f atntest.prg atntest.ll $(OBJECTS)

atntest.prg atntest.ll: $(OBJECTS) atntest.cfg
	ld65 -o atntest.prg -C atntest.cfg $(OBJECTS)

%.o: %.s
	ca65 -o $@ $*.s
# ~/~ end
