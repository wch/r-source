include ../MkRules

all: ../../../bin/Rgui.exe ../../../bin/Rterm.exe


../../../bin/Rgui.exe: Rgui.exe
	$(MKDIR) -p ../../../bin
	$(CP) $^ $@

../../../bin/Rterm.exe: Rterm.exe
	$(MKDIR) -p ../../../bin
	$(CP) $^ $@	

graphappmain.o: ../graphapp/graphappmain.c
	$(CC) -c -o $@ -I../graphapp $(CFLAGS) $^

LIBS=-L.. -lR
LINKFLAGS=-s

Rgui-LINKFLAGS=-mwindows 
rgui-CFLAGS=-I../../include 
rterm-CFLAGS=-I../../include 
Rgui.exe: graphappmain.o  rgui.o rgico.o

Rterm.exe: graphappmain.o rterm.o rtico.o


clean:
	$(RM) *.o *.exe *~ *.exp
