include ../MkRules

ifdef DEBUG
  DEBUGFLAG=-g
  LINKFLAGS=
else
  DEBUGFLAG=
  LINKFLAGS=-s
endif

all: ../../../bin/Rgui.exe ../../../bin/Rterm.exe

../../../bin/Rgui.exe: Rgui.exe
	$(MKDIR) -p ../../../bin
	$(CP) $^ $@

../../../bin/Rterm.exe: Rterm.exe
	$(MKDIR) -p ../../../bin
	$(CP) $^ $@	

graphappmain.o: ../graphapp/graphappmain.c
	$(CC) -c -o $@ -I../graphapp $(DEBUGFLAG) $(CFLAGS) $^

LIBS=-L.. -lR

Rgui-LINKFLAGS=-mwindows 
rgui-CFLAGS=-I../../include -DWin32
rterm-CFLAGS=-I../../include -DWin32

Rgui.exe: graphappmain.o  rgui.o rgico.o

Rterm.exe: graphappmain.o rterm.o rtico.o

rgui.o rterm.o rproxy.o: ../../include/Rversion.h

clean:
	$(RM) *.o *.exe *~ *.exp


