include ../MkRules

ifdef DEBUG
  OPTFLAGS=-g
  LINKFLAGS=
  DLLFLAGS=
else
  OPTFLAGS=-O2 -Wall -pedantic
  LINKFLAGS=-s
  DLLFLAGS=-s
endif
CFLAGS=$(OPTFLAGS) -I../../include

all: ../../../bin/Rgui.exe ../../../bin/Rterm.exe ../../../bin/Rproxy.dll

../../../bin/Rgui.exe: Rgui.exe
	$(MKDIR) -p ../../../bin
	$(CP) $^ $@

../../../bin/Rterm.exe: Rterm.exe
	$(MKDIR) -p ../../../bin
	$(CP) $^ $@	

graphappmain.o: ../graphapp/graphappmain.c
	$(CC) -c -o $@ -I../graphapp $(CFLAGS) $^

LIBS=-L.. -lR

Rgui-LINKFLAGS=-mwindows
Rterm-LIBS=-luser32
rgui-CFLAGS=-DWin32
rterm-CFLAGS=-DWin32

Rgui.exe: graphappmain.o  rgui.o rgico.o

Rterm.exe: graphappmain.o rterm.o rtico.o

rgui.o rterm.o rproxy_impl.o: ../../include/Rversion.h

clean:
	$(RM) *.o *.obj *.exe *~ *.exp *.def *.dll *.lib

bdx-CFLAGS=-I../../include
rproxy-DLLFLAGS=-mwindows
rproxy-DLLLIBS=-L.. -lR
rproxy.dll: rproxy.o rproxy_impl.o bdx.o rtico.o
rprx.exe: rprx.o

../../../bin/Rproxy.dll: rproxy.dll
	$(MKDIR) -p ../../../bin
	$(CP) $^ $@	
