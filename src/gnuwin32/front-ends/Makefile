include ../MkRules

ifdef DEBUG
  DEBUGFLAG=-g
  LINKFLAGS=
else
  DEBUGFLAG=
  LINKFLAGS=-s
endif

all: ../../../bin/Rgui.exe ../../../bin/Rterm.exe ../../../bin/Rproxy.dll

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
Rterm-LIBS=-luser32
rgui-CFLAGS=-I../../include -DWin32
rterm-CFLAGS=-I../../include -DWin32

Rgui.exe: graphappmain.o  rgui.o rgico.o

Rterm.exe: graphappmain.o rterm.o rtico.o

rgui.o rterm.o rproxy_impl.o: ../../include/Rversion.h

clean:
	$(RM) *.o *.obj *.exe *~ *.exp *.def *.dll *.lib

rproxy-CFLAGS=-I../../include -Wall
rproxy_impl-CFLAGS=-I../../include -Wall
bdx-CFLAGS=-I../../include -I.. -Wall
rproxy-DLLFLAGS=-mwindows -L.. -Wall
rproxy-DLLLIBS=-lR
rproxy.dll: rproxy.o rproxy_impl.o bdx.o rtico.o

../../../bin/Rproxy.dll: rproxy.dll
	$(MKDIR) -p ../../../bin
	$(CP) $^ $@	
