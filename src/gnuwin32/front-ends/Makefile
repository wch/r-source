include ../MkRules
R_HOME = ../../..

ifdef DEBUG
  OPTFLAGS=-g
  LINKFLAGS=
  DLLFLAGS=
else
  OPTFLAGS=-O3 -Wall -pedantic
  LINKFLAGS=-s
  DLLFLAGS=-s
endif
CFLAGS=$(OPTFLAGS) -I../../include

all: $(R_HOME)/bin/Rgui.exe $(R_HOME)/bin/Rterm.exe \
     $(R_HOME)/bin/Rcmd.exe  $(R_HOME)/bin/RSetReg.exe  \
     $(R_HOME)/bin/Rproxy.dll $(R_HOME)/bin/R.exe \
     $(R_HOME)/bin/md5check.exe

$(R_HOME)/bin/Rgui.exe: Rgui.exe
	$(MKDIR) -p $(R_HOME)/bin
	$(CP) $^ $@

$(R_HOME)/bin/Rterm.exe: Rterm.exe
	$(MKDIR) -p $(R_HOME)/bin
	$(CP) $^ $@	

$(R_HOME)/bin/Rcmd.exe: Rcmd.exe
	$(MKDIR) -p $(R_HOME)/bin
	$(CP) $^ $@

$(R_HOME)/bin/RSetReg.exe: RSetReg.exe
	$(MKDIR) -p $(R_HOME)/bin
	$(CP) $^ $@

$(R_HOME)/bin/R.exe: R.exe
	$(MKDIR) -p $(R_HOME)/bin
	$(CP) $^ $@	

$(R_HOME)/bin/md5check.exe: md5check.exe
	$(MKDIR) -p $(R_HOME)/bin
	$(CP) $^ $@	

graphappmain.o: ../graphapp/graphappmain.c
	$(CC) -c -o $@ -I../graphapp $(CFLAGS) $^

rgui-CFLAGS=-DWin32
rterm-CFLAGS=-DWin32
Rgui-LINKFLAGS=-mwindows

Rterm-LIBS=-luser32 -L../../../bin -lR
Rgui-LIBS=-L../../../bin -lR
Rpwd-LIBS=-lkernel32

Rgui-LINKFLAGS+=-Wl,--large-address-aware
Rterm-LINKFLAGS+=-Wl,--large-address-aware
## Increase stack size to 10Mb
Rgui-LINKFLAGS+=-Wl,--stack=0xA00000
Rterm-LINKFLAGS+=-Wl,--stack=0xA00000

Rgui.exe: graphappmain.o  rgui.o rgico.o

Rterm.exe: graphappmain.o rterm.o rtico.o

Rcmd.exe: rcmd.o ../rhome.o ../shext.o rcico.o rcmdfn.o

R.exe: R.o ../rhome.o ../shext.o rcico.o rcmdfn.o

RSetReg.exe: RSetReg.o ../rhome.o rcico.o

md5check-CFLAGS=-I../../../include -I../../library/tools/src
md5check.exe : md5check.o md5ico.o ../../library/tools/src/md5.o

Rpwd:
	$(MAKE) -C ../../include -f Makefile.win version
	$(MAKE) Rpwd.exe

Rpwd.exe: rpwd.o rcico.o

rcmd.o R.o rgui.o rterm.o RSetReg.o rproxy_impl.o rgico.o rtico.o rcico.o: ../../include/Rversion.h

clean:
	$(RM) *.o *.obj *.exe *~ *.exp *.def *.dll *.lib *.a

bdx_util-CFLAGS=-D_IN_RPROXY_
bdx_com-CFLAGS=-D_IN_RPROXY_
bdx_SEXP-CFLAGS=-D_IN_RPROXY_
Rproxy-DLLFLAGS=-mwindows
Rproxy-DLLLIBS=-L../../../bin -lR -lole32 -luuid -loleaut32
## this one needs Rproxy.dll as the LIBRARY line
Rproxy.dll: rproxy.o rproxy_dev.o rproxy_impl.o com_util.o \
	bdx_util.o bdx_SEXP.o bdx_com.o rtico.o
	@$(ECHO) LIBRARY $*.dll > $*.def
	@$(ECHO) EXPORTS >> $*.def
	@$(NM) $^ | $(SED) -n 's/^........ [BCDRT] _/ /p' >> $*.def
	$(DLL) -shared $(DLLFLAGS) $($*-DLLFLAGS) -o $@ $*.def $^ $($*-DLLLIBS) $(DLLLIBS)
	@$(RM) $*.def

$(R_HOME)/bin/Rproxy.dll: Rproxy.dll
	$(MKDIR) -p $(R_HOME)/bin
	$(CP) Rproxy.dll $@	
