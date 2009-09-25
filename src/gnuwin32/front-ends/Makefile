include ../MkRules
R_HOME = ../../..

ifdef DEBUG
  OPTFLAGS=$(G_FLAG) -O3 -Wall
  LINKFLAGS=
  DLLFLAGS=
else
  OPTFLAGS=-O3 -Wall -pedantic
  LINKFLAGS=-s
  DLLFLAGS=-s
endif
CPPFLAGS=-I../../include
CFLAGS=$(OPTFLAGS)

## Increase stack size to 10Mb
LARGE_STACK=-Wl,--stack=0xA00000
## allow 32-bit builds to address over 2Gb
ifeq ($(strip $(WIN)),32)
LARGE_ADDR=-Wl,--large-address-aware
endif

.PHONY : all makeMakedeps

all: makeMakedeps \
     $(R_HOME)/bin/Rgui.exe $(R_HOME)/bin/Rterm.exe \
     $(R_HOME)/bin/Rcmd.exe  $(R_HOME)/bin/RSetReg.exe  \
     $(R_HOME)/bin/R.exe \
     $(R_HOME)/bin/md5check.exe $(R_HOME)/bin/Rscript.exe \
     $(R_HOME)/bin/open.exe 

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

$(R_HOME)/bin/Rscript.exe: Rscript.exe
	$(MKDIR) -p $(R_HOME)/bin
	$(CP) $^ $@	

$(R_HOME)/bin/open.exe: open.exe
	$(MKDIR) -p $(R_HOME)/bin
	$(CP) $^ $@	

rgui-CPPFLAGS=-DWin32
rterm-CPPFLAGS=-DWin32

## -luser32 -lkernel32 -ladvapi32 -lshell32 are now standard
Rgui-LIBS=-L../../../$(IMPDIR) -lR $(GRAPHAPP_LIB)
Rscript-LIBS=-L../../../$(IMPDIR) -lR $(GRAPHAPP_LIB)
Rterm-LIBS=-L../../../$(IMPDIR) -lR $(GRAPHAPP_LIB)

Rgui-LINKFLAGS=-mwindows $(LARGE_ADDR) $(LARGE_STACK)
Rterm-LINKFLAGS=$(LARGE_ADDR) $(LARGE_STACK)
Rscript-LINKFLAGS=$(LARGE_ADDR) $(LARGE_STACK)

Rgui.exe: graphappmain.o  rgui.o rgico.o

Rterm.exe: graphappmain.o rterm.o rtico.o

Rcmd.exe: rcmd.o ../rhome.o ../shext.o rcico.o rcmdfn.o Renviron.o

R.exe: R.o ../rhome.o ../shext.o rcico.o rcmdfn.o Renviron.o

RSetReg.exe: RSetReg.o ../rhome.o rcico.o

md5check-CPPFLAGS=-I../../../include -I../../library/tools/src
md5check.exe : md5check.o md5ico.o ../../library/tools/src/md5.o

Rscript.o: ../../unix/Rscript.c
	$(CC) $(CFLAGS) -I. -I../../include -DHAVE_CONFIG_H -c $< -o $@

Rscript.exe: Rscript.o rcico.o

Rpwd:
	$(MAKE) -C ../../include -f Makefile.win version
	$(MAKE) Rpwd.exe

Rpwd.exe: rpwd.o rcico.o

rcmd.o R.o rgui.o rterm.o RSetReg.o rgico.o rtico.o rcico.o: ../../include/Rversion.h
rgico.o: Rgui.exe.manifest
rtico.o: Rterm.exe.manifest
rcico.o: Rcmd.exe.manifest

open.o: open.c
open.exe: open.o

clean:
	$(RM) *.o *.obj *.exe *~ *.exp *.dll *.lib *.a *.d Makedeps

CSOURCES=$(filter-out R.c rcmd.c rpwd.c rtest.c, $(wildcard *.c))
DEPS=$(CSOURCES:.c=.d)

makeMakedeps: $(DEPS)
	@$(RM) Makedeps
ifneq ($(strip $(DEPS)),)
	@cat $(DEPS) >> Makedeps
endif

-include Makedeps
