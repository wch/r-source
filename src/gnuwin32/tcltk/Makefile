include ../MkRules

ifdef DEBUG
 OPTFLAGS=-g -O2 -Wall
 DLLFLAGS=
else
 OPTFLAGS=-O2 -Wall -pedantic
 DLLFLAGS=-s 
endif

CFLAGS=$(OPTFLAGS) -I../../include -I. -DHAVE_CONFIG_H

TCL_HOME = c:/packages/Tcl
tcltk_glue-CFLAGS += -I $(TCL_HOME)/include
Rtcltk-DLLLIBS = -L.. -lR -L. -ltcl83 -ltk83

all: implibs Rtcltk.dll installdll

Rtcltk.dll: tcltk_glue.o
implibs: libtcl83.a libtk83.a
libtcl83.a: tcl83.def
libtk83.a: tk83.def

installdll: 
	$(CP) Rtcltk.dll ../../../bin

clean:
	$(RM) *.o *.a *.dll Rtcltk.def *~ \#*\#




