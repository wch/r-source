srcdir=.
top_srcdir=../..
 
top_builddir = ../..
subdir=tests/Embedding
 
include $(top_builddir)/Makeconf

CFLAGS+= -I../../include -I../../src/include 

R_SRC=code.R foo.R

EXECS=Rtest Rplot Rpostscript Rerror RNamedCall RParseEval Rpackage

SRC=\
  Rtest.c \
  Rerror.c \
  Rplot.c \
  Rpostscript.c \
  Rerror.c \
  embeddedRCall.c \
  RNamedCall.c \
  RParseEval.c \
  Rpackage.c
 
HEADERS=embeddedRCall.h

tests: $(EXECS)
	(R_HOME=../.. ; export R_HOME ; ./Rtest --silent)
	(R_HOME=../.. ; export R_HOME ; ./RNamedCall --silent)
	(R_HOME=../.. ; export R_HOME ; ./RParseEval --silent)
	(R_HOME=../.. ; export R_HOME ; ./Rpackage --silent)
	(R_HOME=../.. ; export R_HOME ; ./Rerror --silent)
	(R_HOME=../.. ; export R_HOME ; ./Rplot --silent)
	(R_HOME=../.. ; export R_HOME ; ./Rpostscript --silent --gui=none --no-save)


all: Rtest Rplot Rpostscript Rerror RNamedCall RParseEval

Rtest: Rtest.o embeddedRCall.o
	../../bin/R CMD LINK $(CC) -o $@ Rtest.o embeddedRCall.o -L../../bin -lR

Rplot: Rplot.o embeddedRCall.o
	../../bin/R CMD LINK $(CC) -o $@ Rplot.o embeddedRCall.o -L../../bin -lR

Rpostscript: Rpostscript.o embeddedRCall.o
	../../bin/R CMD LINK $(CC) -o $@ Rpostscript.o embeddedRCall.o -L../../bin -lR

Rerror: Rerror.o embeddedRCall.o
	../../bin/R CMD LINK $(CC) -o $@ Rerror.o embeddedRCall.o -L../../bin -lR

Rpackage: Rpackage.o  embeddedRCall.o
	../../bin/R CMD LINK $(CC) -o $@ Rpackage.o embeddedRCall.o -L../../bin -lR

RNamedCall: RNamedCall.o embeddedRCall.o
	../../bin/R CMD LINK $(CC) -o $@ RNamedCall.o embeddedRCall.o -L../../bin -lR

RParseEval: RParseEval.o embeddedRCall.o
	../../bin/R CMD LINK $(CC) -o $@ RParseEval.o embeddedRCall.o -L../../bin -lR


R.so: showArgs.o
	$(SHLIB_LINK) -o $@ showArgs.o

clean:
	for f in $(SRC:%.c=%.o) Rplots.ps ; do \
	  if test -f $$f ; then rm  -f $$f ; fi \
	done

distclean: clean
	for f in $(EXECS) Rplots.ps config.status config.cache config.log ; do \
	  if test -f $$f ; then rm  -f $$f ; fi \
	done

configure: configure.in
	autoconf


