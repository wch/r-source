include ../MkRules

all: makeMakedeps ga.a

ifdef DEBUG
  DEBUGFLAG=-g
else
  DEBUGFLAG=
endif

CFLAGS=-I. -I.. -I../../include -Wall $(DEBUGFLAG)  -O2 -DINDLL -DR_DLL_BUILD
ifdef SUPPORT_GUI_MBCS
CFLAGS += -DSUPPORT_GUI_MBCS
endif
ifdef SUPPORT_UTF8
CFLAGS += -DSUPPORT_UTF8
endif
SOURCES=$(filter-out graphappmain.c, $(wildcard *.c))
OBJS=$(foreach i, $(SOURCES), $(basename $i).o)

ga.a: $(OBJS)

clean:
	$(RM) *.o *.a *~ \#*\# *.d Makedeps

# Dependencies
DEPS=$(SOURCES:.c=.d)

makeMakedeps: $(DEPS)
	@$(RM) Makedeps
	@cat $(DEPS) >> Makedeps

-include Makedeps
