include ../MkRules

all: makeMakedeps ga.a

CFLAGS=-I. -I.. -I../../include $(OPTFLAGS) -DINDLL -DR_DLL_BUILD
SOURCES=$(filter-out graphappmain.c xredef.c, $(wildcard *.c))
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
