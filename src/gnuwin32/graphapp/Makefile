include ../MkRules

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
	$(RM) *.o *.a *~ \#*\#

#dependencies
arith.o: graphapp.h
array.o: internal.h ga.h graphapp.h
bitmap.o: internal.h ga.h graphapp.h
buttons.o: internal.h ga.h graphapp.h
clipboard.o: internal.h ga.h graphapp.h ../rui.h
context.o: internal.h ga.h graphapp.h
controls.o: internal.h ga.h graphapp.h
drawing.o: internal.h ga.h graphapp.h
drawtext.o: internal.h ga.h graphapp.h
events.o: internal.h ga.h graphapp.h
fonts.o: internal.h ga.h graphapp.h
gbuttons.o: internal.h ga.h graphapp.h
gdraw.o: internal.h ga.h graphapp.h
gimage.o: ga.h graphapp.h
gmenus.o: internal.h ga.h graphapp.h
graphappmain.o: internal.h ga.h graphapp.h
image.o: internal.h ga.h graphapp.h
init.o: internal.h ga.h graphapp.h
menus.o: internal.h ga.h graphapp.h
metafile.o: internal.h ga.h graphapp.h ../rui.h
objects.o: internal.h ga.h graphapp.h
printer.o: internal.h ga.h graphapp.h ../rui.h
rgb.o: ga.h graphapp.h
status.o: internal.h ga.h graphapp.h
stdimg.o: graphapp.h
strings.o: internal.h ga.h graphapp.h
tooltips.o: internal.h ga.h graphapp.h
windows.o: internal.h ga.h graphapp.h
xredef.o: internal.h ga.h graphapp.h
