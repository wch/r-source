include ../MkRules

CFLAGS=-DWin32 -I../../include
LINKFLAGS=-s

ifdef DEBUG
 CFLAGS+=-g
endif

gl.a: getline.o

test: testgl.exe
	$(ECHO) starting testgl. Hit CRTL+BREAK to stop it.
	testgl.exe

testgl.exe: testgl.o getline.o	


clean:
	$(RM) *.o *~ *.exe *.exp gl.a
