# Makefile for R for Windows Installer

include ../MkRules
UNZIPDIR=unzip
PREFIX=rw1023

.PHONY: GA UZ

all: R.ico GA UZ rwinst.exe

# O1 for small rather than fast
CFLAGS=-I../graphapp -I../../include -I$(UNZIPDIR)/windll -Wall -O1 -DRVER=\"$(PREFIX)\"

GA:
	$(MAKE) -C ../graphapp

UZ:
	$(MAKE) -C $(UNZIPDIR) -f ../Makefile.unzip

R.ico: ../front-ends/R.ico
	$(CP) -p ../front-ends/R.ico .

rwico.o: rwico.rc

rwinst.o: ../../include/Rversion.h ../graphapp/ga.h ../graphapp/graphapp.h

rwinst.exe: rwinst.o rwico.o dounzip.o ../ga.a $(UNZIPDIR)/unzip32.a
	$(CC) -s -mwindows -o $@ $^ -lcomctl32

clean:
	$(RM) R.ico *.o *~ \#*\# rwinst.exe
	-$(MAKE) -C $(UNZIPDIR) -f ../Makefile.unzip clean

#======================================================================

# section for JR software installer.

IS: SetupR.exe
# location where Inno Setup was installed
ISDIR=C:/packages/Inno

R.iss: JRins.pl ../$(PREFIX)b1.zip ../$(PREFIX)b2.zip ../$(PREFIX)h.zip \
../$(PREFIX)ch.zip ../$(PREFIX)d1.zip ../$(PREFIX)d2.zip \
../$(PREFIX)l.zip ../$(PREFIX)sp.zip ../$(PREFIX)w.zip
	unzip -qo ../$(PREFIX)b1
	unzip -qo ../$(PREFIX)b2
	unzip -qo ../$(PREFIX)h
	unzip -qo ../$(PREFIX)ch -d $(PREFIX)ch
	unzip -qo ../$(PREFIX)d1 -d $(PREFIX)d1
	unzip -qo ../$(PREFIX)d2 -d $(PREFIX)d2
	unzip -qo ../$(PREFIX)l -d $(PREFIX)l
	unzip -qo ../$(PREFIX)sp -d $(PREFIX)sp
	unzip -qo ../$(PREFIX)w -d $(PREFIX)w
	perl JRins.pl $(PREFIX)

SetupR.exe: R.iss
	"$(ISDIR)/compil32" /cc R.iss

Rsmall.iss: JRsmall.pl ../$(PREFIX)b1.zip ../$(PREFIX)b2.zip \
  ../$(PREFIX)h.zip ../$(PREFIX)ch.zip ../$(PREFIX)d1.zip
	unzip -qo ../$(PREFIX)b1
	unzip -qo ../$(PREFIX)b2
	unzip -qo ../$(PREFIX)h
	unzip -qo ../$(PREFIX)ch -d $(PREFIX)ch
	unzip -qo ../$(PREFIX)d1 -d $(PREFIX)d1
	rm $(PREFIX)d1/$(PREFIX)/doc/manual/R-exts.pdf
	rm $(PREFIX)d1/$(PREFIX)/doc/manual/R-lang.pdf
	mkdir -p $(PREFIX)/doc/html
	perl JRsmall.pl $(PREFIX)

miniR.exe: Rsmall.iss
	"$(ISDIR)/compil32" /cc Rsmall.iss

ISclean:
	rm -rf R.iss Rsmall.iss $(PREFIX)*

ISdistclean: ISclean
	rm -f SetupR.exe miniR*
