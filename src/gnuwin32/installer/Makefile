# Makefile for R for Windows Installers

include ../MkRules
UNZIPDIR=unzip
include ../RWVERSION

.PHONY: GA UZ

all: SetupR.exe miniR.exe
old: R.ico GA UZ rwinst.exe

# O1 for small rather than fast
CFLAGS=-I../graphapp -I../../include -I$(UNZIPDIR)/windll -Wall -O1 -DRVER=\"$(RPREFIX)\"

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

RECOMMENDED=KernSmooth VR boot cluster foreign mgcv nlme rpart survival
IS: SetupR.exe
# location where Inno Setup was installed
ISDIR=C:/packages/Inno
# location where binary packages are kept
PKGS=C:/R/Rdist/libs1030

R.iss: JRins.pl ../$(RPREFIX)b1.zip ../$(RPREFIX)b2.zip ../$(RPREFIX)h.zip \
../$(RPREFIX)ch.zip ../$(RPREFIX)d1.zip ../$(RPREFIX)d2.zip \
../$(RPREFIX)l.zip ../$(RPREFIX)sp.zip ../$(RPREFIX)w.zip
	unzip -qo ../$(RPREFIX)b1
	unzip -qo ../$(RPREFIX)b2
	unzip -qo ../$(RPREFIX)h
	unzip -qo ../$(RPREFIX)ch -d $(RPREFIX)ch
	unzip -qo ../$(RPREFIX)d1 -d $(RPREFIX)d1
	unzip -qo ../$(RPREFIX)d2 -d $(RPREFIX)d2
	unzip -qo ../$(RPREFIX)l -d $(RPREFIX)l
	unzip -qo ../$(RPREFIX)sp -d $(RPREFIX)sp
	unzip -qo ../$(RPREFIX)w -d $(RPREFIX)w
	for p in $(RECOMMENDED); do \
	  unzip -qo $(PKGS)/$${p} -d $(RPREFIX)/library; \
	done
	zip -rmq foo.zip $(RPREFIX)/library/*/html
	unzip -qo foo -d $(RPREFIX)w
	rm foo.zip
	zip -rmq foo.zip $(RPREFIX)/library/*/chtml
	unzip -qo foo -d $(RPREFIX)ch
	rm foo.zip
	zip -rmq foo.zip $(RPREFIX)/library/*/latex
	unzip -qo foo -d $(RPREFIX)l
	rm foo.zip
	$(CAT) $(RPREFIX)/library/*/CONTENTS >  \
	  $(RPREFIX)w/$(RPREFIX)/doc/html/search/index.txt
	$(CP) $(RPREFIX)w/$(RPREFIX)/doc/html/packages-head.html $(RPREFIX)/doc/html
	echo "make.packages.html(.Library)" | $(RPREFIX)/bin/rterm --vanilla --slave
	$(CP) $(RPREFIX)/doc/html/packages.html $(RPREFIX)w/$(RPREFIX)/doc/html
	$(RM) $(RPREFIX)/doc/html/packages*
	perl JRins.pl $(RPREFIX)


SetupR.exe: R.iss
	"$(ISDIR)/iscc" R.iss > setupR.log

Rsmall.iss: JRsmall.pl ../$(RPREFIX)b1.zip ../$(RPREFIX)b2.zip \
  ../$(RPREFIX)h.zip ../$(RPREFIX)ch.zip ../$(RPREFIX)d1.zip
	rm -rf $(RPREFIX)*
	unzip -qo ../$(RPREFIX)b1
	unzip -qo ../$(RPREFIX)b2
	unzip -qo ../$(RPREFIX)h
	unzip -qo ../$(RPREFIX)ch -d $(RPREFIX)ch
	unzip -qo ../$(RPREFIX)d1 -d $(RPREFIX)d1
	rm $(RPREFIX)d1/$(RPREFIX)/doc/manual/R-admin.pdf
	rm $(RPREFIX)d1/$(RPREFIX)/doc/manual/R-exts.pdf
	rm $(RPREFIX)d1/$(RPREFIX)/doc/manual/R-lang.pdf
	for p in $(RECOMMENDED); do \
	  unzip -qo $(PKGS)/$${p} -d $(RPREFIX)/library; \
	done
	rm -rf $(RPREFIX)/library/*/html $(RPREFIX)/library/*/latex
	rm -rf $(RPREFIX)/library/rpart/Manuscript
	rm -rf $(RPREFIX)/library/survival/doc
	perl JRsmall.pl $(RPREFIX)

miniR.exe: Rsmall.iss
	"$(ISDIR)/iscc" Rsmall.iss > miniR.log

ISclean:
	rm -rf R.iss Rsmall.iss *.log $(RPREFIX)*

ISdistclean: ISclean
	rm -f SetupR.exe miniR*
