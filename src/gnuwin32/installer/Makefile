# Makefile for R for Windows Installers

include ../MkRules
include ../RWVERSION

all: SetupR.exe miniR.exe
clean: ISclean
distclean: ISdistclean

#======================================================================

# section for JR software installer.

RECOMMENDED=KernSmooth VR boot cluster foreign mgcv nlme rpart survival
EXTRA=grid lattice

IS: SetupR.exe
# location where Inno Setup was installed
ISDIR=C:/packages/Inno
# location where binary packages are kept
PKGS=C:/R/Rdist/libs

R.iss: JRins.pl ../$(RPREFIX)b.zip ../$(RPREFIX)h.zip \
../$(RPREFIX)ch.zip ../$(RPREFIX)d1.zip ../$(RPREFIX)d2.zip \
../$(RPREFIX)l.zip ../$(RPREFIX)sp.zip ../$(RPREFIX)w.zip
	unzip -qo ../$(RPREFIX)b
	unzip -qo ../$(RPREFIX)h
	unzip -qo ../$(RPREFIX)ch -d $(RPREFIX)ch
	unzip -qo ../$(RPREFIX)d1 -d $(RPREFIX)d1
	unzip -qo ../$(RPREFIX)d2 -d $(RPREFIX)d2
	unzip -qo ../$(RPREFIX)l -d $(RPREFIX)l
	unzip -qo ../$(RPREFIX)sp -d $(RPREFIX)sp
	unzip -qo ../$(RPREFIX)w -d $(RPREFIX)w
	for p in $(RECOMMENDED) $(EXTRA); do \
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
	rm -rf $(RPREFIX)/library/*/man
	$(CAT) $(RPREFIX)/library/*/CONTENTS >  \
	  $(RPREFIX)w/$(RPREFIX)/doc/html/search/index.txt
	$(CP) $(RPREFIX)w/$(RPREFIX)/doc/html/packages-head.html $(RPREFIX)/doc/html
	echo "make.packages.html(.Library)" | $(RPREFIX)/bin/rterm --vanilla --slave
	$(CP) $(RPREFIX)/doc/html/packages.html $(RPREFIX)w/$(RPREFIX)/doc/html
	$(RM) $(RPREFIX)/doc/html/packages*
	perl JRins.pl $(RPREFIX)


SetupR.exe: R.iss
	"$(ISDIR)/iscc" R.iss > setupR.log

Rsmall.iss: JRsmall.pl ../$(RPREFIX)b.zip \
  ../$(RPREFIX)h.zip ../$(RPREFIX)ch.zip ../$(RPREFIX)d1.zip
	rm -rf $(RPREFIX)*
	unzip -qo ../$(RPREFIX)b
	unzip -qo ../$(RPREFIX)h
	unzip -qo ../$(RPREFIX)ch -d $(RPREFIX)ch
	unzip -qo ../$(RPREFIX)d1 -d $(RPREFIX)d1
	rm $(RPREFIX)d1/$(RPREFIX)/doc/manual/R-admin.pdf
	rm $(RPREFIX)d1/$(RPREFIX)/doc/manual/R-exts.pdf
	rm $(RPREFIX)d1/$(RPREFIX)/doc/manual/R-lang.pdf
	for p in $(RECOMMENDED); do \
	  unzip -qo $(PKGS)/$${p} -d $(RPREFIX)/library; \
	done
	rm -rf $(RPREFIX)/library/*/html $(RPREFIX)/library/*/latex \
	  $(RPREFIX)/library/*/man
	rm -rf $(RPREFIX)/library/rpart/Manuscript
	rm -rf $(RPREFIX)/library/survival/doc $(RPREFIX)/library/survival/survival.ps.gz
	perl JRsmall.pl $(RPREFIX)

miniR.exe: Rsmall.iss
	"$(ISDIR)/iscc" Rsmall.iss > miniR.log

ISclean:
	rm -rf R.iss Rsmall.iss *.log $(RPREFIX)*

ISdistclean: ISclean
	rm -f SetupR.exe miniR*
