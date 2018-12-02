include ../MkRules

R_HOME = ../../..

RPREFIX = $(shell ../../../bin$(R_ARCH)/Rscript ../fixed/rwver.R)
FULLVERSION=`cat ../../../VERSION`
SVNREVISION=`sed -e 1s/Revision:.// -e q < $(R_HOME)/SVN-REVISION`
RVER = $(shell cut -d' ' -f1 ../../../VERSION | sed -n 1p)
DATE = $(shell date +%F)
SIZEMB = $(shell du -m ../installer/$(RPREFIX)-win.exe | cut -f1)

R_EXE = $(R_HOME)/$(BINDIR)/rterm.exe --vanilla --slave

all:
	$(CP) ../../../doc/html/CHANGES.html CHANGES.$(RPREFIX).html
	$(CP) ../../../doc/html/NEWS.html NEWS.$(RPREFIX).html
	$(CP) ../installer/$(RPREFIX)-win*.exe .
	($(ECHO) 'cat(md5sum("$(RPREFIX)-win.exe"), "*$(RPREFIX)-win.exe")' \
         | $(R_EXE) R_DEFAULT_PACKAGES=tools > md5sum.txt)
	$(CP) ../../../doc/html/rw-FAQ.html .	
	@for page in index rdevel rpatched rtest release; do \
	  $(SED) -e s/@RWVER@/$(RPREFIX)/g \
	    -e s/@RVER@/$(RVER)/g \
	    -e "s/@DATE@/$(DATE)/g" \
	    -e "s/@SIZEMB@/$(SIZEMB)/g" \
	    -e "s/@SVNREVISION@/$(SVNREVISION)/g" \
	    -e "s/@FULLVERSION@/$(FULLVERSION)/g" $${page}.in >$${page}.html; \
	done
	$(CP) release.html release.htm
	$(SED) -e s/@RWVER@/$(RPREFIX)/g \
	  -e s/@RVER@/$(RVER)/g \
	  -e "s/@FULLVERSION@/$(FULLVERSION)/g" ../README > README.$(RPREFIX)
	$(SED) -e s/@RWVER@/$(RPREFIX)/g \
	  -e s/@RVER@/$(RVER)/g \
	  -e "s/@FULLVERSION@/$(FULLVERSION)/g" ReadMe.in > ReadMe	  
	@(zip -q dosfiles.zip CHANGES.$(RPREFIX) NEWS.$(RPREFIX) README.$(RPREFIX) ReadMe; \
	 unzip -aqo dosfiles.zip; \
	 $(RM) dosfiles.zip)
	$(ECHO) set target=$(RPREFIX) >target.cmd


clean:
	$(RM) -R *.html *.htm ReadMe README* *.exe md5sum.txt NEWS.* CHANGES.* target.cmd
