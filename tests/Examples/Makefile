###-*- Makefile -*- for 'test-Examples', also see ${RHOME}/tests/Makefile

CleanMe = .RData sink-examp.txt
## The following must be an ABSOLUTE path
## or RELATIVE to 'Ex_Tests' (RHOME/test/Examples/ :
rRHOME= ../..
#
RETC	= $(rRHOME)/etc
R       = $(rRHOME)/bin/R --no-init-file --no-save --no-restore
Rbinary = $(rRHOME)/bin/R.binary
Rbase	= $(rRHOME)/library/base

MANSRC = $(rRHOME)/src/library/*/man/*.Rd
MANbaseDIR = $(rRHOME)/src/library/base/man

##- Uncomment one of the 2 'Rd_files' defs -- depending on your 'make' version
##--- <<<<<< CONFIGURE should do this >>>>>>
##-- Sun make
#> Rd_files:sh = ../../etc/Rd.files
##-- GNU make
Rd_files := $(shell $(rRHOME)/etc/Rd.files)
#this has PATH prepended: Rd_files := $(wildcard $(MANbaseDIR)/*.Rd)
#________in future_______  (for ALL 'make' versions)
# FAILS: has directory/. _AND_ is not expanded
#Rd_files = $(MANbaseDIR)/*.Rd
#
R_src = $(Rd_files:%.Rd=%.R)
#________in future_______: (FAILS!)
#R_src = $(Rd_files:$(MANbaseDIR)/%.Rd=%.R)


##--------------------------------- TARGETS ------------------------------
test-Examples: All-Ex.Rout

##-- The 'show-..' are just for testing purposes:
show-R: ; @-(cd ../../include; echo "R = '$(R)'"
show-RHOME: ; @-(RHOME=`cd ${rRHOME}; pwd`;export RHOME; printenv RHOME)
show-Rd:
	@-echo 'R help() files = "Rd files" :'
	@-echo $(Rd_files)
show-Ex:
	@-echo 'R_src [targets of  help() files] :'
	@-echo $(R_src)


%.R: $(MANbaseDIR)/%.Rd
	@RHOME=$(rRHOME) $(RETC)/Rdconv -t example $< > $@
	@-echo $@

All-Ex.R: $(R_src)
	@if [ -f $@.bak ]; then mv $@.bak $@.bakk ; fi
	@if [ -f $@ ]; then mv $@ $@.bak ; fi
	@echo "massaging single Examples into $@ ..."
	@-./massage-Examples $(R_src) > $@

All-Ex.Rout: All-Ex.R $(Rbinary) $(Rbase)
	@if [ -f $@.bak ]; then mv $@.bak $@.bakk ; fi
	@if [ -f $@ ]; then mv $@ $@.bak ; fi
	@echo 'Running all help() EXAMPLES ..'
	$(R) -v 6 < All-Ex.R > $@

clean:
	@rm -f *.Rd* *.ps *.tex *.dat* data foo*

realclean: clean
	@rm -f *.R
	@rm -fr $(CleanMe)
