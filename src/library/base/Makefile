base:
	@echo Building R base library
	@if test ! -d ../../../library/base; then mkdir ../../../library/base; fi
	@if test ! -d ../../../library/base/R; then mkdir ../../../library/base/R; fi
	@if test ! -d ../../../library/base/data; then mkdir ../../../library/base/data; fi
	@cat `ls R/[a-z]* | sed '/CVS/d; /~$$/d; /\.orig$$/d; \
		/\.bak$$/d; /^core$$/d; /system.*/d'` R/system.unix | \
	 sed '/^[ 	]*$$/d'  > ../../../library/base/R/base
#		   ^^ = TAB + Space
##-- do NOT delete all '#' lines -- this BREAKS strings containing '#' !!
##-- Could still delete all  '/^[\t ]*#/ lines, but why not leave comments ?
##	@cat $(FUNS) STARTUP |sed  '/^$$/d;/^[ 	]*#/d'  > ../../../library/base
##					      ^^ = TAB + Space
	@cp INDEX TITLE ../../../library/base
	@cp `ls data/[a-z]*` ../../../library/base/data
