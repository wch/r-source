base:
	@echo Building R base library
	@mkdir -p ../../../library/base/R
	@cat `ls R/[a-z]* | sed '/CVS/d; /~$$/d; /\.orig$$/d; \
		/\.bak$$/d; /system.*/d'` R/system.unix | \
	 sed '/^[ 	]*$$/d'  > ../../../library/base/R/base
#		   ^^ = TAB + Space
##-- do NOT delete all '#' lines -- this BREAKS strings containing '#' !!
##-- Could still delete all  '/^[\t ]*#/ lines, but why not leave comments ?
##	@cat $(FUNS) STARTUP |sed  '/^$$/d;/^[ 	]*#/d'  > ../../../library/base
##					      ^^ = TAB + Space
	@cp INDEX TITLE ../../../library/base
