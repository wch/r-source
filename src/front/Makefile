install: R.sh
	@echo Creating front-end shell script
	@cat R.sh | (cd ../.. ; sed "s@RHOME=.*@RHOME=`pwd`@" > bin/R)
	@chmod +x ../../bin/R

realclean:
	@echo Cleaning in src/front
	@rm -f R.sh
