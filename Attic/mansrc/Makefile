dummy:
	@echo you must make something

html:
	@echo making the html manual ...
	@cd html; make

help:
	@echo making help files
	@cd help; make
	@echo help files installed

latex:
	@echo making latex manual
	@cd latex; make
	@echo TeX files are in directory dvi

clean:
	@echo cleaning in manual
	@-rm -f *.aux *.log *.dvi
	@cd help; make clean
	@cd latex; make clean
	@cd html; make clean

realclean:
	@echo cleaning in manual
	@-rm -f *.aux *.log *.dvi
	@cd help; make clean
	@cd latex; make clean
	@cd html; make clean
	@cd undoc; make clean
