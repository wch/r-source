##-- Makefile in   manual/latex  ; fixed to be okay for 0.2
##-- 		   ~~~~~~~~~~~~    Now, a second "make" doesn't have to redo...
DVIdir = ../../dvi

Man.dvi: doc.tex Man.tex
	@echo making latex documentation.
	-@rm -f Man.dvi
	latex Man
	mkdir -p ${DVIdir}
	cp -p Man.tex doc.tex  ${DVIdir}
	mv Man.dvi ${DVIdir}
	ln -s  ${DVIdir}/Man.dvi .
	@echo 'latex documents placed in  ${DVIdir}'

sources: doc.tex
doc.tex: doc2latex
	@echo building latex source files
	mkdir -p src; cd src; rm -r -f *
	cd src; ln -s ../../man/* .
	cd src ; ../doc2latex `ls | sort -f| sed '/CVS/d; /~/d' ` > ../doc.tex

print:	Man.dvi
	dvips -f Man.dvi | lpr

print2:	Man.dvi
	dvips -f Man.dvi | mpage -P -2  > Man.ps
	lpr -s Man.ps

clean:
	@echo cleaning in manual/latex
	@-rm -f *.aux *.dvi *.log doc.tex *.ps
	@-cd src; rm -r -f *

