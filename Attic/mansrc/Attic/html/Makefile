HTMLDIR= ../../html
HTMLfun= $(HTMLDIR)/funs

all:
	@echo Making html documentation
	-(mkdir -p $(HTMLfun); cd $(HTMLfun); rm -f `echo * | sed /CVS/d`)
	-(mkdir -p src; cd src; rm -f `ls | sed '/CVS/d'`)
	cd src; ln -s `ls -d ../../man/* | sed '/CVS/d; /~/d'` .
	html.format
	html.aliases
	html.index > $(HTMLfun)/index.html

clean:
	@echo cleaning in manual/html
	@-cd src; rm -f `ls | sed '/CVS/d'`
