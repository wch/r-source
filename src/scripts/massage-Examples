#!/bin/sh
## R_HOME/bin/massage-Examples
## Given a list of files of the form .../.../<name>.R, produce one large
## file, i.e. write to stdout, `cat'ting the files together with
## 1) Putting a HEADER in front
## 2) Wrapping every file in order to be more order independent
## 3) appending a FOOTER ...

PKG=${1}; shift; FILES="${@}"

## 1) ---- Header ----
(cat <<_EOF_
attach(NULL, name = "CheckExEnv")
assign(".CheckExEnv", pos.to.env(2), pos = length(search())) # base
## This plot.new() patch has not yet an effect for
## persp();   layout() & filled.contour() are neither ok
assign("plot.new", function() { .Internal(plot.new())
		       pp <- par(c("mfg","mfcol","oma","mar"))
		       if(all(pp\$mfg[1:2] == c(1, pp\$mfcol[2]))) {
		         outer <- (oma4 <- pp\$oma[4]) > 0; mar4 <- pp\$mar[4]
			 mtext(paste("help(",..nameEx,")"), side = 4,
			       line = if(outer)max(1, oma4 - 1) else min(1, mar4 - 1),
			       outer = outer, adj=1, cex= .8, col="orchid")} },
       env = .CheckExEnv)
assign("..nameEx", "__{must remake R-ex/*.R}__", env = .CheckExEnv) #-- for now
assign("ptime", proc.time(), env = .CheckExEnv)
postscript("PKG-Examples.ps")
assign("par.postscript", par(no.readonly = TRUE), env = .CheckExEnv)
options(contrasts = c(unordered = "contr.treatment", ordered = "contr.poly"))
_EOF_
) | sed "s/PKG/${PKG}/"
if [ "${PKG}" = "tcltk" ]; then 
  echo "require('tcltk') || q()"
else
  if [ "${PKG}" != "base" ]; then echo "library('${PKG}')" ; fi
fi

## 2) ---- edit a few of these files:
for file in ${FILES}
do
  bf=`basename $file .R`
  if test -n "`grep '_ Examples _' $file`"; then
    echo "rm(list = ls(all = TRUE)); .Random.seed <- c(0,rep(7654,3))"
    echo '..nameEx <- "'"`echo $bf | sed -e 's/[^- .a-zA-Z0-9]/./g'`"'"'
  fi

  cat ${file}

  if test -n "`grep 'par(' ${file}`"; then
    ## if there were 'par(..)' calls, now reset them:
    echo 'par(get("par.postscript", env = .CheckExEnv))'
  fi
  if test -n "`grep 'options(contrasts' ${file}`"; then
    ## if contrasts were set, now reset them:
    echo 'options(contrasts = c(unordered = "contr.treatment", ordered = "contr.poly"))'
  fi
done

## 3) ---- Footer ----
cat <<_EOF_
cat("Time elapsed: ", proc.time() - get("ptime", env = .CheckExEnv),"\n")
dev.off(); quit('no')
_EOF_

### Local Variables: ***
### mode: sh ***
### sh-indentation: 2 ***
### End: ***
