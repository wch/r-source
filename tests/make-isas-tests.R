#### Produce an R test script
ls.base <- ls(pos=length(search()))#- something more elegant ?
base.is.f <- sapply(ls.base, function(x) is.function(get(x)))
bi <- ls.base[base.is.f]
iroot <- substring(is.bi <- bi[substring(bi,1,3) == "is."],4)
aroot <- substring(as.bi <- bi[substring(bi,1,3) == "as."],4)
(root <- intersect(iroot,aroot))# both an  is.foo and as.foo function exist

##--- producing the real R script:
sink("isas-tests.R")

cat("options(error = expression(NULL))",
    "# don't stop on error in batch\n##~~~~~~~~~~~~~~\n")
for(x in expression(1, NULL, pi, "1.3", list(), list(a=1))) {
    cat("\n###--------\n x <- ", deparse(x), "\n", sep="")
    ## is.foo(as.foo( bar )) #>> TRUE
    for(r in root) cat("is.",r,"(as.",r,"( x ))\n", sep="")
    cat("\n")
    ## if(is.foo(bar))  bar ``=='' as.foo(bar)
    for(r in root)
        cat("if(is.",r,"( x ))  all.equal(x, as.",r,"( x ))\n", sep="")
}



