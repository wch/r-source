#### Produce an R test script
ls.base <- ls(pos=length(search()))#- something more elegant ?
base.is.f <- sapply(ls.base, function(x) is.function(get(x)))
bi <- ls.base[base.is.f]
iroot <- substring(is.bi <- bi[substring(bi,1,3) == "is."],4)
aroot <- substring(as.bi <- bi[substring(bi,1,3) == "as."],4)
(root <- intersect(iroot,aroot))# both an  is.foo and as.foo function exist

ex.list <- expression(integer(0), NULL, list(), 1:1, pi, "1.3", list(a=1),
    as.data.frame(character(0)))

##--- producing the real R script:
sink("isas-tests.R")

cat("isall.equal <- function(x,y)",
    "typeof(x) == typeof(y) && is.logical(r <- all.equal(x,y, tol=0)) && r \n",
    sep="\n")

cat("options(error = expression(NULL))",
    "# don't stop on error in batch\n##~~~~~~~~~~~~~~\n")
for(x in ex.list) {
    cat("\n###--------\n x <- ", deparse(x), "\n", sep="")
    ## is.foo(as.foo( bar )) #>> TRUE :
    for(r in root) cat("is.",r,"(as.",r,"( x ))\n", sep="")
    cat("\n")
    ## if(is.foo(bar))  bar ``=='' as.foo(bar) :
    for(r in root)
        cat("if(is.",r,"(x)) { ",
            "cat('IS: ');all.equal(x, as.",r,"( x ), tol=0)\n",
            "} else   !isall.equal(x, as.",r,"( x ))\n", sep="")

    ## f <- as.foo(x)  ==>  as.foo(f) == f :
    for(r in aroot)
        cat("f <- as.",r,"( x );  all.equal(f, as.",r,"( f ), tol=0)\n", sep="")
}



