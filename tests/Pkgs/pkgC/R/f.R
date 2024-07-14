setGeneric("unique")

nil <- NULL # see if we can use '::' on it

Cfun <- function(x) {
    new("classA", # classA was imported from 'pkgA'
       cbind(x, b=x+2))
}


## S3 generic and method -- both *not* exported; -- methods(pkgC:::foobar)  should still work:
foobar <- function(x) { UseMethod("foobar") }
foobar.default <- function(x) { cat("foobar.default(x):, x="); str(x) }
foobar.Date <- function(x) {
    r <- x + 1
    cat("foobar.Date(.): next day is: ", format(r), "\n")
    invisible(r)
}

## Pkg 'ecd' had
## setClassUnion("numericMpfr", c("numeric", "mpfr", "mpfrArray"))
##
## which gave an 'R CMD INSTALL'  warning with Rmpfr 0.7-2
'
** byte-compile and prepare package for lazy loading
Warning: subclass "summaryMpfr" of class "mpfr" is not local and cannot be updated for new inheritance information; consider setClassUnion()
'
## 'Rmpfr' there corresponds to 'pkgA' here;
##  ----   class "mpfr"        to "classA"   here,
##         class "summaryMpfr" to "classApp" here .. :
setClassUnion("numericA", c("numeric", "classA", "matrix"))

