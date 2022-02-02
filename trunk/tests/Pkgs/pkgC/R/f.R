setGeneric("unique")

nil <- NULL # see if we can use '::' on it

Cfun <- function(x) {
    new("classA", # classA was imported from 'pkgA'
       cbind(x, b=x+2))
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

