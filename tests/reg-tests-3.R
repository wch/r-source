### Regression tests for which the printed output is the issue
### May fail, e.g. by needing Recommended packages

# postscript("reg-tests-3.ps")

## str() for character & factors with NA (levels), and for Surv objects:
ff <- factor(c(2:1,  NA),  exclude = NULL)
str(levels(ff))
str(ff)
str(ordered(ff, exclude=NULL))
if(require(survival)) {
    data(aml)
    (sa <- Surv(aml$time, aml$status))
    str(sa)
    detach("package:survival")
}
## were different, the last one failed in 1.6.2 (at least)
