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


## lm.influence where hat[1] == 1
if(require(MASS)) {
    fit <- lm(formula = 1000/MPG.city ~ Weight + Cylinders + Type + EngineSize + DriveTrain, data = Cars93)
    print(lm.influence(fit))
    ## row 57 should have hat = 1 and resid=0.
    summary(influence.measures(fit))
}
## only last two cols in row 57 should be influential
