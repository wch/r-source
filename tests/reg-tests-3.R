### Regression tests for which the printed output is the issue
### May fail, e.g. by needing Recommended packages

postscript("reg-tests-3.ps")

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


## PR#6640  Zero weights in plot.lm
if(require(MASS)) {
    fm1 <- lm(time~dist, data=hills, weights=c(0,0,rep(1,33)))
    plot(fm1)
}
## gave warnings in 1.8.1


## PR#7829 model.tables & replications
if(require(MASS)) {
oats.aov <- aov(Y ~ B + V + N + V:N, data=oats[-1,])
model.tables(oats.aov, "means", cterms=c("N", "V:N"))
}
## wrong printed output in 2.1.0
