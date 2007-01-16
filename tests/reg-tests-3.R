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


## drop1 on weighted lm() fits
if(require(MASS)) {
    hills.lm <- lm(time ~ 0 + dist + climb, data=hills, weights=1/dist^2)
    print(drop1(hills.lm))
    print(stats:::drop1.default(hills.lm))
    hills.lm2 <- lm(time/dist ~ 1 + I(climb/dist), data=hills)
    drop1(hills.lm2)
}
## quoted unweighted RSS etc in 2.2.1


## tests of ISO C99 compliance (Windows fails without a workaround)
sprintf("%g", 123456789)
sprintf("%8g", 123456789)
sprintf("%9.7g", 123456789)
sprintf("%10.9g", 123456789)
sprintf("%g", 12345.6789)
sprintf("%10.9g", 12345.6789)
sprintf("%10.7g", 12345.6789)
sprintf("%.7g", 12345.6789)
sprintf("%.5g", 12345.6789)
sprintf("%.4g", 12345.6789)
sprintf("%9.4g", 12345.6789)
sprintf("%10.4g", 12345.6789)
## Windows used e+008 etc prior to 2.3.0


## weighted glm() fits
if(require(MASS)) {
    hills.glm <- glm(time ~ 0 + dist + climb, data=hills, weights=1/dist^2)
    print(AIC(hills.glm))
    print(extractAIC(hills.glm))
    print(drop1(hills.glm))
    stats:::drop1.default(hills.glm)
}
## wrong AIC() and drop1 prior to 2.3.0.

## calculating no of signif digits
print(1.001, digits=16)
## 2.4.1 gave  1.001000000000000
## 2.5.0 errs on the side of caution.
