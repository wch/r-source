## tests of nls, especially of weighted fits

.proctime00 <- proc.time()
library(stats)
options(digits=5) # to avoid trivial printed differences

postscript("nls-test.ps")

## selfStart.default() w/ no parameters:
logist <- deriv( ~Asym/(1+exp(-(x-xmid)/scal)), c("Asym", "xmid", "scal"),
		function(x, Asym, xmid, scal){} )
logistInit <- function(mCall, LHS, data) {
    xy <- sortedXyData(mCall[["x"]], LHS, data)
    if(nrow(xy) < 3) stop("Too few distinct input values to fit a logistic")
    Asym <- max(abs(xy[,"y"]))
    if (Asym != max(xy[,"y"])) Asym <- -Asym  # negative asymptote
    xmid <- NLSstClosestX(xy, 0.5 * Asym)
    scal <- NLSstClosestX(xy, 0.75 * Asym) - xmid
    value <- c(Asym, xmid, scal)
    names(value) <- mCall[c("Asym", "xmid", "scal")]
    value
}
logist <- selfStart(logist, initial = logistInit) ##-> Error in R 1.5.0
str(logist)

## lower and upper in algorithm="port"
set.seed(123)
x <- runif(200)
a <- b <- 1; c <- -0.1
y <- a+b*x+c*x^2+rnorm(200, sd=0.05)
plot(x,y)
curve(a+b*x+c*x^2, add = TRUE)
nls(y ~ a+b*x+c*I(x^2), start = c(a=1, b=1, c=0.1), algorithm = "port")
(fm <- nls(y ~ a+b*x+c*I(x^2), start = c(a=1, b=1, c=0.1),
           algorithm = "port", lower = c(0, 0, 0)))
confint(fm)


## weighted nls fit: unsupported < 2.3.0
set.seed(123)
y <- x <- 1:10
yeps <- y + rnorm(length(y), sd = 0.01)
wts <- rep(c(1, 2), length = 10); wts[5] <- 0
fit0 <- lm(yeps ~ x, weights = wts)
summary(fit0, cor = TRUE)
cf0 <- coef(summary(fit0))[, 1:2]
fit <- nls(yeps ~ a + b*x, start = list(a = 0.12345, b = 0.54321),
           weights = wts, trace = TRUE)
summary(fit, cor = TRUE)
stopifnot(all.equal(residuals(fit), residuals(fit0), 1e5))
stopifnot(df.residual(fit) == df.residual(fit0))
cf1 <- coef(summary(fit))[, 1:2]
fit2 <- nls(yeps ~ a + b*x, start = list(a = 0.12345, b = 0.54321),
            weights = wts, trace = TRUE, algorithm = "port")
summary(fit2, cor = TRUE)
cf2 <- coef(summary(fit2))[, 1:2]
rownames(cf0) <- c("a", "b")
# expect relative errors ca 2e-08
stopifnot(all.equal(cf1, cf0, 1e-6),  all.equal(cf1, cf0, 1e-6))
stopifnot(all.equal(residuals(fit2), residuals(fit0), 1e5))


DNase1 <- subset(DNase, Run == 1)
DNase1$wts <- rep(8:1, each = 2)
fm1 <- nls(density ~ SSlogis(log(conc), Asym, xmid, scal),
           data = DNase1, weights = wts)
summary(fm1)

## directly
fm2 <- nls(density ~ Asym/(1 + exp((xmid - log(conc))/scal)),
           data = DNase1, weights = wts,
           start = list(Asym = 3, xmid = 0, scal = 1))
summary(fm2)
stopifnot(all.equal(coef(summary(fm2)), coef(summary(fm1)), tol = 1e-6))
stopifnot(all.equal(residuals(fm2), residuals(fm1), tol = 1e-5))
stopifnot(all.equal(fitted(fm2), fitted(fm1), tol = 1e-6))
fm2a <- nls(density ~ Asym/(1 + exp((xmid - log(conc)))),
            data = DNase1, weights = wts,
            start = list(Asym = 3, xmid = 0))
anova(fm2a, fm2)

## and without using weights
fm3 <- nls(~ sqrt(wts) * (density - Asym/(1 + exp((xmid - log(conc))/scal))),
           data = DNase1, start = list(Asym = 3, xmid = 0, scal = 1))
summary(fm3)
stopifnot(all.equal(coef(summary(fm3)), coef(summary(fm1)), tol = 1e-6))
ft <- with(DNase1, density - fitted(fm3)/sqrt(wts))
stopifnot(all.equal(ft, fitted(fm1), tol = 1e-6))
# sign of residuals is reversed
r <- with(DNase1, -residuals(fm3)/sqrt(wts))
all.equal(r, residuals(fm1), tol = 1e05)
fm3a <- nls(~ sqrt(wts) * (density - Asym/(1 + exp((xmid - log(conc))))),
            data = DNase1, start = list(Asym = 3, xmid = 0))
anova(fm3a, fm3)

## using conditional linearity
fm4 <- nls(density ~ 1/(1 + exp((xmid - log(conc))/scal)),
           data = DNase1, weights = wts,
           start = list(xmid = 0, scal = 1), algorithm = "plinear")
summary(fm4)
cf <- coef(summary(fm4))[c(3,1,2), ]
rownames(cf)[2] <- "Asym"
stopifnot(all.equal(cf, coef(summary(fm1)), tol = 1e-6))
stopifnot(all.equal(residuals(fm4), residuals(fm1), tol = 1e-5))
stopifnot(all.equal(fitted(fm4), fitted(fm1), tol = 1e-6))
fm4a <- nls(density ~ 1/(1 + exp((xmid - log(conc)))),
            data = DNase1, weights = wts,
            start = list(xmid = 0), algorithm = "plinear")
anova(fm4a, fm4)

## using 'port'
fm5 <- nls(density ~ Asym/(1 + exp((xmid - log(conc))/scal)),
           data = DNase1, weights = wts,
           start = list(Asym = 3, xmid = 0, scal = 1),
           algorithm = "port")
summary(fm5)
stopifnot(all.equal(coef(summary(fm5)), coef(summary(fm1)), tol = 1e-6))
stopifnot(all.equal(residuals(fm5), residuals(fm1), tol = 1e-5))
stopifnot(all.equal(fitted(fm5), fitted(fm1), tol = 1e-6))

## check profiling
pfm1 <- profile(fm1)
pfm3 <- profile(fm3)
for(m in names(pfm1)) stopifnot(all.equal(pfm1[[m]], pfm3[[m]], tol=1e-5))
pfm5 <- profile(fm5)
for(m in names(pfm1)) stopifnot(all.equal(pfm1[[m]], pfm5[[m]], tol=1e-5))
(c1 <- confint(fm1))
(c4 <- confint(fm4, 1:2))
stopifnot(all.equal(c1[2:3, ], c4, tol = 1e-3))


cat('Time elapsed: ', proc.time() - .proctime00,'\n')
