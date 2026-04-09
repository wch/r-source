#  File src/library/stats/tests/ts-tests.R
#  Part of the R package, https://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  https://www.R-project.org/Licenses/

## tests of time-series functionality

.proctime00 <- proc.time()
library(stats)

pdf("ts-test.pdf")

### ar
ar(lh)
ar(lh, method = "burg")
ar(lh, method = "ols")
ar(lh, FALSE, 4) # fit ar(4)
ar.ols(lh)
ar.ols(lh, FALSE, 4) # fit ar(4) by OLS

ar(LakeHuron)
ar(LakeHuron, method = "burg")
ar(LakeHuron, method = "ols")
ar(LakeHuron, method = "mle")

ar(sunspot.year, method = "yw")
ar(sunspot.year, method = "burg")
ar(sunspot.year, method = "ols")
ar(sunspot.year, method = "mle")

x4d <- diff(log(EuStockMarkets))
x1 <- x4d[,1]
## aic=FALSE: just compute AR(<order.max>):
m6 <- ar.ols(x1, order.max = 6, demean = FALSE, aic = FALSE)
stopifnot(all.equal(array(c(4.167, -22.03, -7.737,
                            4.181, -27.62, 6.851), dim = c(6L, 1L, 1L)),
                    m6$ar*1000, tolerance = 1e-3))
lm6 <- lapply(setNames(,c("yw","burg","mle")), \(meth)
              ar(x1, aic=FALSE, order.max = 6, method = meth, demean=FALSE))
(mar <- vapply(lm6, `[[`, numeric(6), "ar")) # 6 x 3  AR(6) coef
relD <- abs(mar / c(m6$ar) - 1) # |rel. difference to "ols"|
stopifnot(identical(dim(relD), c(6L, 3L)),
          ## AR(6) coefficients are "relatively equal":
          print(colMeans(relD)) < 0.1, max(relD) < 0.2)


### tests using presidents, contains missing values
acf(presidents, na.action = na.pass)
pacf(presidents, na.action = na.pass)
## graphs in example(acf) suggest order 1 or 3
(fit1 <- arima(presidents, c(1, 0, 0)))
tsdiag(fit1)
(fit3 <- arima(presidents, c(3, 0, 0)))  # smaller AIC
tsdiag(fit3)

## Short example for bug PR#15832:
e <- rep(c(1.48e-6, 1.49e-6, 1.5e-6, 1.51e-6), c(2,3,9,7))
stopifnot(abs(acf(e, plot=FALSE)$acf) <= 1)
## Failed for R <= 3.2.0



### tests of arima:
arima(USAccDeaths, order = c(0,1,1), seasonal = list(order=c(0,1,1)))
arima(USAccDeaths, order = c(0,1,1), seasonal = list(order=c(0,1,1)),
      method = "CSS") # drops first 13 observations.

## test fitting with NAs
tmp <- LakeHuron
trend <- time(LakeHuron) - 1920
tmp[c(17, 45, 96)] <- NA
arima(tmp, order=c(2,0,0), xreg=trend)
arima(tmp, order=c(1,1,1), xreg=trend)
trend[c(20, 67)] <- NA
arima(tmp, order=c(2,0,0), xreg=trend)

## tests of prediction
predict(arima(lh, order=c(1,0,1)), n.ahead=5)
predict(arima(lh, order=c(1,1,0)), n.ahead=5)
predict(arima(lh, order=c(0,2,1)), n.ahead=5)

## test of init
arima(lh, order = c(1,0,1), init = c(0.5, 0.5, NA))
arima(lh, order = c(1,0,1), init = c(0.5, 2, NA))
try(arima(lh, order = c(1,0,1), init = c(2, NA, NA)))

## test of fixed
arima(lh, order = c(1,0,1), fixed = c(0.5, NA, NA), trans = FALSE)
trend <- time(LakeHuron) - 1920
arima(LakeHuron, order=c(2,0,0), xreg=trend)
arima(x = LakeHuron, order = c(2, 0, 0), xreg = trend,
      fixed = c(NA, NA, 580, -0.02))
arima(x = LakeHuron, order = c(2, 0, 0), xreg = trend,
      fixed = c(NA, NA, 580, 0))


### model selection from WWWusage
aics <- matrix(, 6, 6, dimnames=list(p=0:5, q=0:5))
for(q in 1:5) aics[1, 1+q] <- arima(WWWusage, c(0,1,q),
    optim.control = list(maxit = 500))$aic
for(p in 1:5)
   for(q in 0:5) aics[1+p, 1+q] <- arima(WWWusage, c(p,1,q),
       optim.control = list(maxit = 500))$aic
round(aics - min(aics, na.rm=TRUE), 2)



### nottem
nott <- window(nottem, end=c(1936,12))
fit <- arima(nott,order=c(1,0,0), list(order=c(2,1,0), period=12))
nott.fore <- predict(fit, n.ahead=36)
ts.plot(nott, nott.fore$pred, nott.fore$pred+2*nott.fore$se,
        nott.fore$pred-2*nott.fore$se, gpars=list(col=c(1,1,4,4)))


### StructTS
(fit <- StructTS(log10(UKgas), type = "BSM"))
(fit <- StructTS(log10(UKgas), type = "BSM", fixed=c(0, NA, NA, NA)))
(fit <- StructTS(log10(UKgas), type = "BSM", fixed=c(NA, 0, NA, NA)))
(fit <- StructTS(log10(UKgas), type = "BSM", fixed=c(NA, NA, NA, 0)))

### from AirPassengers
## The classic `airline model', by full ML
(fit <- arima(log10(AirPassengers), c(0, 1, 1),
              seasonal = list(order=c(0, 1 ,1), period=12)))
update(fit, method = "CSS")
update(fit, x=window(log10(AirPassengers), start = 1954))
pred <- predict(fit, n.ahead = 24)
tl <- pred$pred - 1.96 * pred$se
tu <- pred$pred + 1.96 * pred$se
ts.plot(AirPassengers, 10^tl, 10^tu, log = "y", lty = c(1,2,2))

## full ML fit is the same if the series is reversed, CSS fit is not
ap0 <- rev(log10(AirPassengers))
attributes(ap0) <- attributes(AirPassengers)
fr1 <- arima(ap0, c(0, 1, 1), seasonal = list(order=c(0, 1 ,1), period=12))
fr2 <- arima(ap0, c(0, 1, 1), seasonal = list(order=c(0, 1 ,1), period=12),
             method = "CSS")
i <- c("coef", "sigma2", "var.coef")
stopifnot(all.equal(fr1[i], fit[i], tol=4e-4))# 64b: 9e-5 is ok

## Structural Time Series
ap <- log10(AirPassengers) - 2
(fit <- StructTS(ap, type= "BSM"))
par(mfrow=c(1,2))
plot(cbind(ap,   fitted(fit)), plot.type = "single")
plot(cbind(ap, tsSmooth(fit)), plot.type = "single")

## PR14925
a <- ts(matrix(1:36, 12), start = 2000, freq = 12)
b <- ts(matrix(1:48, 16), start = c(1999,9), freq = 12)
window(a, start = c(2000,6)) <- window(b, start = c(2000,6), end = c(2000,12))
## failed in R < 2.15.1


## ts() and t(ts(.)) classes and  is.mts()
(mCls   <- class(aics))             # == c("matrix", "array")
(nmAmat <- names(attributes(aics))) # == c("dim", "dimnames")
ata <- attributes(ta <- t(a))
str(att <- attributes(tts <- t(TS <- ts(cbind(1, 1:20)))))
cat("TS: "); class(TS) ; cat(" attributes(TS):  "); str(attributes(TS), indent.str="  attr> ")
tools::assertError(verbose = TRUE, ts(numeric()))
stopifnot(exprs = {
    is.mts(a)
    is.mts(b)
    identical(class(ta), mCls)    # no "ts"
    identical(names(ata), nmAmat) # no "class"
    is.mts(EuStockMarkets)
    is.mts(Seatbelts)
    !is.mts(t(b))
    { ap3 <- AiP <- AirPassengers; dim(ap3) <- c(3,4,12)
        is.character(class(ap3) <- class(AiP) <- class(Seatbelts)) }
    !is.mts(AiP) # is.mts(.) was TRUE, wrongly in R <= 4.2.x
    identical(class(tts), mCls)
    identical(names(att), nmAmat)# had "class" at some point
    is.ts(ts())# an NA
    !is.mts(structure(numeric(), class = "mts"))
    !is.mts(structure(numeric(), class = c("mts", "ts", "matrix")))
})
## is.mts() was too simplistic in R <= 4.2.x

assertErrV  <- function(...) tools::assertError  (..., verbose=TRUE)
assertWarnV <- function(...) tools::assertWarning(..., verbose=TRUE)
identical3 <- function(a,b,c)   identical(a,b) && identical(b,c)
identical4 <- function(a,b,c,d) identical(a,b) && identical3(b,c,d)
##' get value of `expr` and keep warning as attribute (if there is one)
getVaW <- function(expr, obj=FALSE) {
    W <- NULL
    withCallingHandlers(val <- expr,
                        warning = function(w) {
                            W <<- if(obj) w else conditionMessage(w)
                            invokeRestart("muffleWarning") })
    structure(val %||% quote(._NULL_()), warning = W) # NULL cannot have attr.
}
##
onWindows <- .Platform$OS.type == "windows"
englishMsgs <- {
    ## 1. LANGUAGE takes precedence over locale settings:
    if(nzchar(lang <- Sys.getenv("LANGUAGE")))
        lang == "en"
    else { ## 2. Query the  locale
        if(!onWindows) {
            ## sub() :
            lc.msgs <- sub("\\..*", "", print(Sys.getlocale("LC_MESSAGES")))
            lc.msgs == "C" || substr(lc.msgs, 1,2) == "en"
        } else { ## Windows
            lc.type <- sub("\\..*", "", sub("_.*", "", print(Sys.getlocale("LC_CTYPE"))))
            lc.type == "English" || lc.type == "C"
        }
    }
}
cat(sprintf("English messages: %s\n", englishMsgs))
options(warn = 2, # only caught or asserted warnings
        width = 99) # instead of 80

## More Ops.ts() cases
m2 <- matrix(1:12, 6,2)
str(tm <- ts(m2))
typeof(tm.7 <- ts(m2,   start = 7)) # integer
typeof(tmd7 <- ts(m2+0, start = 7)) # double
typeof(td2 <- (tm+0) + tm) # d
assertErrV(tm + ts(matrix(1:12, 6,3))) # non-conformable
   (r  <- getVaW(tm  + tm.7)) # all three r* are 0 x 2 matrices
str(rd.<- getVaW(tm.7+ (tm+0)))
    rd <- getVaW(tm  + tmd7)
stopifnot(exprs = {
    identical4(c(0L, 2L), dim(r), dim(rd), dim(rd.))
    is.character(w <- attr(r,  "warning"))
    !englishMsgs || w == "non-intersecting series"
    identical3(w, attr(rd, "warning"), attr(rd., "warning"))
    is.integer(r) # as tm and tm.7
    is.double(rd.)# *not* true in R <= 4.5.3
    is.double(rd) # (neither)
    is.double(td2)
})
##



cat('Time elapsed: ', proc.time() - .proctime00,'\n')
