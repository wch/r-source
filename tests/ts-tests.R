### ar
data(lh)
ar(lh)
ar(lh, method="burg")
ar(lh, method="ols")
ar(lh, FALSE, 4) # fit ar(4)
ar.ols(lh)
ar.ols(lh, FALSE, 4) # fit ar(4) by OLS

data(LakeHuron)
ar(LakeHuron)
ar(LakeHuron, method="burg")
ar(LakeHuron, method="ols")
ar(LakeHuron, method="mle")

data(sunspot)
ar(sunspot.year, method = "yw")
ar(sunspot.year, method = "burg")
ar(sunspot.year, method = "ols")
ar(sunspot.year, method = "mle")


### tests using presidents
data(presidents) # contains missing values
acf(presidents, na.action = na.pass)
pacf(presidents, na.action = na.pass)
## graphs in example(acf) suggest order 1 or 3
(fit1 <- arima0(presidents, c(1, 0, 0), delta = -1))  # avoid warning
tsdiag(fit1)
(fit3 <- arima0(presidents, c(3, 0, 0), delta = -1))  # smaller AIC
tsdiag(fit3)


### tests of arima0:
data(lh); data(USAccDeaths)
arima0(USAccDeaths, order = c(0,1,1), seasonal = list(order=c(0,1,1)))
arima0(USAccDeaths, order = c(0,1,1), seasonal = list(order=c(0,1,1)),
       delta = -1) # full ML
arima0(USAccDeaths, order = c(0,1,1), seasonal = list(order=c(0,1,1)),
       method = "CSS") # drops first 13 observations.

## test fitting with NAs
tmp <- LakeHuron
trend <- time(LakeHuron) - 1920
tmp[c(17, 45, 96)] <- NA
arima0(tmp, order=c(2,0,0), xreg=trend, delta = -1)
arima0(tmp, order=c(1,1,1), xreg=trend, delta = -1)
trend[c(20, 67)] <- NA
arima0(tmp, order=c(2,0,0), xreg=trend, delta = -1)

## tests of prediction
predict(arima0(lh, order=c(1,0,1)), n.ahead=5)
predict(arima0(lh, order=c(1,1,0)), n.ahead=5)
predict(arima0(lh, order=c(0,2,1)), n.ahead=5)

## test of init
arima0(lh, order = c(1,0,1), init = c(0.5, 0.5, NA))
arima0(lh, order = c(1,0,1), init = c(0.5, 2, NA))
try(arima0(lh, order = c(1,0,1), init = c(2, NA, NA)))

## test of fixed
arima0(lh, order = c(1,0,1), fixed = c(0.5, NA, NA), trans = FALSE)
trend <- time(LakeHuron) - 1920
arima0(LakeHuron, order=c(2,0,0), xreg=trend, delta = -1)
arima0(x = LakeHuron, order = c(2, 0, 0), xreg = trend,
      delta = -1, fixed = c(NA, NA, 580, -0.02))
arima0(x = LakeHuron, order = c(2, 0, 0), xreg = trend,
      delta = -1, fixed = c(NA, NA, 580, 0))


### model selection from WWWusage
data(WWWusage)
aics <- matrix(, 6, 6, dimnames=list(p=0:5, q=0:5))
for(q in 1:5) aics[1, 1+q] <- arima0(WWWusage, c(0,1,q),
    optim.control = list(maxit = 500))$aic
for(p in 1:5)
   for(q in 0:5) aics[1+p, 1+q] <- arima0(WWWusage, c(p,1,q),
       optim.control = list(maxit = 500))$aic
round(aics - min(aics, na.rm=TRUE), 2)


### nottem
data(nottem)
nott <- window(nottem, end=c(1936,12))
fit <- arima0(nott,order=c(1,0,0), list(order=c(2,1,0), period=12))
nott.fore <- predict(fit, n.ahead=36)
ts.plot(nott, nott.fore$pred, nott.fore$pred+2*nott.fore$se,
        nott.fore$pred-2*nott.fore$se, gpars=list(col=c(1,1,4,4)))


