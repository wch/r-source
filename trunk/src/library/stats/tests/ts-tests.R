## tests of time-series functionality

.proctime00 <- proc.time()
library(stats)

postscript("ts-test.ps")

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


### tests using presidents, contains missing values
acf(presidents, na.action = na.pass)
pacf(presidents, na.action = na.pass)
## graphs in example(acf) suggest order 1 or 3
(fit1 <- arima(presidents, c(1, 0, 0)))
tsdiag(fit1)
(fit3 <- arima(presidents, c(3, 0, 0)))  # smaller AIC
tsdiag(fit3)


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
arima(ap0, c(0, 1, 1), seasonal = list(order=c(0, 1 ,1), period=12))
arima(ap0, c(0, 1, 1), seasonal = list(order=c(0, 1 ,1), period=12),
      method = "CSS")

## Structural Time Series
ap <- log10(AirPassengers) - 2
(fit <- StructTS(ap, type= "BSM"))
par(mfrow=c(1,2))
plot(cbind(ap, fitted(fit)), plot.type = "single")
plot(cbind(ap, tsSmooth(fit)), plot.type = "single")

cat('Time elapsed: ', proc.time() - .proctime00,'\n')
