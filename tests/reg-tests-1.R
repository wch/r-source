## PR 640 (diff.default computes an incorrect starting time)
## By: Laimonis Kavalieris <lkavalieris@maths.otago.ac.nz>
library(ts)
y <- ts(rnorm(24),freq=12)
x <- ts(rnorm(24),freq=12)
arima0(y,xreg=x,seasonal=list(order=c(0,1,0)))
## Comments:

## PR 644 (crash using fisher.test on Windows)
## By: Uwe Ligges <ligges@statistik.uni-dortmund.de>
library(ctest)
x <- matrix(c(2, 2, 4, 8, 6, 0, 1, 1, 7, 8, 1, 3, 1, 3, 7, 4, 2, 2, 2,
              1, 1, 0, 0, 0, 0, 0, 1, 1, 2, 0, 1, 1, 0, 2, 1, 0, 0, 0),
            nc = 2)
fisher.test(x)
## Comments:

## PR 753 (step can't find variables)
##
x<-data.frame(a=rnorm(10),b=rnorm(10),c=rnorm(10))
x0.lm<-lm(a~1,data=x)
step(x0.lm,~b+c)
## Comments:
