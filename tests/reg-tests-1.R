## PR 640 (diff.default computes an incorrect starting time)
## By: Laimonis Kavalieris <lkavalieris@maths.otago.ac.nz>
library(ts)
y <- ts(rnorm(24),freq=12)
x <- ts(rnorm(24),freq=12)
arima0(y,xreg=x,seasonal=list(order=c(0,1,0)))
## Comments:
