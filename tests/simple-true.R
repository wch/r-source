##-*- R -*-
##-- These should all return  'TRUE'

all(1:12 == cumsum(rep(1,12)))

x <- rnorm(127); sx <- sum(x);  abs((sum(rev(x)) -sx)) < 1e-12 * abs(sx)

typeof(1:4) == "integer" #-- fails for 0.2, 0.3,.., 0.9

all((0:6) == pi + ((-pi):pi))
all((0:7) == (pi+seq(-pi,pi, len=8))*7/(2*pi))

## This should be symmetric !!
cc <- c(1:10,10:1) ;		all(cc == rev(cc))
##nomore:
##cc <- convolve(cc, c(1,2,1));	all(cc == rev(cc))

## Real Trig.:
cos(0) == 1
sin(3*pi/2) == cos(pi)
x <- rnorm(99)
all( sin(-x) == - sin(x))
all( cos(-x) == cos(x))

x <- 1:99/100
all(abs(1 - x / asin(sin(x))) <= .Machine$double.eps)
all(abs(1 - x / atan(tan(x))) <= .Machine$double.eps)
