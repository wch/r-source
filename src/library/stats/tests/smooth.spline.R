## moved from ?smooth.spline, quite system-specific.
y18 <- c(1:3,5,4,7:3,2*(2:5),rep(10,4))
xx  <- seq(1,length(y18), len=201)
s2. <- smooth.spline(y18, cv = TRUE,
                     control = list(trace=TRUE, tol=1e-6,low= -3,maxit=20))
s2. ## i386-Linux: Df ~= (even! > ) 18 : interpolating -- much smaller PRESS
## {others, e.g., may end quite differently!}
plot(y18)
lines(predict(s2., xx), col = 4)
mtext(deparse(s2.$call,200), side= 1, line= -1, cex= 0.8, col= 4)

(sdf8 <- smooth.spline(y18, df = 8, control=list(trace=TRUE)))
sdf8$df - 8

try(smooth.spline(y18, spar = 50)) #>> error : spar 'way too large'
