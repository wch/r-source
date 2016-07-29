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

(sdf8 <- smooth.spline(y18, df = 8, control=list(trace=TRUE)))# 11 iter.
sdf8$df - 8 # -0.0009159978
(sdf8. <- smooth.spline(y18, df = 8, control=list(tol = 1e-8)))# 14 iter.

e <- try(smooth.spline(y18, spar = 50)) #>> error : spar 'way too large'
stopifnot(inherits(e, "try-error"))
e <- try(smooth.spline(y18, spar = -9)) #>> error : .. too small'
stopifnot(inherits(e, "try-error"))


## "extreme" range of spar / lambda --> problem/bug for too large lambda
e10 <- c(-20, -10, -7, -4:4, 7, 10)
rss <- lapply(setNames(10^e10, paste0("lambda = 10^", e10)),
              function(LAM) smooth.spline(y18, spar = c(lambda = LAM)))
rss

invisible(lapply(seq_along(rss), function(i) lines(predict(rss[[i]], xx), col=i)))
i18 <- 1:18
abline(lm(y18 ~ i18), col = adjustcolor('tomato',1/2), lwd = 5, lty = 3)
## --> lambda = 10^10 is clearly wrong
legend("topleft", as.expression(lapply(e10, function(E)
				substitute(lambda == 10^e, list(e = E)))),
       ncol = 2, bty = "n", col = seq_along(rss), lty=1)
