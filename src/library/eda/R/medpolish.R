"medpolish" <-
function (x, eps=0.01, maxiter=10) 
{
	z <- as.matrix(x)
	nr <- nrow(z)
	nc <- ncol(z)
	t <- 0
	r <- rep(0, nr)
	c <- rep(0, nc)
	oldsum <- 0
	for(iter in 1:maxiter) {
		rdelta <- apply(z, 1, median)
		z <- z - matrix(rdelta, nr=nr, nc=nc)
		r <- r + rdelta
		delta <- median(c)
		c <- c - delta
		t <- t + delta
		cdelta <- apply(z, 2, median)
		z <- z - matrix(cdelta, nr=nr, nc=nc, byrow=TRUE)
		c <- c + cdelta
		delta <- median(r)
		r <- r - delta
		t <- t + delta
		newsum <- sum(abs(z))
		if(abs(1-oldsum/newsum)<eps) break;
		oldsum <- newsum
		cat(newsum,"\n")
		
	}
	cat(newsum,"\n")
	names(r) <- rownames(z)
	names(c) <- colnames(z)
	ans <- list(overall=t, row=r, col=c, residuals=z,
		    name=deparse(substitute(x)))
	class(ans) <- "medpolish"
	ans
}

"print.medpolish" <-
function(x) {
	cat("\nMedian Polish Results (Dataset: \"", x$name, "\")\n", sep="")
	cat("\nOverall:", x$overall, "\n")
	cat("\nRow Effects:\n")
	print(x$row)
	cat("\nColumn Effects:\n")
	print(x$col)
	cat("\nResiduals:\n")
	print(x$residuals)
	cat("\n")
}

"plot.medpolish" <-
function(x, main="Tukey Additivity Plot", ...) {
	plot(outer(x$row,x$col)/x$overall, x$residuals,
	     main=main, xlab="Diagnostic Comparison Values",
	     ylab="Residuals", ...)
	abline(h=0, v=0, lty="dotted")
}

# Local Variables:
# mode:R
# R-temp-buffer-p:t
# End:
