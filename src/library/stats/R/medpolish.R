medpolish <-
    function (x, eps=0.01, maxiter=10, trace.iter = TRUE, na.rm = FALSE)
{
    z <- as.matrix(x)
    nr <- nrow(z)
    nc <- ncol(z)
    t <- 0
    r <- numeric(nr)
    c <- numeric(nc)
    oldsum <- 0
    for(iter in 1:maxiter) {
	rdelta <- apply(z, 1, median, na.rm = na.rm)
	z <- z - matrix(rdelta, nr=nr, nc=nc)
	r <- r + rdelta
	delta <- median(c, na.rm = na.rm)
	c <- c - delta
	t <- t + delta
	cdelta <- apply(z, 2, median, na.rm = na.rm)
	z <- z - matrix(cdelta, nr=nr, nc=nc, byrow=TRUE)
	c <- c + cdelta
	delta <- median(r, na.rm = na.rm)
	r <- r - delta
	t <- t + delta
	newsum <- sum(abs(z), na.rm = na.rm)
	converged <- newsum==0 || abs(newsum-oldsum) < eps*newsum
	if(converged) break
	oldsum <- newsum
	if(trace.iter) cat(iter,":", newsum,"\n")
    }
    if(converged) { if(trace.iter) cat("Final:", newsum,"\n")
    } else warning("medpolish() not converged in ",maxiter, " iterations")
    names(r) <- rownames(z)
    names(c) <- colnames(z)
    ans <- list(overall=t, row=r, col=c, residuals=z,
		name=deparse(substitute(x)))
    class(ans) <- "medpolish"
    ans
}

print.medpolish <- function(x, digits=getOption("digits"), ...)
{
    cat("\nMedian Polish Results (Dataset: \"", x$name, "\")\n", sep="")
    cat("\nOverall:", x$overall, "\n\nRow Effects:\n")
    print(x$row, digits=digits, ...)
    cat("\nColumn Effects:\n")
    print(x$col, digits=digits, ...)
    cat("\nResiduals:\n")
    print(x$residuals, digits=max(2, digits-2), ...)
    cat("\n")
    invisible(x)
}

plot.medpolish <- function(x, main="Tukey Additivity Plot", ...) {
    plot(outer(x$row,x$col)/x$overall, x$residuals,
	 main=main, xlab="Diagnostic Comparison Values",
	 ylab="Residuals", ...)
    abline(h=0, v=0, lty="dotted")
}
