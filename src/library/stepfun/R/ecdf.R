#### Empirical Cumulative Distribution Functions :  "ecdf"
##--  inherit from  "stepfun"

## Constructor
ecdf <- function (x)
{
    x <- sort(x)
    n <- length(x)
    rval <- approxfun(x, (1:n)/n,
		      method = "constant", yleft=0, yright=1, f = 0)
    class(rval) <- c("ecdf", "stepfun", class(rval))
    attr(rval, "call") <- sys.call()
    rval
}

print.ecdf <- function (Fn, digits=options("digits")[[1]]-2, ...)
{
    numform <- function(x)	paste(formatC(x, dig=digits), collapse=", ")
    cat("Empirical CDF \nCall: ")
    print(attr(Fn, "call"), ...)
    n <- length(x <- eval(expression(x),env = environment(Fn)))
    i1 <- 1:min(3,n)
    i2 <- if(n>=4) max(4,n-1):n else integer(0)
    cat(" x[1:",n,"] = ", numform(x[i1]),
	if(n>3) ", ", if(n>5) " ..., ", numform(x[i2]), "\n", sep = "")
    invisible(Fn)
}

summary.ecdf <- function(Fn, ...){
    cat("Empirical CDF:	 ",
	eval(expression(n),env = environment(Fn)),"obs.\n")
    summary(knots(Fn), ...)
}


plot.ecdf <- function(..., verticals = FALSE, col.01line = "gray70") {
    plot.stepfun(ylab="Fn(x)", ..., verticals = verticals)
    abline(h=c(0,1), col = col.01line, lty=2)
}
