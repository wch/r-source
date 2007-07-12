#### Empirical Cumulative Distribution Functions :  "ecdf"
##--  inherit from  "stepfun"

## Constructor
ecdf <- function (x)
{
    x <- sort(x) # drops NAs
    n <- length(x)
    if(n < 1) stop("'x' must have 1 or more non-missing values")
    vals <- unique(x)
    rval <- approxfun(vals, cumsum(tabulate(match(x, vals)))/n,
		      method = "constant", yleft = 0, yright = 1, f = 0,
                      ties = "ordered")
    class(rval) <- c("ecdf", "stepfun", class(rval))
    attr(rval, "call") <- sys.call()
    rval
}

print.ecdf <- function (x, digits= getOption("digits") - 2, ...)
{
    numform <- function(x) paste(formatC(x, digits=digits), collapse=", ")
    cat("Empirical CDF \nCall: ")
    print(attr(x, "call"), ...)
    n <- length(xx <- eval(expression(x), envir = environment(x)))
    i1 <- 1:min(3,n)
    i2 <- if(n >= 4) max(4,n-1):n else integer(0)
    cat(" x[1:",n,"] = ", numform(xx[i1]),
	if(n>3) ", ", if(n>5) " ..., ", numform(xx[i2]), "\n", sep = "")
    invisible(x)
}

summary.ecdf <- function(object, ...)
{
    cat("Empirical CDF:	 ",
	eval(expression(n), envir = environment(object)),
        "unique values with summary\n")
    summary(knots(object), ...)
}


## add  conf.int = 0.95
## and  conf.type = c("none", "KS")
## (these argument names are compatible to Kaplan-Meier survfit() !)
## and use ./KS-confint.R 's  code !!!

plot.ecdf <- function(x, ..., ylab="Fn(x)", verticals = FALSE,
		      col.01line = "gray70")
{
    plot.stepfun(x, ..., ylab = ylab, verticals = verticals)
    abline(h=c(0,1), col = col.01line, lty=2)
}
