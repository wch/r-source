integrate<- function(f, lower, upper, subdivisions=100,
		     rel.tol = .Machine$double.eps^.25,
		     abs.tol = rel.tol, stop.on.error = TRUE,
		     keep.xy = FALSE, aux = NULL, ...)
{
    f <- match.fun(f)
    ff <- function(x) f(x, ...)
    limit <- as.integer(subdivisions)
    if (limit < 1 || (abs.tol <= 0 &&
	rel.tol < max(50*.Machine$double.eps, 0.5e-28)))
	stop("invalid parameter values")
    if(is.finite(lower) && is.finite(upper)) {
	wk <- .External("call_dqags",
			ff, rho = environment(),
			as.double(lower), as.double(upper),
			as.double(abs.tol), as.double(rel.tol),
			limit = limit,
			PACKAGE = "base")
    } else { # indefinite integral
	if(is.na(lower) || is.na(upper)) stop("a limit is missing")
	if (is.finite(lower)) {
	    inf <- 1
	    bound <- lower
	} else if (is.finite(upper)) {
	    inf <- -1
	    bound <- upper
	} else {
	    inf <- 2
	    bound <- 0.0
	}
	wk <- .External("call_dqagi",
			ff, rho = environment(),
			as.double(bound), as.integer(inf),
			as.double(abs.tol), as.double(rel.tol),
			limit = limit,
			PACKAGE = "base")
    }
    res <- wk[c("value", "abs.error", "subdivisions")]
    res$message <-
	switch(wk$ierr + 1,
	       "OK",
	       "maximum number of subdivisions reached",
	       "roundoff error was detected",
	       "extremely bad integrand behaviour",
	       "roundoff error is detected in the extrapolation table",
	       "the integral is probably divergent",
	       "the input is invalid")
    if(wk$ierr == 6 || (wk$ierr > 0 && stop.on.error)) stop(res$message)
    res$call <- match.call()
    class(res) <- "integrate"
    res
}

print.integrate <- function (x, digits=getOption("digits"), ...)
{
    if(x$message == "OK") cat(format(x$value, digits=digits),
       " with absolute error < ", format(x$abs.error, digits=2),
       "\n", sep = "")
    else cat("failed with message ", sQuote(x$message), "\n", sep = "")
    invisible(x)
}
