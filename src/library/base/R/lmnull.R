###-------- This is  UGLY :  a lot of coding is just doubled from  ./lm.R  ----

anova.lm.null <- function (object, ...)
{
    if (length(list(object, ...)) > 1)
	return(anova.lmlist(object, ...))
    w <- weights(object)
    ssr <- sum(if (is.null(w))resid(object)^2 else w * resid(object)^2)
    ##comp <- object$effects[1:object$rank]
    ##asgn <- object$assign[object$qr$pivot][1:object$rank]
    dfr <- df.residual(object)
    ss <- ssr
    df <- dfr
    ms <- ss/df
    f <- ms/(ssr/dfr)
    p <- 1 - pf(f, df, dfr)
    table <- data.frame(df, ss, ms, f, p)
    table[length(p), 4:5] <- NA
    dimnames(table) <- list(c(attr(object$terms, "term.labels"), "Residuals"),
			    c("Df", "Sum Sq", "Mean Sq", "F value", "Pr(>F)"))
    structure(table, heading = c("Analysis of Variance Table\n",
                     paste("Response:", formula(object)[[2]])),
	      class= c("anova", "data.frame"))# was "tabular"
}

print.lm.null <- function (x, digits = max(3, getOption("digits") - 3), ...)
{
    cat("\nCall:\n", deparse(x$call), "\n\n", sep = "")
    cat("No coefficients:\n\n")
    invisible(x)
}

print.summary.lm.null <- function (x, digits = max(3, getOption("digits") - 3), ...)
{
    cat("\nCall:\n")
    cat(paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n\n", sep = "")
    resid <- x$residuals
    df <- x$df
    rdf <- df[2]
    if (rdf > 5) {
	cat("Residuals:\n")
	if (length(dim(resid)) == 2) {
	    rq <- apply(t(resid), 1, quantile)
	    dimnames(rq) <- list(c("Min", "1Q", "Median", "3Q", "Max"),
				 dimnames(resid)[[2]])
	}
	else {
	    rq <- quantile(resid)
	    names(rq) <- c("Min", "1Q", "Median", "3Q", "Max")
	}
	print(rq, digits = digits, ...)
    }
    else if (rdf > 0) {
	cat("Residuals:\n")
	print(resid, digits = digits, ...)
    }
    else cat("\nNo Coefficients:\n")
    cat("\nResidual standard error:",
	format(signif(x$sigma, digits)), "on", rdf, "degrees of freedom\n")
    cat("\n")
    invisible(x)
}

summary.lm.null <- function (z, correlation = FALSE, ...)
{
    n <- length(z$fitted.values)
    p <- 0
    r <- resid(z)
    f <- fitted(z)
    w <- weights(z)
    if (is.null(z$terms)) {
	stop("invalid \'lm\' object:  no terms component")
    }
    else {
	rss <- sum(r^2)
	mss <- sum(f^2)
    }
    resvar <- rss/(n - p)
###R <- chol2inv(z$qr$qr[p1, p1, drop = FALSE])
###se <- sqrt(diag(R) * resvar)
###est <- z$coefficients[z$qr$pivot[p1]]
###tval <- est/se
    ans <- z[c("call", "terms")]
    ans$residuals <- r
    ans$coefficients <- NULL
    ans$sigma <- sqrt(resvar)
    ans$df <- c(p, n - p, n - p)
    ans$r.squared <- 0
    ans$cov.unscaled <- NULL
    class(ans) <- "summary.lm.null"
    ans
}

### The next two are used by lm.fit when it detects a null design
### matrix. A bit of a kludge, but it makes drop1 and friends work
### with no-intercept models

lm.fit.null <- function (x, y, method = "qr", tol = 1e-07, ...)
    list(coefficients = numeric(0), residuals = y, fitted.values = 0 *
         y, weights = NULL, rank = 0, df.residual = length(y))


lm.wfit.null <- function (x, y, w, method = "qr", tol = 1e-07, ...)
    list(coefficients = numeric(0), residuals = y, fitted.values = 0 *
         y, weights = w, rank = 0, df.residual = length(y))

model.matrix.lm.null <- function(x,...)
{
  rval <- matrix(ncol=0, nrow=length(object$y))
  attr(rval,"assign") <- integer(0)
}
