hat <- function(x, intercept = TRUE)
{
    if(is.qr(x)) n <- nrow(x$qr)
    else {
	if(intercept) x <- cbind(1, x)
	n <- nrow(x)
	x <- qr(x)
    }
    apply(qr.qy(x, diag(1, nrow = n, ncol = x$rank))^2, 1, sum)
}

weighted.residuals <- function(obj, drop0 = TRUE)
{
    w <- weights(obj)
    r <- residuals(obj)
    if(is.null(w)) r
    else if(drop0) (sqrt(w)*r)[w != 0]
    else sqrt(w)*r
}

lm.influence <- function (lm.obj)
{
    if (is.empty.model(lm.obj$terms)) {
	warning("Can\'t compute influence on an empty model")
	return(NULL)
    }
    n <- as.integer(nrow(lm.obj$qr$qr))
    k <- as.integer(lm.obj$qr$rank)
    ## in na.exclude case, omit NAs.
    e <- na.omit(weighted.residuals(lm.obj))
    if(length(e) != n)
        stop("non-NA residual length does not match cases used in fitting")
    res <- .Fortran("lminfl",
                    lm.obj$qr$qr,
                    n,
                    n,
                    k,
                    lm.obj$qr$qraux,
                    e,
                    hat = double(n),
                    coefficients = matrix(0, nr = n, nc = k),
                    sigma = double(n),
                    DUP = FALSE, PACKAGE="base")[c("hat", "coefficients", "sigma")]
    if(!is.null(lm.obj$na.action)) {
        hat <- naresid(lm.obj$na.action, res$hat)
        hat[is.na(hat)] <- 0 # omitted cases have 0 leverage
        res$hat <- hat
        coefficients <- naresid(lm.obj$na.action, res$coefficients)
        coefficients[is.na(coefficients)] <- 0 # omitted cases have 0 change
        res$coefficients <- coefficients
        sigma <- naresid(lm.obj$na.action, res$sigma)
        sigma[is.na(sigma)] <- sqrt(deviance(lm.obj)/df.residual(lm.obj))
        res$sigma <- sigma
    }
    res
}

rstandard <- function(lm.obj, infl = lm.influence(lm.obj),
                      res = weighted.residuals(lm.obj),
                      sd = sqrt(deviance(lm.obj)/df.residual(lm.obj)))
    res / (sd * sqrt(1 - infl$hat))
## OLD (<= 0.90.1); fails for glm objects:
##  res / (summary(lm.obj)$sigma * sqrt(1 - infl$hat))


rstudent <- function(lm.obj, infl = lm.influence(lm.obj),
                     res = weighted.residuals(lm.obj))
    res / (infl$sigma * sqrt(1 - infl$hat))

dffits <- function(lm.obj, infl = lm.influence(lm.obj),
                   res = weighted.residuals(lm.obj))
    res * sqrt(infl$hat)/(infl$sigma*(1-infl$hat))

dfbetas <- function (lm.obj, infl = lm.influence(lm.obj))
{
    xxi <- chol2inv(lm.obj$qr$qr, lm.obj$qr$rank)
    d <- infl$coefficients/(outer(infl$sigma, sqrt(diag(xxi))))
    dimnames(d) <- list(case.names(lm.obj), variable.names(lm.obj))
    d
}

covratio <- function(lm.obj, infl = lm.influence(lm.obj),
                     res = weighted.residuals(lm.obj))
{
    n <- nrow(lm.obj$qr$qr)
    p <- lm.obj$rank
    omh <- 1-infl$hat
    e.star <- res/(infl$sigma*sqrt(omh))
    1/(omh*(((n - p - 1)+e.star^2)/(n - p))^p)
}

## Used in plot.lm(); allow passing of known parts:
cooks.distance <- function(lm.obj, infl = lm.influence(lm.obj),
                           res = weighted.residuals(lm.obj),
                           sd = sqrt(deviance(lm.obj)/df.residual(lm.obj)))
{
    p <- lm.obj$rank
    hat <- infl$hat
    ((res/(sd * (1 - hat)))^2 * hat)/p
}

influence.measures <- function(lm.obj)
{
    is.influential <- function(infmat, n)
    {
	## Argument is result of using influence.measures
	## Returns a matrix  of logicals structured like the argument
	k <- ncol(infmat) - 4
	if(n <= k)
	    stop("Too few cases, n < k")
	absmat <- abs(infmat)
	result <- cbind(absmat[, 1:k] > 1, # |dfbetas| > 1
			absmat[, k + 1] > 3 * sqrt(k/(n - k)), # |dffit| > ..
			abs(1 - infmat[, k + 2]) > (3*k)/(n - k),# |1-cov.r| >..
			pf(infmat[, k + 3], k, n - k) > 0.5,# "P[cook.d..]" > .5
			infmat[, k + 4] > (3 * k)/n) # hat > 3k/n
	dimnames(result) <- dimnames(infmat)
	result
    }
    infl <- lm.influence(lm.obj)
    p <- lm.obj$rank
    e <- weighted.residuals(lm.obj)
    s <- sqrt(sum(e^2, na.rm=TRUE)/df.residual(lm.obj))
    xxi <- chol2inv(lm.obj$qr$qr, lm.obj$qr$rank)
    si <- infl$sigma
    h <- infl$hat
    dfbetas <- infl$coefficients / outer(infl$sigma, sqrt(diag(xxi)))
    vn <- variable.names(lm.obj); vn[vn == "(Intercept)"] <- "1_"
    colnames(dfbetas) <- paste("dfb",abbreviate(vn),sep=".")
    dffits <- e*sqrt(h)/(si*(1-h))
    cov.ratio <- (si/s)^(2 * p)/(1 - h)
    cooks.d <- ((e/(s * (1 - h)))^2 * h)/p
    dn <- dimnames(lm.obj$qr$qr)
    infmat <- cbind(dfbetas, dffit = dffits, cov.r = cov.ratio,
		    cook.d = cooks.d, hat=h)
    is.inf <- is.influential(infmat, sum(h>0))
    ans <- list(infmat = infmat, is.inf = is.inf, call = lm.obj$call)
    class(ans) <- "infl"
    ans
}

print.infl <- function(x, digits = max(3, getOption("digits") - 4), ...)
{
    ## `x' : as the result of  influence.measures(.)
    cat("Influence measures of\n\t", deparse(x$call),":\n\n")
    is.star <- apply(x$is.inf, 1, any, na.rm = TRUE)
    print(data.frame(x$infmat,
		     inf = ifelse(is.star, "*", " ")),
	  digits = digits, ...)
    invisible(x)
}

summary.infl <- function(object, digits = max(2, getOption("digits") - 5), ...)
{
    ## object must be as the result of	influence.measures(.)
    is.inf <- object$is.inf
    is.star <- apply(is.inf, 1, any, na.rm=TRUE)
    is.inf <- is.inf[is.star,]
    cat("Potentially influential observations of\n\t",
	deparse(object$call),":\n")
    if(any(is.star)) {
	imat <- object $ infmat[is.star,, drop = FALSE]
	if(is.null(rownam <- dimnames(object $ infmat)[[1]]))
	    rownam <- format(seq(is.star))
	dimnames(imat)[[1]] <- rownam[is.star]
	chmat <- format(round(imat, digits = digits))
	cat("\n")
	print(array(paste(chmat,c("","_*")[1+is.inf], sep=''),
		    dimnames = dimnames(imat), dim=dim(imat)),
	      quote = FALSE)
	invisible(imat)
    } else {
	cat("NONE\n")
	numeric(0)
    }
}
