### "lm"  *and*	 "glm"	 leave-one-out influence measures

## this is mainly for back-compatibility (from "lsfit" time) -- use hatvalues()!
hat <- function(x, intercept = TRUE)
{
    if(is.qr(x)) n <- nrow(x$qr)
    else {
	if(intercept) x <- cbind(1, x)
	n <- nrow(x)
	x <- qr(x)
    }
    rowSums(qr.qy(x, diag(1, nrow = n, ncol = x$rank))^2)
}

## see PR#7961
weighted.residuals <- function(obj, drop0 = TRUE)
{
    w <- weights(obj)
    r <- residuals(obj, type="deviance")
    if(drop0 && !is.null(w)) r[w != 0] else r
}

lm.influence <- function (model, do.coef = TRUE)
{
    wt.res <- weighted.residuals(model)
    e <- na.omit(wt.res)

    if (model$rank == 0) {
        n <- length(wt.res) # drops 0 wt, may drop NAs
        sigma <- sqrt(deviance(model)/df.residual(model))
        res <- list(hat = rep(0, n), coefficients = matrix(0, n, 0),
                    sigma = rep(sigma, n), wt.res = e)
    } else {
        ## if we have a point with hat = 1, the corresponding e should be
        ## exactly zero.  Protect against returning Inf by forcing this
        e[abs(e) < 100 * .Machine$double.eps * median(abs(e))] <- 0
        n <- as.integer(nrow(model$qr$qr))
        k <- as.integer(model$qr$rank)
        ## in na.exclude case, omit NAs; also drop 0-weight cases
        if(NROW(e) != n)
            stop("non-NA residual length does not match cases used in fitting")
        do.coef <- as.logical(do.coef)
        res <- .Fortran("lminfl",
                        model$qr$qr,
                        n,
                        n,
                        k,
                        as.integer(do.coef),
                        model$qr$qraux,
                        wt.res = e,
                        hat = double(n),
                        coefficients= if(do.coef) matrix(0, n, k) else double(0),
                        sigma = double(n),
                        tol = 10 * .Machine$double.eps,
                        DUP = FALSE, PACKAGE="base"
                        )[c("hat", "coefficients", "sigma","wt.res")]
        if(!is.null(model$na.action)) {
            hat <- naresid(model$na.action, res$hat)
            hat[is.na(hat)] <- 0       # omitted cases have 0 leverage
            res$hat <- hat
            if(do.coef) {
                coefficients <- naresid(model$na.action, res$coefficients)
                coefficients[is.na(coefficients)] <- 0 # omitted cases have 0 change
                res$coefficients <- coefficients
            }
            sigma <- naresid(model$na.action, res$sigma)
            sigma[is.na(sigma)] <- sqrt(deviance(model)/df.residual(model))
            res$sigma <- sigma
        }
    }
    res$wt.res <- naresid(model$na.action, res$wt.res)
    res$hat[res$hat > 1 - 10*.Machine$double.eps] <- 1 # force 1
    names(res$hat) <- names(res$sigma) <- names(res$wt.res)
    if(!do.coef) ## drop it
	res$coefficients <- NULL
    else {
        rownames(res$coefficients) <- names(res$wt.res)
        colnames(res$coefficients) <- names(coef(model))[!is.na(coef(model))]
    }
    res
}

## The following is adapted from John Fox's  "car" :
influence <- function(model, ...) UseMethod("influence")
influence.lm  <- function(model, do.coef = TRUE, ...)
    lm.influence(model, do.coef = do.coef, ...)
influence.glm <- function(model, do.coef = TRUE, ...) {
    res <- lm.influence(model, do.coef = do.coef, ...)
    pRes <- na.omit(residuals(model, type = "pearson"))[model$prior.weights != 0]
    pRes <- naresid(model$na.action, pRes)
    names(res)[names(res) == "wt.res"] <- "dev.res"
    c(res, list(pear.res = pRes))
}

hatvalues <- function(model, ...) UseMethod("hatvalues")
hatvalues.lm <- function(model, infl = lm.influence(model, do.coef=FALSE), ...)
{
    hat <- infl$hat
    names(hat) <- names(infl$wt.res)
    hat
}

rstandard <- function(model, ...) UseMethod("rstandard")
rstandard.lm <- function(model, infl = lm.influence(model, do.coef=FALSE),
                         sd = sqrt(deviance(model)/df.residual(model)), ...)
{
    res <- infl$wt.res / (sd * sqrt(1 - infl$hat))
    res[is.infinite(res)] <- NaN
    res
}

## FIXME ! -- make sure we are following "the literature":
rstandard.glm <- function(model, infl = lm.influence(model, do.coef=FALSE), ...)
{
    res <- infl$wt.res # = "dev.res"  really
    res <- res / sqrt(summary(model)$dispersion * (1 - infl$hat))
    res[is.infinite(res)] <- NaN
    res
}

rstudent <- function(model, ...) UseMethod("rstudent")
rstudent.lm <- function(model, infl = lm.influence(model, do.coef=FALSE),
			res = infl$wt.res, ...)
{
    res <- res / (infl$sigma * sqrt(1 - infl$hat))
    res[is.infinite(res)] <- NaN
    res
}

rstudent.glm <- function(model, infl = influence(model, do.coef=FALSE), ...)
{
    r <- infl$dev.res
    r <- sign(r) * sqrt(r^2 + (infl$hat * infl$pear.res^2)/(1 - infl$hat))
    r[is.infinite(r)] <- NaN
    r
    if (any(family(model)$family == c("binomial", "poisson")))
	r else r/infl$sigma
}

### FIXME for glm (see above) ?!?
dffits <- function(model, infl = lm.influence(model, do.coef=FALSE),
		   res = weighted.residuals(model))
{
    res <- res * sqrt(infl$hat)/(infl$sigma*(1-infl$hat))
    res[is.infinite(res)] <- NaN
    res
}


dfbeta <- function(model, ...) UseMethod("dfbeta")

dfbeta.lm <- function(model, infl = lm.influence(model, do.coef=TRUE), ...)
{
    ## for lm & glm
    b <- infl$coefficients
    dimnames(b) <- list(names(infl$wt.res), variable.names(model))
    b
}

dfbetas <- function(model, ...) UseMethod("dfbetas")

dfbetas.lm <- function (model, infl = lm.influence(model, do.coef=TRUE), ...)
{
    ## for lm & glm
    xxi <- chol2inv(model$qr$qr, model$qr$rank)
    dfbeta(model, infl) / outer(infl$sigma, sqrt(diag(xxi)))
}

covratio <- function(model, infl = lm.influence(model, do.coef=FALSE),
		     res = weighted.residuals(model))
{
    n <- nrow(model$qr$qr)
    p <- model$rank
    omh <- 1-infl$hat
    e.star <- res/(infl$sigma*sqrt(omh))
    e.star[is.infinite(e.star)] <- NaN
    1/(omh*(((n - p - 1)+e.star^2)/(n - p))^p)
}

cooks.distance <- function(model, ...) UseMethod("cooks.distance")

## Used in plot.lm(); allow passing of known parts; `infl' used only via `hat'
cooks.distance.lm <-
function(model, infl = lm.influence(model, do.coef=FALSE),
	 res = weighted.residuals(model),
	 sd = sqrt(deviance(model)/df.residual(model)),
	 hat = infl$hat, ...)
{
    p <- model$rank
    res <- ((res/(sd * (1 - hat)))^2 * hat)/p
    res[is.infinite(res)] <- NaN
    res
}

cooks.distance.glm <-
function(model, infl = influence(model, do.coef=FALSE),
	 res = infl$pear.res,
	 dispersion = summary(model)$dispersion, hat = infl$hat, ...)
{
    p <- model$rank
    res <- (res/(1-hat))^2 * hat/(dispersion* p)
    res[is.infinite(res)] <- NaN
    res
}

## FIXME: The following probably needs partial adapation for glm
## -----  (whenever there's an extra glm method above!)

influence.measures <- function(model)
{
    is.influential <- function(infmat, n)
    {
	## Argument is result of using influence.measures
	## Returns a matrix  of logicals structured like the argument
	k <- ncol(infmat) - 4
	if(n <= k)
	    stop("too few cases, n < k")
	absmat <- abs(infmat)
	result <- cbind(absmat[, 1:k] > 1, # |dfbetas| > 1
			absmat[, k + 1] > 3 * sqrt(k/(n - k)), # |dffit| > ..
			abs(1 - infmat[, k + 2]) > (3*k)/(n - k),# |1-cov.r| >..
			pf(infmat[, k + 3], k, n - k) > 0.5,# "P[cook.d..]" > .5
			infmat[, k + 4] > (3 * k)/n) # hat > 3k/n
	dimnames(result) <- dimnames(infmat)
	result
    }
    infl <- lm.influence(model)
    p <- model$rank
    e <- weighted.residuals(model)
    s <- sqrt(sum(e^2, na.rm=TRUE)/df.residual(model))
    xxi <- chol2inv(model$qr$qr, model$qr$rank)
    si <- infl$sigma
    h <- infl$hat
    dfbetas <- infl$coefficients / outer(infl$sigma, sqrt(diag(xxi)))
    vn <- variable.names(model); vn[vn == "(Intercept)"] <- "1_"
    colnames(dfbetas) <- paste("dfb",abbreviate(vn),sep=".")
    dffits <- e*sqrt(h)/(si*(1-h))
    cov.ratio <- (si/s)^(2 * p)/(1 - h)
    cooks.d <- ((e/(s * (1 - h)))^2 * h)/p
#    dn <- dimnames(model$qr$qr)
    infmat <- cbind(dfbetas, dffit = dffits, cov.r = cov.ratio,
		    cook.d = cooks.d, hat=h)
    infmat[is.infinite(infmat)] <- NaN
    is.inf <- is.influential(infmat, sum(h > 0))
    ans <- list(infmat = infmat, is.inf = is.inf, call = model$call)
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
    ## will have NaN values from any hat=1 rows.
    is.inf[is.na(is.inf)] <- FALSE
     is.star <- apply(is.inf, 1, any)
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
