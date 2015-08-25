#  File src/library/stats/R/lsfit.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2013 The R Core Team
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  https://www.R-project.org/Licenses/

lsfit <- function(x, y, wt = NULL, intercept = TRUE, tolerance = 1e-07,
                  yname = NULL)
{
    ## find names of x variables (design matrix)

    x <- as.matrix(x)
    y <- as.matrix(y)
    xnames <- colnames(x)
    if( is.null(xnames) ) {
	if(ncol(x) == 1L) xnames <- "X"
	else xnames <- paste0("X", 1L:ncol(x))
    }
    if( intercept ) {
	x <- cbind(1, x)
	xnames <- c("Intercept", xnames)
    }

    ## find names of y variables (responses)

    if(is.null(yname) && ncol(y) > 1) yname <- paste0("Y", 1L:ncol(y))

    ## remove missing values

    good <- complete.cases(x, y, wt)
    dimy <- dim(as.matrix(y))
    if( any(!good) ) {
        warning(sprintf(ngettext(sum(!good),
                                 "%d missing value deleted",
                                 "%d missing values deleted"),
                        sum(!good)), domain = NA)
	x <- as.matrix(x)[good, , drop=FALSE]
	y <- as.matrix(y)[good, , drop=FALSE]
	wt <- wt[good]
    }

    ## check for compatible lengths

    nrx <- NROW(x)
    ncx <- NCOL(x)
    nry <- NROW(y)
    ncy <- NCOL(y)
    nwts <- length(wt)
    if(nry != nrx)
        stop(sprintf(paste0(ngettext(nrx,
                       "'X' matrix has %d case (row)",
                       "'X' matrix has %d cases (rows)"),
              ", ",
              ngettext(nry,
                       "'Y' has %d case (row)",
                       "'Y' has %d cases (rows)")),
                       nrx, nry),
                       domain = NA)
    if(nry < ncx)
        stop(sprintf(paste0(ngettext(nry,
                              "only %d case",
                              "only %d cases"),
                     ", ",
                     ngettext(ncx,
                              "but %d variable",
                              "but %d variables")),
                     nry, ncx),
             domain = NA)
    ## check weights if necessary
    if( !is.null(wt) ) {
	if(any(wt < 0)) stop("negative weights not allowed")
	if(nwts != nry)
            stop(gettextf("number of weights = %d should equal %d (number of responses)", nwts, nry), domain = NA)
	wtmult <- wt^0.5
	if(any(wt == 0)) {
	    xzero <- as.matrix(x)[wt == 0, ]
	    yzero <- as.matrix(y)[wt == 0, ]
	}
	x <- x*wtmult
	y <- y*wtmult
	invmult <- 1/ifelse(wt == 0, 1, wtmult)
    }

    # Here y is a matrix, so z$residuals and z$effects will be
    z <- .Call(C_Cdqrls, x, y, tolerance, FALSE)

    resids <- array(NA, dim = dimy)
    dim(z$residuals) <- c(nry, ncy)
    if(!is.null(wt)) {
	if(any(wt == 0)) {
	    if(ncx == 1L) fitted.zeros <- xzero * z$coefficients
	    else fitted.zeros <- xzero %*% z$coefficients
	    z$residuals[wt == 0, ] <- yzero - fitted.zeros
	}
	z$residuals <- z$residuals*invmult
    }
    resids[good, ] <- z$residuals
    if(dimy[2L] == 1 && is.null(yname)) {
	resids <- drop(resids)
	names(z$coefficients) <- xnames
    } else {
	colnames(resids) <- yname
	colnames(z$effects) <- yname
	dim(z$coefficients) <- c(ncx, ncy)
	dimnames(z$coefficients) <- list(xnames, yname)
    }
    z$qr <- as.matrix(z$qr)
    colnames(z$qr) <- xnames
    output <- list(coefficients = z$coefficients, residuals = resids)

    ## if X matrix was collinear, then the columns may have been
    ## pivoted hence xnames may need to be corrected

    if( z$rank != ncx ) {
	xnames <- xnames[z$pivot]
	dimnames(z$qr) <- list(NULL, xnames)
	warning("'X' matrix was collinear")
    }

    ## return weights if necessary

    if (!is.null(wt) ) {
	weights <- rep.int(NA, dimy[1L])
	weights[good] <- wt
	output <- c(output, list(wt=weights))
    }

    ## return rest of output

    ## Neither qt nor tol are documented to be there.
    rqr <- list(qt = drop(z$effects), qr = z$qr, qraux = z$qraux, rank = z$rank,
		pivot = z$pivot, tol = z$tol)
    oldClass(rqr) <- "qr"
    output <- c(output, list(intercept = intercept, qr = rqr))
    return(output)
}

ls.diag <- function(ls.out)
{
    resids <- as.matrix(ls.out$residuals)
    d0 <- dim(resids)
    xnames <- colnames(ls.out$qr$qr)
    yname <- colnames(resids)

    ## remove any missing values

    good <- complete.cases(resids, ls.out$wt)
    if( any(!good) ) {
	warning("missing observations deleted")
	resids <- resids[good, , drop = FALSE]
    }

    ## adjust residuals if needed

    if( !is.null(ls.out$wt) ) {
	if( any(ls.out$wt[good] == 0) )
	    warning("observations with 0 weight not used in calculating standard deviation")
	resids <- resids * ls.out$wt[good]^0.5
    }

    ## initialize

    p <- ls.out$qr$rank
    n <- nrow(resids)
    hatdiag <- rep.int(NA, n)
    stats <- array(NA, dim = d0)
    colnames(stats) <- yname
    stdres <- studres <- dfits <- Cooks <- stats

    ## calculate hat matrix diagonals

    q <- qr.qy(ls.out$qr, rbind(diag(p), matrix(0, nrow=n-p, ncol=p)))
    hatdiag[good] <- rowSums(as.matrix(q^2))

    ## calculate diagnostics

    stddev <- (colSums(as.matrix(resids^2))/(n - p))^0.5
    stddevmat <- matrix(stddev, nrow=sum(good), ncol=ncol(resids), byrow=TRUE)
    stdres[good, ] <- resids/((1-hatdiag[good])^0.5 * stddevmat)
    studres[good, ] <- (stdres[good, ]*stddevmat)/
        (((n-p)*stddevmat^2 - resids^2/(1-hatdiag[good]))/(n-p-1))^0.5
    dfits[good, ] <- (hatdiag[good]/(1-hatdiag[good]))^0.5 * studres[good, ]
    Cooks[good, ] <- ((stdres[good, ]^2 * hatdiag[good])/p)/(1-hatdiag[good])
    if(ncol(resids)==1 && is.null(yname)) {
	stdres <- as.vector(stdres)
	Cooks <- as.vector(Cooks)
	studres <- as.vector(studres)
	dfits <- as.vector(dfits)
    }

    ## calculate unscaled covariance matrix

    qr <- as.matrix(ls.out$qr$qr[1L:p, 1L:p])
    qr[row(qr)>col(qr)] <- 0
    qrinv <- solve(qr)
    covmat.unscaled <- qrinv%*%t(qrinv)
    dimnames(covmat.unscaled) <- list(xnames, xnames)

    ## calculate scaled covariance matrix

    covmat.scaled <- sum(stddev^2) * covmat.unscaled

    ## calculate correlation matrix

    cormat <- covmat.scaled/
	(outer(diag(covmat.scaled), diag(covmat.scaled))^0.5)

    ## calculate standard error

    stderr <- outer(diag(covmat.unscaled)^0.5, stddev)
    dimnames(stderr) <- list(xnames, yname)

    return(list(std.dev=stddev, hat=hatdiag, std.res=stdres,
		stud.res=studres, cooks=Cooks, dfits=dfits,
		correlation=cormat, std.err=stderr,
		cov.scaled=covmat.scaled, cov.unscaled=covmat.unscaled))
}

ls.print <- function(ls.out, digits = 4L, print.it = TRUE)
{
    ## calculate residuals to be used

    resids <- as.matrix(ls.out$residuals)
    if( !is.null(ls.out$wt) ) {
	if(any(ls.out$wt == 0))
	    warning("observations with 0 weights not used")
	resids <- resids * ls.out$wt^0.5
    }
    n <- apply(resids, 2L, length) - colSums(is.na(resids))
    lsqr <- ls.out$qr
    p <- lsqr$rank

    ## calculate total sum sq and df

    if(ls.out$intercept) {
	if(is.matrix(lsqr$qt))
	    totss <- colSums(lsqr$qt[-1L, ]^2)
	else totss <- sum(lsqr$qt[-1L]^2)
	degfree <- p - 1
    } else {
	totss <- colSums(as.matrix(lsqr$qt^2))
	degfree <- p
    }

    ## calculate residual sum sq and regression sum sq

    resss <- colSums(resids^2, na.rm=TRUE)
    resse <- (resss/(n-p))^.5
    regss <- totss - resss
    rsquared <- regss/totss
    fstat <- (regss/degfree)/(resss/(n-p))
    pvalue <- pf(fstat, degfree, (n-p), lower.tail = FALSE)

    ## construct summary

    Ynames <- colnames(resids)
    summary <- cbind(format(round(resse, digits)),
		     format(round(rsquared, digits)),
		     format(round(fstat, digits)),
		     format(degfree),
		     format(n-p),
		     format(round(pvalue, digits)))
    dimnames(summary) <- list(Ynames,
			      c("Mean Sum Sq", "R Squared",
				"F-value", "Df 1", "Df 2", "Pr(>F)"))
    mat <- as.matrix(lsqr$qr[1L:p, 1L:p])
    mat[row(mat)>col(mat)] <- 0
    qrinv <- solve(mat)

    ## construct coef table

    m.y <- ncol(resids)
    coef.table <- as.list(1L:m.y)
    if(m.y==1) coef <- matrix(ls.out$coefficients, ncol=1)
    else coef <- ls.out$coefficients
    for(i in 1L:m.y) {
	covmat <- (resss[i]/(n[i]-p)) * (qrinv%*%t(qrinv))
	se <- diag(covmat)^.5
	coef.table[[i]] <- cbind(coef[, i], se, coef[, i]/se,
				 2*pt(abs(coef[, i]/se), n[i]-p,
                                      lower.tail = FALSE))
	dimnames(coef.table[[i]]) <-
	    list(colnames(lsqr$qr),
		 c("Estimate", "Std.Err", "t-value", "Pr(>|t|)"))

	##-- print results --

	if(print.it) {
	    if(m.y>1)
		cat("Response:", Ynames[i], "\n\n")
	    cat(paste("Residual Standard Error=",
                      format(round(resse[i], digits)), "\nR-Square=",
                      format(round(rsquared[i], digits)), "\nF-statistic (df=",
		      format(degfree), ", ", format(n[i]-p), ")=",
		      format(round(fstat[i], digits)), "\np-value=",
		      format(round(pvalue[i], digits)), "\n\n", sep=""))
	    print(round(coef.table[[i]], digits))
	    cat("\n\n")
	}
    }
    names(coef.table) <- Ynames

    invisible(list(summary = summary, coef.table = coef.table))
}
