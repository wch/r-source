lsfit <- function(x, y, wt=NULL, intercept=TRUE, tolerance=1e-07, yname=NULL)
{
    ## find names of x variables (design matrix)

    x <- as.matrix(x)
    y <- as.matrix(y)
    xnames <- colnames(x)
    if( is.null(xnames) ) {
	if(ncol(x)==1) xnames <- "X"
	else xnames <- paste("X", 1:ncol(x), sep="")
    }
    if( intercept ) {
	x <- cbind(1, x)
	xnames <- c("Intercept", xnames)
    }

    ## find names of y variables (responses)

    if(is.null(yname) && ncol(y) > 1) yname <- paste("Y", 1:ncol(y), sep="")

    ## remove missing values

    good <- complete.cases(x, y, wt)
    dimy <- dim(as.matrix(y))
    if( any(!good) ) {
	warning(paste(sum(!good), "missing values deleted"))
	x <- as.matrix(x)[good, ]
	y <- as.matrix(y)[good, ]
	wt <- wt[good]
    }

    ## check for compatible lengths

    nrx <- NROW(x)
    ncx <- NCOL(x)
    nry <- NROW(y)
    ncy <- NCOL(y)
    nwts <- length(wt)
    if(nry != nrx) stop(paste("X matrix has", nrx, "responses, Y",
       "has", nry, "responses."))
    if(nry < ncx) stop(paste(nry, "responses, but only", ncx, "variables"))

    ## check weights if necessary

    if( !is.null(wt) ) {
	if(any(wt < 0)) stop("negative weights not allowed")
	if(nwts != nry) stop(paste("Number of weights =", nwts,
	   ", should equal", nry, "(number of responses)"))
	wtmult <- wt^0.5
	if( any(wt==0) ) {
	    xzero <- as.matrix(x)[wt==0, ]
	    yzero <- as.matrix(y)[wt==0, ]
	}
	x <- x*wtmult
	y <- y*wtmult
	invmult <- 1/ifelse(wt==0, 1, wtmult)
    }

    ## call linpack

    storage.mode(x) <- "double"
    storage.mode(y) <- "double"
    z <- .Fortran("dqrls",
		  qr=x,
		  n=nrx,
		  p=ncx,
		  y=y,
		  ny=ncy,
		  tol=tolerance,
		  coefficients=mat.or.vec(ncx, ncy),
		  residuals=mat.or.vec(nrx, ncy),
		  effects=mat.or.vec(nrx, ncy),
		  rank=integer(1),
		  pivot=as.integer(1:ncx),
		  qraux=double(ncx),
		  work=double(2*ncx),
                  PACKAGE="base")

    ## dimension and name output from linpack

    resids <- array(NA, dim=dimy)
    dim(z$residuals) <- c(nry, ncy)
    if(!is.null(wt)) {
	if(any(wt==0)) {
	    if(ncx==1) fitted.zeros <- xzero * z$coefficients
	    else fitted.zeros <- xzero %*% z$coefficients
	    z$residuals[wt==0, ] <- yzero - fitted.zeros
	}
	z$residuals <- z$residuals*invmult
    }
    resids[good, ] <- z$residuals
    if(dimy[2] == 1 && is.null(yname)) {
	resids <- as.vector(resids)
	names(z$coefficients) <- xnames
    }
    else {
	colnames(resids) <- yname
	colnames(z$effects) <- yname
	dim(z$coefficients) <- c(ncx, ncy)
	dimnames(z$coefficients) <- list(xnames, yname)
    }
    z$qr <- as.matrix(z$qr)
    colnames(z$qr) <- xnames
    output <- list(coefficients=z$coefficients, residuals=resids)

    ## if X matrix was collinear, then the columns would have been
    ## pivoted hence xnames need to be corrected

    if( z$rank != ncx ) {
	xnames <- xnames[z$pivot]
	dimnames(z$qr) <- list(NULL, xnames)
	warning("X matrix was collinear")
    }

    ## return weights if necessary

    if (!is.null(wt) ) {
	weights <- rep.int(NA, dimy[1])
	weights[good] <- wt
	output <- c(output, list(wt=weights))
    }

    ## return rest of output

    rqr <- list(qt=z$effects, qr=z$qr, qraux=z$qraux, rank=z$rank,
		pivot=z$pivot, tol=z$tol)
    oldClass(rqr) <- "qr"
    output <- c(output, list(intercept=intercept, qr=rqr))
    return(output)
}

ls.diag <- function(ls.out)
{
    resids <- as.matrix(ls.out$residuals)
    xnames <- colnames(ls.out$qr$qr)
    yname <- colnames(resids)

    ## remove any missing values

    good <- complete.cases(resids, ls.out$wt)
    if( any(!good) ) {
	warning("missing observations deleted")
	resids <- as.matrix(resids)[good, ]
    }

    ## adjust residuals if needed

    if( !is.null(ls.out$wt) ) {
	if( any(ls.out$wt[good] == 0) )
	    warning(paste("observations with 0 weight not used in",
			  "calculating standard deviation"))
	resids <- resids * ls.out$wt[good]^0.5
    }

    ## initialize

    p <- ls.out$qr$rank
    n <- nrow(resids)
    hatdiag <- rep.int(NA, n)
    stats <- array(NA, dim = dim(resids))
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

    qr <- as.matrix(ls.out$qr$qr[1:p, 1:p])
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

ls.print <- function(ls.out, digits=4, print.it=TRUE)
{
    ## calculate residuals to be used

    resids <- as.matrix(ls.out$residuals)
    if( !is.null(ls.out$wt) ) {
	if(any(ls.out$wt == 0))
	    warning("Observations with 0 weights not used")
	resids <- resids * ls.out$wt^0.5
    }
    n <- apply(resids, 2, length)-colSums(is.na(resids))
    lsqr <- ls.out$qr
    p <- lsqr$rank

    ## calculate total sum sq and df

    if(ls.out$intercept) {
	if(is.matrix(lsqr$qt))
	    totss <- colSums(lsqr$qt[-1, ]^2)
	else totss <- sum(lsqr$qt[-1]^2)
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
    mat <- as.matrix(lsqr$qr[1:p, 1:p])
    mat[row(mat)>col(mat)] <- 0
    qrinv <- solve(mat)

    ## construct coef table

    m.y <- ncol(resids)
    coef.table <- as.list(1:m.y)
    if(m.y==1) coef <- matrix(ls.out$coef, nc=1)
    else coef <- ls.out$coef
    for(i in 1:m.y) {
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

    invisible(list(summary=summary, coef.table=coef.table))
}
