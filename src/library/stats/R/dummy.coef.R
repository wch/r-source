#  File src/library/stats/R/dummy.coef.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1998 B. D. Ripley
#  Copyright (C) 1998-2012 The R Core Team
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
#  http://www.r-project.org/Licenses/

dummy.coef <- function(object, ...) UseMethod("dummy.coef")

dummy.coef.lm <- function(object, use.na=FALSE, ...)
{
    Terms <- terms(object)
    tl <- attr(Terms, "term.labels")
    int <- attr(Terms, "intercept")
    facs <- attr(Terms, "factors")[-1, , drop=FALSE]
    Terms <- delete.response(Terms)
    vars <- all.vars(Terms)
    xl <- object$xlevels
    if(!length(xl)) {			# no factors in model
	return(as.list(coef(object)))
    }
    nxl <- setNames(rep.int(1, length(vars)), vars)
    tmp <- unlist(lapply(xl, length)) ## ?? vapply(xl, length, 1L)
    nxl[names(tmp)] <- tmp
    lterms <- apply(facs, 2L, function(x) prod(nxl[x > 0]))
    nl <- sum(lterms)
    args <- setNames(vector("list", length(vars)), vars)
    for(i in vars)
	args[[i]] <- if(nxl[[i]] == 1) rep.int(1, nl)
	else factor(rep.int(xl[[i]][1L], nl), levels = xl[[i]])
    dummy <- do.call("data.frame", args)
    pos <- 0
    rn <- rep.int(tl, lterms)
    rnn <- rep.int("", nl)
    for(j in tl) {
	i <- vars[facs[, j] > 0]
	ifac <- i[nxl[i] > 1]
	if(length(ifac) == 0L) {        # quantitative factor
	    rnn[pos+1] <- j
	} else if(length(ifac) == 1L) {	# main effect
	    dummy[ pos+1L:lterms[j], ifac ] <- xl[[ifac]]
	    rnn[ pos+1L:lterms[j] ] <- as.character(xl[[ifac]])
	} else {			# interaction
	    tmp <- expand.grid(xl[ifac])
	    dummy[ pos+1L:lterms[j], ifac ] <- tmp
	    rnn[ pos+1L:lterms[j] ] <-
		apply(as.matrix(tmp), 1L, function(x) paste(x, collapse=":"))
	}
	pos <- pos + lterms[j]
    }
    ## some terms like poly(x,1) will give problems here, so allow
    ## NaNs and set to NA afterwards.
    mf <- model.frame(Terms, dummy, na.action=function(x)x, xlev=xl)
    mm <- model.matrix(Terms, mf, object$contrasts, xl)
    if(any(is.na(mm))) {
        warning("some terms will have NAs due to the limits of the method")
        mm[is.na(mm)] <- NA
    }
    coef <- object$coefficients
    if(!use.na) coef[is.na(coef)] <- 0
    asgn <- attr(mm,"assign")
    res <- setNames(vector("list", length(tl)), tl)
    for(j in seq_along(tl)) {
	keep <- asgn == j
	ij <- rn == tl[j]
	res[[j]] <-
	    setNames(drop(mm[ij, keep, drop=FALSE] %*% coef[keep]), rnn[ij])
    }
    if(int > 0) {
	res <- c(list("(Intercept)" = coef[int]), res)
    }
    class(res) <- "dummy_coef"
    res
}

dummy.coef.aovlist <- function(object, use.na = FALSE, ...)
{
    Terms <- terms(object, specials="Error")
    err <- attr(Terms,"specials")$Error - 1
    tl <- attr(Terms, "term.labels")[-err]
    int <- attr(Terms, "intercept")
    facs <- attr(Terms, "factors")[-c(1,1+err), -err, drop=FALSE]
    vars <- rownames(facs)
    xl <- attr(object, "xlevels")
    if(!length(xl)) {			# no factors in model
	return(as.list(coef(object)))
    }
    nxl <- setNames(rep.int(1, length(vars)), vars)
    tmp <- unlist(lapply(xl, length))
    nxl[names(tmp)] <- tmp
    lterms <- apply(facs, 2L, function(x) prod(nxl[x > 0]))
    nl <- sum(lterms)
    args <- setNames(vector("list", length(vars)), vars)
    for(i in vars)
	args[[i]] <- if(nxl[[i]] == 1) rep.int(1, nl)
	else factor(rep.int(xl[[i]][1L], nl), levels = xl[[i]])
    dummy <- do.call("data.frame", args)
    pos <- 0
    rn <- rep.int(tl, lterms)
    rnn <- rep.int("", nl)
    for(j in tl) {
	i <- vars[facs[, j] > 0]
	ifac <- i[nxl[i] > 1]
	if(length(ifac) == 0L) {        # quantitative factor
	    rnn[pos + 1] <- j
	} else if(length(ifac) == 1L) {	# main effect
	    dummy[ pos+1L:lterms[j], ifac ] <- xl[[ifac]]
	    rnn[ pos+1L:lterms[j] ] <- as.character(xl[[ifac]])
	} else {			# interaction
	    tmp <- expand.grid(xl[ifac])
	    dummy[ pos+1L:lterms[j], ifac ] <- tmp
	    rnn[ pos+1L:lterms[j] ] <-
		apply(as.matrix(tmp), 1L, function(x) paste(x, collapse=":"))
	}
	pos <- pos + lterms[j]
    }
    form <- paste("~", paste(tl, collapse = " + "))
    if (!int) form <- paste(form, "- 1")
    mm <- model.matrix(terms(formula(form)), dummy,
		       attr(object, "contrasts"), xl)
    tl <- c("(Intercept)", tl)
    res <- setNames(vector("list", length(object)), names(object))
    allasgn <- attr(mm, "assign")
    for(i in names(object)) {
	coef <- object[[i]]$coefficients
	if(!use.na) coef[is.na(coef)] <- 0
	asgn <- object[[i]]$assign
	uasgn <- unique(asgn)
	tll <- tl[1 + uasgn]
	mod <- setNames(vector("list", length(tll)), tll)
	for(j in uasgn) {
	    mod[[tl[1+j]]] <-
		if(j == 0) {
		    structure(coef[asgn == j], names="(Intercept)")
		} else {
		    ij <- rn == tl[1+j]
		    setNames(drop(mm[ij, allasgn == j, drop=FALSE] %*%
				  coef[asgn == j]),
			     rnn[ij])
		}
	}
	res[[i]] <- mod
    }
    class(res) <- "dummy_coef_list"
    res
}

print.dummy_coef <- function(x, ..., title)
{
    terms <- names(x)
    n <- length(x)
    nm <- max(vapply(x, length, 1L))
    ans <- matrix("", 2L*n, nm)
    rn <- rep.int("", 2L*n)
    line <- 0
    for (j in seq_len(n)) {
	this <- x[[j]]
	n1 <- length(this)
	if(n1 > 1) {
	    line <- line + 2
	    ans[line-1, 1L:n1] <- names(this)
	    ans[line, 1L:n1] <- format(this, ...)
	    rn[line-1] <- paste0(terms[j], ":   ")
	} else {
	    line <- line + 1
	    ans[line, 1L:n1] <- format(this, ...)
	    rn[line] <- paste0(terms[j], ":   ")
	}
    }
    rownames(ans) <- rn
    colnames(ans) <- rep.int("", nm)
    cat(if(missing(title)) "Full coefficients are" else title, "\n")
    print(ans[1L:line, , drop=FALSE], quote=FALSE, right=TRUE)
    invisible(x)
}

print.dummy_coef_list <- function(x, ...)
{
    for(strata in names(x))
	print.dummy_coef(x[[strata]], ..., title=paste("\n     Error:", strata))
    invisible(x)
}
