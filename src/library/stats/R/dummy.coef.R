#  File src/library/stats/R/dummy.coef.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1998 B. D. Ripley
#  Copyright (C) 1998-2016 The R Core Team
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

dummy.coef <- function(object, ...) UseMethod("dummy.coef")

dummy.coef.lm <- function(object, use.na=FALSE, ...)
{
    xl <- object$xlevels
    if(!length(xl)) # no factors in model
	return(as.list(coef(object)))
    Terms <- terms(object)
    tl <- attr(Terms, "term.labels")
    int <- attr(Terms, "intercept")
    facs <- attr(Terms, "factors")[-1, , drop=FALSE]
    Terms <- delete.response(Terms)
    mf <- object$model
    if (is.null(mf)) mf <- model.frame(object)
    vars <- dimnames(facs)[[1]] # names
    xtlv <- lapply(mf[,vars, drop=FALSE], levels) ## levels
    nxl <- pmax(lengths(xtlv), 1L)  ## (named) number of levels
    lterms <- apply(facs, 2L, function(x) prod(nxl[x > 0]))
    nl <- sum(lterms)
    ## dummy: data frame of vars
    args <- sapply(vars, function(i)
	if (nxl[i] == 1) rep.int(1, nl)
	else factor(rep.int(xtlv[[i]][1L], nl), levels = xtlv[[i]]),
	simplify=FALSE)
    ## dummy <- as.data.frame(args) # slightly more efficiently:
    dummy <- do.call(data.frame, args); names(dummy) <- vars
    pos <- 0L
    rn <- rep.int(tl, lterms)
    rnn <- character(nl) # all "" --- will be names of rows
    for(j in tl) {
	i <- vars[facs[, j] > 0]
	ifac <- i[nxl[i] > 1]
	lt.j <- lterms[[j]]
	if(length(ifac) == 0L) {        # quantitative factor
	    rnn[pos+1L] <- j
	} else {
	    p.j <- pos + seq_len(lt.j)
	    if(length(ifac) == 1L) {	# main effect
		dummy[p.j, ifac] <- x.i <- xtlv[[ifac]]
		rnn[p.j] <- as.character(x.i)
	    } else {			# interaction
		tmp <- expand.grid(xtlv[ifac], KEEP.OUT.ATTRS=FALSE)
		dummy[p.j, ifac] <- tmp
		rnn[p.j] <- apply(as.matrix(tmp), 1L, paste, collapse = ":")
	    }
	}
	pos <- pos + lt.j
    }
    attr(dummy,"terms") <- attr(mf,"terms")
    lcontr <- object$contrasts
    lci <- vapply(dummy, is.factor, NA)
    lcontr <- lcontr[names(lci)[lci]] ## factors with 1 level have disappeared (?)
    mm <- model.matrix(Terms, dummy, lcontr, xl)
    if(anyNA(mm)) {
        warning("some terms will have NAs due to the limits of the method")
        mm[is.na(mm)] <- NA
    }
    coef <- object$coefficients
    if(!use.na) coef[is.na(coef)] <- 0
    asgn <- attr(mm,"assign")
    res <- setNames(vector("list", length(tl)), tl)
    for(j in seq_along(tl)) {
         keep <- which(asgn == j)
         cf <- coef[keep]
         ij <- rn == tl[j]
         res[[j]] <-
	     if (any(na <- is.na(cf))) {
		 rj <- setNames(drop(mm[ij, keep[!na], drop = FALSE] %*%
				      cf[!na]), rnn[ij])
		 rj[apply(mm[ij, keep[na], drop=FALSE] != 0, 1L, any)] <- NA
		 rj
	     } else
		 setNames(drop(mm[ij, keep, drop = FALSE] %*% cf), rnn[ij])
    }
    if(int > 0) {
	res <- c(list("(Intercept)" = coef[int]), res)
    }
    class(res) <- "dummy_coef"
    res
}

dummy.coef.aovlist <- function(object, use.na = FALSE, ...)
{
    xl <- attr(object, "xlevels")
    if(!length(xl)) # no factors in model
	return(as.list(coef(object)))
    Terms <- terms(object, specials="Error")
    err <- attr(Terms,"specials")$Error - 1
    tl <- attr(Terms, "term.labels")[-err]
    int <- attr(Terms, "intercept")
    facs <- attr(Terms, "factors")[-c(1,1+err), -err, drop=FALSE]
    mf <- object$model
    if (is.null(mf)) mf <- model.frame(object)
    vars <- dimnames(facs)[[1]] # names
    xtlv <- lapply(mf[,vars, drop=FALSE], levels) ## levels
    nxl <- pmax(lengths(xtlv), 1L)  ## (named) number of levels
    lterms <- apply(facs, 2L, function(x) prod(nxl[x > 0]))
    nl <- sum(lterms)
    args <- setNames(vector("list", length(vars)), vars)
    for(i in vars)
	args[[i]] <- if(nxl[[i]] == 1) rep.int(1, nl)
                     else factor(rep.int(xl[[i]][1L], nl), levels = xl[[i]])
    ## dummy <- as.data.frame(args) # slightly more efficiently:
    dummy <- do.call(data.frame, args); names(dummy) <- vars
    pos <- 0L
    rn <- rep.int(tl, lterms)
    rnn <- character(nl) # all "" --- will be names of rows
    for(j in tl) {
	i <- vars[facs[, j] > 0]
	ifac <- i[nxl[i] > 1]
	lt.j <- lterms[[j]]
	if(length(ifac) == 0L) {        # quantitative factor
	    rnn[pos+1L] <- j
	} else {
	    p.j <- pos + seq_len(lt.j)
	    if(length(ifac) == 1L) {	# main effect
		dummy[p.j, ifac] <- x.i <- xtlv[[ifac]]
		rnn[p.j] <- as.character(x.i)
	    } else {			# interaction
		tmp <- expand.grid(xtlv[ifac], KEEP.OUT.ATTRS=FALSE)
		dummy[p.j, ifac] <- tmp
		rnn[p.j] <- apply(as.matrix(tmp), 1L, paste, collapse = ":")
	    }
        }
	pos <- pos + lt.j
    }
    form <- paste0("~", paste0(tl, collapse = " + "), if(!int) "- 1")
    lcontr <- object$contrasts
    lci <- vapply(dummy, is.factor, NA)
    lcontr <- lcontr[names(lci)[lci]] ## factors with 1 level have disappeared
    mm <- model.matrix(terms(formula(form)), dummy, lcontr, xl)
    tl <- c("(Intercept)", tl)
    res <- setNames(vector("list", length(object)), names(object))
    allasgn <- attr(mm, "assign")
    for(i in names(object)) {
	coef <- object[[i]]$coefficients
	if(!use.na) coef[is.na(coef)] <- 0
	asgn <- object[[i]]$assign
	uasgn <- unique(asgn)
	tll <- tl[1L + uasgn]
	mod <- setNames(vector("list", length(tll)), tll)
	for(j in uasgn) {
	    keep <- which(asgn == j)
	    cf <- coef[keep]
	    mod[[tl[j+1L]]] <-
		if(j == 0) {
		    structure(cf, names="(Intercept)")
		} else {
		    ij <- rn == tl[j+1L]
		    if (any(na <- is.na(cf))) {
			rj <- setNames(drop(mm[ij, keep[!na], drop = FALSE] %*%
					    cf[!na]), rnn[ij])
			rj[apply(mm[ij, keep[na], drop=FALSE] != 0, 1L, any)] <- NA
			rj
		    } else
			setNames(drop(mm[ij, allasgn == j, drop=FALSE] %*% cf), rnn[ij])
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
    nm <- max(lengths(x))
    ans <- matrix("", 2L*n, nm)
    rn <- character(2L*n) # ""
    line <- 0L
    for (j in seq_len(n)) {
	this <- x[[j]]
	n1 <- length(this)
	if(n1 > 1) {
	    line <- line + 2L
	    ans[line-1L, 1L:n1] <- names(this)
	    ans[line,    1L:n1] <- format(this, ...)
	    rn [line-1L] <- paste0(terms[j], ":   ")
	} else {
	    line <- line + 1L
	    ans[line, 1L:n1] <- format(this, ...)
	    rn[line] <- paste0(terms[j], ":   ")
	}
    }
    dimnames(ans) <- list(rn, character(nm))
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
