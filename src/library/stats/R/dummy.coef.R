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
    nxl <- rep.int(1, length(vars))
    names(nxl) <- vars
    tmp <- unlist(lapply(xl, length))
    nxl[names(tmp)] <- tmp
    lterms <- apply(facs, 2, function(x) prod(nxl[x > 0]))
    nl <- sum(lterms)
    args <- vector("list", length(vars))
    names(args) <- vars
    for(i in vars)
	args[[i]] <- if(nxl[[i]] == 1) rep.int(1, nl)
	else factor(rep.int(xl[[i]][1], nl), levels = xl[[i]])
    dummy <- do.call("data.frame", args)
    pos <- 0
    rn <- rep.int(tl, lterms)
    rnn <- rep.int("", nl)
    for(j in tl) {
	i <- vars[facs[, j] > 0]
	ifac <- i[nxl[i] > 1]
	if(length(ifac) == 0) {		# quantitative factor
	    rnn[pos+1] <- j
	} else if(length(ifac) == 1) {	# main effect
	    dummy[ pos+1:lterms[j], ifac ] <- xl[[ifac]]
	    rnn[ pos+1:lterms[j] ] <- as.character(xl[[ifac]])
	} else {			# interaction
	    tmp <- expand.grid(xl[ifac])
	    dummy[ pos+1:lterms[j], ifac ] <- tmp
	    rnn[ pos+1:lterms[j] ] <-
		apply(as.matrix(tmp), 1, function(x) paste(x, collapse=":"))
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
    coef <- object$coef
    if(!use.na) coef[is.na(coef)] <- 0
    asgn <- attr(mm,"assign")
    res <- vector("list", length(tl))
    names(res) <- tl
    for(j in seq(along=tl)) {
	keep <- asgn == j
	ans <- drop(mm[rn == tl[j], keep, drop=FALSE] %*% coef[keep])
	names(ans) <- rnn[rn == tl[j]]
	res[[j]] <- ans
    }
    if(int > 0) {
	res <- c(list(coef[int]), res)
	names(res)[1] <- "(Intercept)"
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
    nxl <- rep.int(1, length(vars))
    names(nxl) <- vars
    tmp <- unlist(lapply(xl, length))
    nxl[names(tmp)] <- tmp
    lterms <- apply(facs, 2, function(x) prod(nxl[x > 0]))
    nl <- sum(lterms)
    args <- vector("list", length(vars))
    names(args) <- vars
    for(i in vars)
	args[[i]] <- if(nxl[[i]] == 1) rep.int(1, nl)
	else factor(rep.int(xl[[i]][1], nl), levels = xl[[i]])
    dummy <- do.call("data.frame", args)
    pos <- 0
    rn <- rep.int(tl, lterms)
    rnn <- rep.int("", nl)
    for(j in tl) {
	i <- vars[facs[, j] > 0]
	ifac <- i[nxl[i] > 1]
	if(length(ifac) == 0) {		# quantitative factor
	    rnn[pos + 1] <- j
	} else if(length(ifac) == 1) {	# main effect
	    dummy[ pos+1:lterms[j], ifac ] <- xl[[ifac]]
	    rnn[ pos+1:lterms[j] ] <- as.character(xl[[ifac]])
	} else {			# interaction
	    tmp <- expand.grid(xl[ifac])
	    dummy[ pos+1:lterms[j], ifac ] <- tmp
	    rnn[ pos+1:lterms[j] ] <-
		apply(as.matrix(tmp), 1, function(x) paste(x, collapse=":"))
	}
	pos <- pos + lterms[j]
    }
    form <- paste("~", paste(tl, collapse = " + "))
    if (!int) form <- paste(form, "- 1")
    mm <- model.matrix(terms(formula(form)), dummy,
		       attr(object, "contrasts"), xl)
    res <- vector("list", length(object))
    names(res) <- names(object)
    tl <- c("(Intercept)", tl)
    allasgn <- attr(mm, "assign")
    for(i in names(object)) {
	coef <- object[[i]]$coef
	if(!use.na) coef[is.na(coef)] <- 0
	asgn <- object[[i]]$assign
	uasgn <- unique(asgn)
	tll <- tl[1 + uasgn]
	mod <- vector("list", length(tll))
	names(mod) <- tll
	for(j in uasgn) {
	    if(j == 0) {
		ans <- structure(coef[asgn == j], names="(Intercept)")
	    } else {
		ans <- drop(mm[rn == tl[1+j], allasgn == j, drop=FALSE] %*%
			    coef[asgn == j])
		names(ans) <- rnn[rn == tl[1+j]]
	    }
	    mod[[tl[1+j]]] <- ans
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
    nm <- max(sapply(x, length))
    ans <- matrix("", 2*n, nm)
    rn <- rep.int("", 2*n)
    line <- 0
    for (j in seq(n)) {
	this <- x[[j]]
	n1 <- length(this)
	if(n1 > 1) {
	    line <- line + 2
	    ans[line-1, 1:n1] <- names(this)
	    ans[line, 1:n1] <- format(this, ...)
	    rn[line-1] <- paste(terms[j], ":   ", sep="")
	} else {
	    line <- line + 1
	    ans[line, 1:n1] <- format(this, ...)
	    rn[line] <- paste(terms[j], ":   ", sep="")
	}
    }
    rownames(ans) <- rn
    colnames(ans) <- rep.int("", nm)
    cat(if(missing(title)) "Full coefficients are" else title, "\n")
    print(ans[1:line, , drop=FALSE], quote=FALSE, right=TRUE)
    invisible(x)
}

print.dummy_coef_list <- function(x, ...)
{
    for(strata in names(x))
	print.dummy_coef(x[[strata]], ..., title=paste("\n     Error:", strata))
    invisible(x)
}
