#  File src/library/stats/R/model.tables.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright     1998 B. D. Ripley
#  Copyright (C) 1998-2015 The R Core Team
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

model.tables <- function(x, ...) UseMethod("model.tables")

model.tables.aov <- function(x, type = "effects", se = FALSE, cterms, ...)
{
    if(inherits(x, "maov"))
	stop("'model.tables' is not implemented for multiple responses")
    type <- match.arg(type, c("effects", "means", "residuals"))
    if(type == "residuals")
	stop(gettextf("type '%s' is not implemented yet", type), domain = NA)
    prjs <- proj(x, unweighted.scale = TRUE)
    if(is.null(x$call)) stop("this fit does not inherit from \"lm\"")
    mf <- model.frame(x)
    factors <- attr(prjs, "factors")
    nf <- names(factors)
    dn.proj <- setNames(as.list(nf), nf)
    m.factors <- factors
    t.factor <- attr(prjs, "t.factor")
    vars <- colnames(t.factor)
    which <- match(vars, names(dn.proj))
    which <- which[!is.na(which)]
    dn.proj <- dn.proj[which]
    m.factors <- m.factors[which]
    ## with cterms, can specify subset of tables by name
    if(!missing(cterms)) {
	if(any(is.na(match(cterms, names(factors)))))
	    stop("'cterms' argument must match terms in model object")
	dn.proj <- dn.proj[cterms]
	m.factors <- m.factors[cterms]
    }
    if(type == "means") {
	dn.proj <-
	    lapply(dn.proj,
		   function(x, mat, vn)
		   c("(Intercept)",
		     vn[(t(mat) %*% (as.logical(mat[, x]) - 1)) == 0]),
		   t.factor, vars)
    }
    tables <- make.tables.aovproj(dn.proj, m.factors, prjs, mf)

    ## This was reordering some interaction terms, e.g. N + V:N
    ##n <- replications(paste("~", paste(names(tables), collapse = "+")),
    ##		      data = mf)
    n <- NULL
    for(xx in names(tables)) n <- c(n, replications(paste("~", xx), data=mf))
    if(se)
	if(is.list(n)) {
	    message("Design is unbalanced - use se.contrast() for se's")
	    se <- FALSE
	} else se.tables <- se.aov(x, n, type = type)
    if(type == "means" && "(Intercept)" %in% colnames(prjs)) {
	gmtable <- mean(prjs[,"(Intercept)"])
	class(gmtable) <- "mtable"
	tables <- c("Grand mean" = gmtable, tables)
    }
    result <- list(tables = tables, n = n)
    if(se) result$se <- se.tables
    attr(result, "type") <- type
    class(result) <- c("tables_aov", "list.of")
    result
}

se.aov <- function(object, n, type = "means")
{
    ## for balanced designs only
    rdf <- object$df.residual
    rse <- sqrt(sum(object$residuals^2)/rdf)
    if(type == "effects") result <- rse/sqrt(n)
    if(type == "means")
	result <-
	    lapply(n,
		   function(x, d) {
		       nn <- unique(x)
		       nn <- nn[!is.na(nn)]
		       mat <- outer(nn, nn, function(x, y) 1/x + 1/y)
		       dimnames(mat) <- list(paste(nn), paste(nn))
		       d * sqrt(mat)
		   }, d=rse)
    attr(result, "type") <- type
    class(result) <- "mtable"
    result
}


model.tables.aovlist <- function(x, type = "effects", se = FALSE, ...)
{
    type <- match.arg(type, c("effects", "means", "residuals"))
    if(type == "residuals")
	stop(gettextf("type '%s' is not implemented yet", type), domain = NA)
    prjs <- proj(x, unweighted.scale = TRUE)
    mf <- model.frame.aovlist(x)
    factors <- lapply(prjs, attr, "factors")
    dn.proj <- unlist(lapply(factors, names), recursive = FALSE)
    m.factors <- unlist(factors, recursive = FALSE)
    dn.strata <- rep.int(names(factors), lengths(factors))
    names(dn.strata) <- names(m.factors) <- names(dn.proj) <- unlist(dn.proj)
    t.factor <- attr(prjs, "t.factor")
    efficiency <- FALSE
    if(type == "effects" || type == "means") {
	if(anyDuplicated(names(dn.proj)[names(dn.proj) != "Residuals"])) {
	    efficiency <- eff.aovlist(x)
	    ## Elect to use the effects from the lowest stratum:
	    ##	usually expect this to be highest efficiency
	    eff.used <- apply(efficiency, 2L,
			      function(x, ind = seq_len(x)) {
				  temp <- (x > 0)
				  if(sum(temp) == 1) temp
				  else max(ind[temp]) == ind
			      })
	}
    }
    if(any(efficiency)) {
        if(is.list(eff.used))
            stop("design is unbalanced so cannot proceed")
	which <- match(outer(rownames(efficiency),
			     colnames(efficiency), paste)[eff.used],
		       paste(dn.strata, dn.proj))
	efficiency <- efficiency[eff.used]
    } else  which <- match(colnames(t.factor), names(dn.proj))
    which <- which[!is.na(which)]
    dn.proj <- dn.proj[which]
    dn.strata <- dn.strata[which]
    m.factors <- m.factors[which]
    if(type == "means")	 {
	t.factor <- t.factor[, names(dn.proj), drop = FALSE]
	dn.proj <-
	    lapply(dn.proj,
		   function(x, mat, vn)
		   vn[(t(mat) %*% (as.logical(mat[, x]) - 1)) == 0],
		   t.factor, colnames(t.factor))
    }
    tables <-
	if(any(efficiency)) {
	    names(efficiency) <- names(dn.proj)
	    make.tables.aovprojlist(dn.proj, dn.strata, m.factors, prjs, mf,
				    efficiency)
	}
	else make.tables.aovprojlist(dn.proj, dn.strata, m.factors, prjs, mf)
    if(type == "means") {
	gmtable <- mean(prjs[["(Intercept)"]])
	class(gmtable) <- "mtable"
	tables <- lapply(tables, "+", gmtable)
	tables <- c("Grand mean" = gmtable, tables)
    }
#    n <- replications(attr(x, "call"), data = mf)
    n <- replications(terms(x), data = mf)
    if(se)
	if(type == "effects"  && is.list(n)) {
	    message("Standard error information not returned as design is unbalanced. \nStandard errors can be obtained through 'se.contrast'.")
	    se <- FALSE
	} else if(type != "effects") {
	    warning(gettextf("SEs for type '%s' are not yet implemented",
                             type), domain = NA)
	    se <- FALSE
	} else {
	    se.tables <- se.aovlist(x, dn.proj, dn.strata, factors, mf,
				    efficiency, n, type = type)
	}
    result <- list(tables = tables, n = n)
    if(se) result$se <- se.tables
    attr(result, "type") <- type
    class(result) <- c("tables_aov", "list.of")
    result
}

se.aovlist <- function(object, dn.proj, dn.strata, factors, mf, efficiency, n,
		       type = "diff.means", ...)
{
    if(type != "effects")
	stop(gettextf("SEs for type '%s' are not yet implemented", type),
             domain = NA)
    RSS <- sapply(object, function(x) sum(x$residuals^2)/x$df.residual)
    res <- vector(length = length(n), mode = "list")
    names(res) <- names(n)
    for(i in names(n)) {
	sse <- RSS[[dn.strata[dn.proj[[i]]]]]
	if(any(efficiency))
	    sse <- sse/efficiency[i]
	res[[i]] <- as.vector(sqrt(sse/n[i]))
	class(res[[i]]) <- "mtable"
    }
    attr(res, "type") <- type
    res
}


make.tables.aovproj <-
    function(proj.cols, mf.cols, prjs, mf, fun = "mean", prt = FALSE, ...)
{
    tables <- setNames(vector("list", length(proj.cols)), names(proj.cols))
    for(i in seq_along(tables)) {
	terms <- proj.cols[[i]]
        terms <- terms[terms %in% colnames(prjs)]
	data <-
	    if(length(terms) == 1L) prjs[, terms]
	    else prjs[, terms] %*% as.matrix(rep.int(1, length(terms)))
	tables[[i]] <- tapply(data, mf[mf.cols[[i]]],
                              get(fun, mode="function"))
	class(tables[[i]]) <- "mtable"
	if(prt) print(tables[i], ..., quote = FALSE)
    }
    tables
}


make.tables.aovprojlist <-
    function(proj.cols, strata.cols, model.cols, projections, model, eff,
	     fun = "mean", prt = FALSE, ...)
{
    tables <- setNames(vector("list", length(proj.cols)), names(proj.cols))
    if(!missing(eff)) {
	for(i in seq_along(tables)) {
	    terms <- proj.cols[[i]]
	    if(all(is.na(eff.i <- match(terms, names(eff)))))
		eff.i <- rep.int(1, length(terms))
	    if(length(terms) == 1L)
		data <- projections[[strata.cols[i]]][, terms]/ eff[eff.i]
	    else {
		if(length(strata <- unique(strata.cols[terms])) == 1L)
		    data <- projections[[strata]][, terms] %*%
			as.matrix(1/eff[eff.i])
		else {
		    mat <- NULL
		    for(j in strata) {
			mat <- cbind(mat, projections[[j]][, terms[!is.na(match(terms,
										names(strata.cols)[strata.cols == j]))]])
		    }
		    data <- mat %*% as.matrix(1/eff[eff.i])
		}
	    }
	    tables[[i]] <- tapply(data, model[model.cols[[i]]],
                                  get(fun, mode="function"))
	    attr(tables[[i]], "strata") <- strata.cols[i]
	    class(tables[[i]]) <- "mtable"
	    if(prt) print(tables[i], ..., quote = FALSE)
	}
    } else for(i in seq_along(tables)) {
	terms <- proj.cols[[i]]
	if(length(terms) == 1L) data <- projections[[strata.cols[i]]][, terms]
	else {
	    if(length(strata <- unique(strata.cols[terms])) == 1L)
		data <- projections[[strata]][, terms] %*%
		    as.matrix(rep.int(1, length(terms)))
	    else {
		mat <- NULL
		for(j in strata) {
		    mat <- cbind(mat, projections[[j]][, terms[!is.na(match(terms,
									    names(strata.cols)[strata.cols == j]))]])
		}
		data <- mat %*% as.matrix(rep.int(1, length(terms)))
	    }
	}
	tables[[i]] <- tapply(data, model[model.cols[[i]]], get(fun))
	attr(tables[[i]], "strata") <- strata.cols[i]
	class(tables[[i]]) <- "mtable"
	if(prt) print(tables[i], ..., quote = FALSE)
    }
    tables
}

replications <- function(formula, data = NULL, na.action)
{
    if(missing(data) && inherits(formula, "data.frame")) {
	data <- formula
	formula <-  ~ .
    }
    if(!inherits(formula, "terms")) {
	formula <- as.formula(formula)
	if(length(formula) < 3L) {
	    f <- y ~ x
	    f[[3L]] <- formula[[2L]]
	    formula <- f
	}
	formula <- terms(formula, data = data)
    }
    if(missing(na.action))
        if(!is.null(tj <- attr(data, "na.action")) && is.function(tj))
            na.action <- tj
        else {
            naa <- getOption("na.action")
            if(!is.null(naa)) na.action <- match.fun(naa)
            else  na.action <- na.fail
        }
    f <- attr(formula, "factors")
    o <- attr(formula, "order")
    labels <- attr(formula, "term.labels")
    vars <- as.character(attr(formula, "variables"))[-1L]
    if(is.null(data)) {
	v <- c(quote(data.frame), attr(formula, "variables"))
	data <- eval(as.call(v), parent.frame())
    }
    if(!is.function(na.action)) stop("na.action must be a function")
    data <- na.action(data)
    class(data) <- NULL
    n <- length(o)
    z <- setNames(vector("list", n), labels)
    dummy <- numeric(.row_names_info(data, 2L))
    data <- lapply(data, function(x) if (is.character(x)) as.factor(x) else x)
    notfactor <- !sapply(data, function(x) inherits(x, "factor"))
    balance <- TRUE
    for(i in seq_len(n)) {
	l <- labels[i]
	if(o[i] < 1 || substring(l, 1L, 5L) == "Error") { z[[l]] <- NULL; next }
	select <- vars[f[, i] > 0]
	if(any(nn <- notfactor[select])) {
            warning(gettextf("non-factors ignored: %s",
                             paste(names(nn), collapse = ", ")),
                    domain = NA)
	    next
	}
	if(length(select))
	    tble <- tapply(dummy, unclass(data[select]), length)
	nrep <- unique(as.vector(tble))
	if(length(nrep) > 1L) {
	    balance <- FALSE
	    tble[is.na(tble)] <- 0
	    z[[l]] <- tble
	} else z[[l]] <- as.vector(nrep)
    }
    if(balance) unlist(z) else z
}

print.tables_aov <- function(x, digits = 4L, ...)
{
    tables.aov <- x$tables
    n.aov <- x$n
    se.aov <- if(se <- !is.na(match("se", names(x)))) x$se
    type <- attr(x, "type")
    switch(type,
	   effects = cat("Tables of effects\n"),
	   means = cat("Tables of means\n"),
	   residuals = if(length(tables.aov) > 1L) cat(
	   "Table of residuals from each stratum\n"))
    if(!is.na(ii <- match("Grand mean", names(tables.aov)))) {
	cat("Grand mean\n")
	gmtable <- tables.aov[[ii]]
	print.mtable(gmtable, digits = digits, ...)
    }
    for(i in names(tables.aov)) {
	if(i == "Grand mean") next
	table <- tables.aov[[i]]
	cat("\n", i, "\n")
	if(!is.list(n.aov))
	    print.mtable(table, digits = digits, ...)
	else {
	    n <- n.aov[[i]]
	    if(length(dim(table)) < 2L) {
		table <- rbind(table, n)
		rownames(table) <- c("", "rep")
		print(table, digits = digits, ...)
	    } else {
		ctable <- array(c(table, n), dim = c(dim(table), 2L))
		dim.t <- dim(ctable)
		d <- length(dim.t)
		ctable <- aperm(ctable, c(1, d, 2:(d - 1)))
		dim(ctable) <- c(dim.t[1L] * dim.t[d], dim.t[-c(1, d)])
		dimnames(ctable) <-
		    c(list(format(c(rownames(table), rep.int("rep", dim.t[1L])))),
                      dimnames(table)[-1L])
		ctable <- eval(parse(text = paste(
				     "ctable[as.numeric(t(matrix(seq(nrow(ctable)),ncol=2)))", paste(rep.int(", ", d - 2), collapse = " "), "]"),
                                     keep.source = FALSE))
		names(dimnames(ctable)) <- names(dimnames(table))
		class(ctable) <- "mtable"
		print.mtable(ctable, digits = digits, ...)
	    }
	}
    }
    if(se) {
	if(type == "residuals") rn <- "df" else rn <- "replic."
	switch(attr(se.aov, "type"),
	       effects = cat("\nStandard errors of effects\n"),
	       means = cat("\nStandard errors for differences of means\n"),
	       residuals = cat("\nStandard errors of residuals\n"))
	if(length(unlist(se.aov)) == length(se.aov)) {
	    ## the simplest case: single replication, unique se
					# kludge for NA's
	    n.aov <- n.aov[!is.na(n.aov)]
	    se.aov <- unlist(se.aov)
	    cn <- names(se.aov)
	    se.aov <- rbind(format(se.aov, digits = digits), format(n.aov))
	    dimnames(se.aov) <- list(c(" ", rn), cn)
	    print(se.aov, quote=FALSE, right=TRUE, ...)
	} else for(i in names(se.aov)) {
	    se <- se.aov[[i]]
	    if(length(se) == 1L) { ## single se
		se <- rbind(se, n.aov[i])
		dimnames(se) <- list(c(i, rn), "")
		print(se, digits = digits, ...)
	    } else {		## different se
		dimnames(se)[[1L]] <- ""
		cat("\n", i, "\n")
		cat("When comparing means with same levels of:\n")
		print(se, digits, ...)
		cat("replic.", n.aov[i], "\n")
	    }
	}
    }
    invisible(x)
}

eff.aovlist <- function(aovlist)
{
    Terms <- terms(aovlist)
    if(names(aovlist)[[1L]] == "(Intercept)") aovlist <- aovlist[-1L]
    pure.error.strata <- sapply(aovlist, function(x) is.null(x$qr))
    aovlist <- aovlist[!pure.error.strata]
    s.labs <- names(aovlist)
    ## find which terms are in which strata
    s.terms <-
        lapply(aovlist, function(x) {
            asgn <- x$assign[x$qr$pivot[1L:x$rank]]
            attr(terms(x), "term.labels")[asgn]
        })
    t.labs <- attr(Terms, "term.labels")
    t.labs <- t.labs[t.labs %in% unlist(s.terms)]
    eff <- matrix(0, ncol = length(t.labs), nrow = length(s.labs),
		  dimnames = list(s.labs, t.labs))
    for(i in names(s.terms)) eff[i, s.terms[[i]] ] <- 1
    cs <- colSums(eff)
    ## if all terms are in just one stratum we are done
    if(all(cs <= 1)) return(eff[, cs > 0, drop = FALSE])

    nm <- t.labs[ cs > 1]
    pl <-
	lapply(aovlist, function(x)
	   {
	       asgn <- x$assign[x$qr$pivot[1L:x$rank]]
	       sp <- split(seq_along(asgn), attr(terms(x), "term.labels")[asgn])
               sp <- sp[names(sp) %in% nm]
	       sapply(sp, function(x, y) {
                   y <- y[x, x, drop = FALSE]
                   res <- sum(diag(y)^2)
                   if(nrow(y) > 1 && sum(y^2) > 1.01 * res)
                       stop("eff.aovlist: non-orthogonal contrasts would give an incorrect answer")
                   res
               }, y=x$qr$qr)
	   })
    for(i in names(pl)) eff[i, names(pl[[i]]) ] <- pl[[i]]
    cs <- colSums(eff)
    eff <- eff/rep(cs, each = nrow(eff))
    eff[, cs != 0, drop = FALSE]
}


model.frame.aovlist <- function(formula, data = NULL, ...)
{
    ## formula is an aovlist object
    call <- match.call()
    oc <- attr(formula, "call")
    Terms <- attr(formula, "terms")
    rm(formula)
    indError <- attr(Terms, "specials")$Error
    errorterm <-  attr(Terms, "variables")[[1 + indError]]
    form <- update.formula(Terms,
                           paste(". ~ .-", deparse(errorterm, width.cutoff=500L,
                                                   backtick = TRUE),
                                 "+", deparse(errorterm[[2L]], width.cutoff=500L,
                                              backtick = TRUE)))
    nargs <- as.list(call)
    oargs <- as.list(oc)
    nargs <- nargs[match(c("data", "na.action", "subset"), names(nargs), 0)]
    args <- oargs[match(c("data", "na.action", "subset"), names(oargs), 0)]
    args[names(nargs)] <- nargs
    args$formula <- form
    env <- environment(Terms)
    if (is.null(env)) env <- parent.frame()
    ## need stats:: for non-standard evaluation
    fcall <- c(list(quote(stats::model.frame)), args)
    eval(as.call(fcall), env)
}

print.mtable <-
    function(x, ..., digits = getOption("digits"), quote = FALSE, right = FALSE)
{
    xxx <- x
    xx <- attr(x, "Notes")
#    nn <- names(dimnames(x))
    a.ind <- match(names(a <- attributes(x)), c("dim", "dimnames", "names"))
    a <- a[!is.na(a.ind)]
    class(x) <- attributes(x) <- NULL
    attributes(x) <- a
#    if(length(nn) > 1L)
#	cat(paste("Dim ",paste(seq(length(nn)), "=", nn, collapse= ", "),"\n"))
    if(length(x) == 1 && is.null(names(x)) && is.null(dimnames(x)))
	names(x) <- rep("", length(x))
    if(length(dim(x)) && is.numeric(x)) {
	xna <- is.na(x)
	x <- format(zapsmall(x, digits))
	x[xna] <- "  "
    }
    print(x, quote = quote, right = right, ...)
    if(length(xx)) {
	cat("\nNotes:\n")
	print(xx)
    }
    invisible(xxx)
}


