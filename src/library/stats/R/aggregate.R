#  File src/library/stats/R/aggregate.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2018 The R Core Team
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

aggregate <-
function(x, ...)
    UseMethod("aggregate")

aggregate.default <-
function(x, ...)
{
    if(is.ts(x))
        aggregate.ts(as.ts(x), ...)
    else
        aggregate.data.frame(as.data.frame(x), ...)
}

aggregate.data.frame <-
function(x, by, FUN, ..., simplify = TRUE, drop = TRUE)
{
    if(!is.data.frame(x)) x <- as.data.frame(x)
    ## Do this here to avoid masking by non-function (could happen)
    FUN <- match.fun(FUN)
    if(NROW(x) == 0L) stop("no rows to aggregate")
    if(NCOL(x) == 0L) {
        ## fake it
        x <- data.frame(x = rep(1, NROW(x)))
        return(aggregate.data.frame(x, by, function(x) 0L)[seq_along(by)])
    }
    if(!is.list(by))
        stop("'by' must be a list")
    if(is.null(names(by)) && length(by))
        names(by) <- paste0("Group.", seq_along(by))
    else {
        nam <- names(by)
        ind <- which(!nzchar(nam))
        names(by)[ind] <- paste0("Group.", ind)
    }

    if(any(lengths(by) != NROW(x)))
        stop("arguments must have same length")

    y <- as.data.frame(by, stringsAsFactors = FALSE)
    keep <- complete.cases(by)
    y <- y[keep, , drop = FALSE]
    x <- x[keep, , drop = FALSE]
    nrx <- NROW(x)

    ## Generate a group identifier vector with integers and dots.
    ident <- function(x) {
        y <- as.factor(x)
        l <- length(levels(y))
        s <- as.character(seq_len(l))
        n <- nchar(s)
        levels(y) <- paste0(strrep("0", n[l] - n), s)
        y # levels used for drop = FALSE
    }
    grp <- lapply(y, ident)
    multi.y <- !drop && ncol(y)
    if(multi.y) {
        lev <- lapply(grp, levels)
	y <- as.list(y)
        for (i in seq_along(y)) {
            z <- y[[i]][match(lev[[i]], grp[[i]])]
            if(is.factor(z) && any(keep <- is.na(z)))
                z[keep] <- levels(z)[keep]
            y[[i]] <- z
        }
        eGrid <- function(L)
            expand.grid(L, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
	y <- eGrid(y)
    }
    grp <- if(ncol(y)) {
        names(grp) <- NULL
	do.call(paste, c(rev(grp), list(sep = ".")))
    } else
	integer(nrx)
    if(multi.y) {
        lev <- as.list(eGrid(lev))
        names(lev) <- NULL
        lev <- do.call(paste, c(rev(lev), list(sep = ".")))
    } else
        y <- y[match(sort(unique(grp)), grp, 0L), , drop = FALSE]
    z <- lapply(x,
                function(e) {
                    ## In case of a common length > 1, sapply() gives
                    ## the transpose of what we need ...
		    ans <- lapply(X = unname(split(e, grp)), FUN = FUN, ...)
                    if(simplify &&
                       length(len <- unique(lengths(ans))) == 1L) {
                        ## this used to lose classes
                        if(len == 1L) {
                            cl <- lapply(ans, oldClass)
                            cl1 <- cl[[1L]]
			    ans <- unlist(ans, recursive = FALSE, use.names = FALSE)
                            if (!is.null(cl1) &&
                                all(vapply(cl, identical, NA, y = cl1)))
                                class(ans) <- cl1
                        } else if(len > 1L)
			    ans <- matrix(unlist(ans, recursive = FALSE, use.names = FALSE),
                                          ncol = len,
                                          byrow = TRUE,
					  dimnames =
					      if(!is.null(nms <- names(ans[[1L]])))
						  list(NULL, nms) ## else NULL
					  )
                    }
                    ans
                })
    len <- length(y)
    if(multi.y) {
	keep <- match(lev, sort(unique(grp)))
	for(i in seq_along(z))
	    y[[len + i]] <- if(is.matrix(z[[i]]))
				 z[[i]][keep, , drop = FALSE]
			    else z[[i]][keep]
    } else
	for(i in seq_along(z))
	    y[[len + i]] <- z[[i]]
    names(y) <- c(names(by), names(x))
    row.names(y) <- NULL
    y
}

aggregate.formula <-
function(formula, data, FUN, ..., subset, na.action = na.omit)
{
    if(missing(formula) || !inherits(formula, "formula"))
        stop("'formula' missing or incorrect")
    if(length(formula) != 3L)
        stop("'formula' must have both left and right hand sides")

    m <- match.call(expand.dots = FALSE)
    if(is.matrix(eval(m$data, parent.frame())))
        m$data <- as.data.frame(data)
    m$... <- m$FUN <- NULL
    ## need stats:: for non-standard evaluation
    m[[1L]] <- quote(stats::model.frame)

    if (formula[[2L]] == ".") {
        ## LHS is a dot, expand it ...
        ##rhs <- unlist(strsplit(deparse(formula[[3L]]), " *[:+] *"))
        ## <NOTE>
        ## Note that this will not do quite the right thing in case the
        ## RHS contains transformed variables, such that
        ##   setdiff(rhs, names(data))
        ## is non-empty ...
        ##lhs <- sprintf("cbind(%s)",
        ##              paste(setdiff(names(data), rhs), collapse = ","))
        ## formula[[2L]] <- parse(text = lhs)[[1L]]
        ## </NOTE>

        ## New logic May 2012 --pd

        ## Dot expansion:
        ## lhs ends up as quote(cbind(v1, v2, ....)) using all variables in
        ## data, except those that are used on the RHS.

        ## This version uses terms() to get the rhs variables, which means
        ## that it will NOT remove a variable from the expansion if a
        ## transformation of it is on the RHS of the formula.

        rhs <- as.list(attr(terms(formula[-2L]),"variables")[-1])
        lhs <- as.call(c(quote(cbind),
                         setdiff(lapply(names(data), as.name),
                                 rhs)
                         )
                       )
        formula[[2L]] <- lhs
        m[[2L]] <- formula
    }
    mf <- eval(m, parent.frame())

    if(is.matrix(mf[[1L]])) {
        ## LHS is a cbind() combo, convert to data frame and fix names.
        ## Commented out May 2012 (seems to work without it) -- pd
	##lhs <- setNames(as.data.frame(mf[[1L]]),
	##		as.character(m[[2L]][[2L]])[-1L])
        lhs <- as.data.frame(mf[[1L]])
        aggregate.data.frame(lhs, mf[-1L], FUN = FUN, ...)
    }
    else
        aggregate.data.frame(mf[1L], mf[-1L], FUN = FUN, ...)
}

aggregate.ts <-
function(x, nfrequency = 1, FUN = sum, ndeltat = 1,
         ts.eps = getOption("ts.eps"), ...)
{
    x <- as.ts(x)
    ofrequency <- tsp(x)[3L]
    ## do this here to avoid masking by non-function (could happen)
    FUN <- match.fun(FUN)
    ## Set up the new frequency, and make sure it is an integer.
    if(missing(nfrequency))
        nfrequency <- 1 / ndeltat
    if((nfrequency > 1) &&
        (abs(nfrequency - round(nfrequency)) < ts.eps))
        nfrequency <- round(nfrequency)

    if(nfrequency == ofrequency)
        return(x)
    ratio <- ofrequency /nfrequency
    if(abs(ratio - round(ratio)) > ts.eps)
        stop(gettextf("cannot change frequency from %g to %g",
                      ofrequency, nfrequency), domain = NA)
    ## The desired result is obtained by applying FUN to blocks of
    ## length ofrequency/nfrequency, for each of the variables in x.
    ## We first get the new start and end right, and then break x into
    ## such blocks by reshaping it into an array and setting dim.
    ## avoid e.g. 1.0 %/% 0.2
    ## https://stat.ethz.ch/pipermail/r-devel/2010-April/057225.html
    len <- trunc((ofrequency / nfrequency) + ts.eps)
    mat <- is.matrix(x)
    if(mat) cn <- colnames(x)
    ##   nstart <- ceiling(tsp(x)[1L] * nfrequency) / nfrequency
    ##   x <- as.matrix(window(x, start = nstart))
    nstart <- tsp(x)[1L]
    ## Can't use nstart <- start(x) as this causes problems if
    ## you get a vector of length 2.
    x <- as.matrix(x)
    nend <- floor(nrow(x) / len) * len
    x <- apply(array(c(x[1 : nend, ]),
                     dim = c(len, nend / len, ncol(x))),
               MARGIN = c(2L, 3L), FUN = FUN, ...)
    if(!mat) x <- as.vector(x)
    else colnames(x) <- cn
    ts(x, start = nstart, frequency = nfrequency)
}

