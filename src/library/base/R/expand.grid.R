#  File src/library/base/R/expand.grid.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1995-2015 The R Core Team
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

expand.grid <- function(..., KEEP.OUT.ATTRS = TRUE, stringsAsFactors = TRUE)
{
    ## x should either be a list or a set of vectors or factors
    nargs <- length(args <- list(...))
    if(!nargs) return(as.data.frame(list()))
    if(nargs == 1L && is.list(a1 <- args[[1L]]))
	nargs <- length(args <- a1)
    if(nargs == 0L) return(as.data.frame(list()))
    ## avoid classed args such as data frames: cargs <- args
    cargs <- vector("list", nargs)
    iArgs <- seq_len(nargs)
    nmc <- paste0("Var", iArgs)
    nm <- names(args)
    if(is.null(nm))
	nm <- nmc
    else if(any(ng0 <- nzchar(nm)))
	nmc[ng0] <- nm[ng0]
    names(cargs) <- nmc
    rep.fac <- 1L
    d <- lengths(args)
    if(KEEP.OUT.ATTRS) {
	dn <- vector("list", nargs)
	names(dn) <- nmc
    }
    orep <- prod(d)
    if(orep == 0L) {
        for(i in iArgs) cargs[[i]] <- args[[i]][FALSE]
    } else {
        for(i in iArgs) {
            x <- args[[i]]
            if(KEEP.OUT.ATTRS)
                dn[[i]] <-
                    paste0(nmc[i], "=", if(is.numeric(x)) format(x) else x)
            nx <- length(x)
            orep <- orep/nx
            x <- x[rep.int(rep.int(seq_len(nx),
                                   rep.int(rep.fac, nx)), orep)]
	    ## avoid sorting the levels of character variates
	    if(stringsAsFactors && !is.factor(x) && is.character(x))
		x <- factor(x, levels = unique(x))
            cargs[[i]] <- x
            rep.fac <- rep.fac * nx
        }
    }
    if(KEEP.OUT.ATTRS)
	attr(cargs, "out.attrs") <- list(dim=d, dimnames=dn)
    rn <- .set_row_names( as.integer(prod(d)) )
    structure(cargs, class = "data.frame", row.names = rn)
}
