#  File src/library/base/R/interaction.R
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

### This is almost like the Primitive ":" for factors
### but with drop=TRUE, used in reshape
interaction <- function(..., drop = FALSE, sep = ".", lex.order = FALSE)
{
    args <- list(...)
    narg <- length(args)
    if (narg < 1L)
	stop("No factors specified")
    if (narg == 1L && is.list(args[[1L]])) {
	args <- args[[1L]]
	narg <- length(args)
    }
    for(i in narg:1L) {
        f <- as.factor(args[[i]])[, drop = drop]
        l <- levels(f)
        if1 <- as.integer(f) - 1L
        if(i == narg) {
            ans <- if1
            lvs <- l
        } else {
            if(lex.order) {
                ll <- length(lvs)
                ans <- ans + ll * if1
                lvs <- paste(rep(l, each = ll), rep(lvs, length(l)), sep=sep)
            } else {
                ans <- ans * length(l) + if1
                lvs <- paste(rep(l, length(lvs)),
                             rep(lvs, each = length(l)), sep=sep)
            }
            if(anyDuplicated(lvs)) { ## fix them up
                ulvs <- unique(lvs)
                while((i <- anyDuplicated(flv <- match(lvs, ulvs)))) {
                    lvs <- lvs[-i]
                    ans[ans+1L == i] <- match(flv[i], flv[1:(i-1)]) - 1L
                    ans[ans+1L > i] <- ans[ans+1L > i] - 1L
                }
                lvs <- ulvs
            }
            if(drop) {
                olvs <- lvs
                lvs <- lvs[sort(unique(ans+1L))]
                ans <- match(olvs[ans+1L], lvs) - 1L
            }
        }
    }
    structure(as.integer(ans+1L), levels=lvs, class = "factor")
}
