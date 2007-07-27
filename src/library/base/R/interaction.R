#  File src/library/base/R/interaction.R
#  Part of the R package, http://www.R-project.org
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

### This is almost like the Primitive ":" for factors
### (that has no "drop = TRUE") --- it's not used anywhere in "standard R"
interaction <- function(..., drop = FALSE, sep = ".", lex.order = FALSE)
{
    args <- list(...)
    narg <- length(args)
    if (narg == 1 && is.list(args[[1]])) {
	args <- args[[1]]
	narg <- length(args)
    }
    for(i in narg:1) {
        f <- args[[i]]
	if (!is.factor(f)) f <- factor(f)
	l <- levels(f)
        if1 <- as.integer(f) - 1
        if(i != narg) {
            if(lex.order) {
                ll <- length(lvs)
                ans <- ans + ll * if1
                lvs <- ## as.vector(t(outer(l, lvs, paste, sep=sep)))
                    paste(rep(l, each= ll), rep(lvs, length(l)), sep=sep)
            } else {
                ans <- ans * length(l) + if1
                lvs <- ## as.vector(outer(l, lvs, paste, sep=sep))
                    paste(rep(l, length(lvs)), rep(lvs, each = length(l)), sep=sep)
            }
        } else {
            ans <- if1
            lvs <- l
        }
    }
    ans <- structure(as.integer(ans+1), levels=lvs, class = "factor")
    ans[ , drop=drop]
}
