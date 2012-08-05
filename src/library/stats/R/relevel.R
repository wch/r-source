#  File src/library/stats/R/relevel.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1995-2012 The R Core Team
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

relevel <- function(x, ref, ...) UseMethod("relevel")

relevel.default <- function(x, ref, ...)
    stop("'relevel' only for factors")

relevel.ordered <- function(x, ref, ...)
    stop("'relevel' only for factors")

relevel.factor <- function(x, ref, ...)
{
    lev <- levels(x)
    if(length(ref) != 1L)
        stop("'ref' must be of length one")
    if(is.character(ref))
        ref <- match(ref, lev)
    if(is.na(ref))
        stop("'ref' must be an existing level")
    nlev <- length(lev)
    if(ref < 1 || ref > nlev)
        stop(gettextf("ref = %d must be in 1L:%d", ref, nlev), domain = NA)
    factor(x, levels = lev[c(ref, seq_along(lev)[-ref])])
}
