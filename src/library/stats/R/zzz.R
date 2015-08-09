#  File src/library/stats/R/zzz.R
#  Part of the R package, https://www.R-project.org
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
#  https://www.R-project.org/Licenses/

.noGenerics <- TRUE

.onLoad <- function(libname, pkgname)
{
    op <- options()
    op.stats <-
        list(contrasts = c(unordered="contr.treatment",
             ordered="contr.poly"),
             na.action = "na.omit",
             show.coef.Pvalues = TRUE,
             show.signif.stars = TRUE,
	     str.dendrogram.last = "`",
             ts.eps = 1e-5,
             ts.S.compat = FALSE)
    toset <- !(names(op.stats) %in% names(op))
    if(any(toset)) options(op.stats[toset])
}

.onUnload <- function(libpath)
    library.dynam.unload("stats", libpath)
