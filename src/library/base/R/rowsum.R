#  File src/library/base/R/rowsum.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1995-2014 The R Core Team
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

rowsum <- function(x, group, reorder = TRUE, ...) UseMethod("rowsum")

rowsum.default <- function(x, group, reorder = TRUE, na.rm = FALSE, ...)
{
    if (!is.numeric(x)) stop("'x' must be numeric")
    if (length(group) != NROW(x)) stop("incorrect length for 'group'")
    if (anyNA(group)) warning("missing values for 'group'")
    ugroup <- unique(group)
    if (reorder) ugroup <- sort(ugroup, na.last = TRUE, method = "quick")
    ## ugroup can be either a vector or a factor, so do as.character here
    .Internal(rowsum_matrix(x, group, ugroup, na.rm, as.character(ugroup)))
}

rowsum.data.frame <- function(x, group, reorder = TRUE, na.rm = FALSE, ...)
{
    if (!is.data.frame(x)) stop("not a data frame") ## make MM happy
    if (length(group) != NROW(x)) stop("incorrect length for 'group'")
    if (anyNA(group)) warning("missing values for 'group'")
    ugroup <- unique(group)
    if (reorder) ugroup <- sort(ugroup, na.last = TRUE, method = "quick")
    .Internal(rowsum_df(x, group, ugroup, na.rm, as.character(ugroup)))
}
