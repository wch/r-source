#  File src/library/base/R/aperm.R
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

aperm <- function(a, perm, ...) UseMethod("aperm")

aperm.default <- function (a, perm = NULL, resize = TRUE, ...)
     .Internal(aperm(a, perm, resize))

aperm.table <- function(a, perm = NULL, resize = TRUE, keep.class = TRUE, ...)
{
     r <- aperm.default(a, perm, resize=resize)
     if(keep.class) class(r) <- class(a)
     r
}
