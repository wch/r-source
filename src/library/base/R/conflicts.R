#  File src/library/base/R/conflicts.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1998 B. D. Ripley
#  Copyright (C) 2005-2011 The R Core Team
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

conflicts <- function(where = search(), detail = FALSE)
{
    if(length(where) < 1L) stop("argument 'where' of length 0")
    z <- vector(length(where), mode="list")
    names(z) <- where
    for(i in seq_along(where)) z[[i]] <- objects(pos = where[i])
    all <- unlist(z, use.names=FALSE)
    dups <- duplicated(all)
    dups <- all[dups]
    if(detail) {
	for(i in where) z[[i]] <- z[[i]][match(dups, z[[i]], 0L)]
	z[vapply(z, function(x) length(x) == 0L, NA)] <- NULL
	z
    } else dups
}
