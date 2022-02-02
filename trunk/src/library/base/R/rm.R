#  File src/library/base/R/rm.R
#  Part of the R package, https://www.R-project.org
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
#  https://www.R-project.org/Licenses/

rm <-
    function (..., list = character(), pos = -1, envir = as.environment(pos),
              inherits = FALSE)
{
    dots <- match.call(expand.dots=FALSE)$...
    if(length(dots) &&
       !all(vapply(dots, function(x) is.symbol(x) || is.character(x), NA, USE.NAMES=FALSE)))
       stop("... must contain names or character strings")
    names <- vapply(dots, as.character, "")
    if (length(names) == 0L) names <- character()
    list <- .Primitive("c")(list, names)
    .Internal(remove(list, envir, inherits))
}

remove <- rm
