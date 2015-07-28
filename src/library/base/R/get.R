#  File src/library/base/R/get.R
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

exists <-
    function (x, where = -1,
              envir = if(missing(frame)) as.environment(where) else sys.frame(frame),
              frame, mode = "any", inherits = TRUE)
    .Internal(exists(x, envir, mode, inherits))

get <-
    function (x, pos = -1L, envir = as.environment(pos), mode = "any",
              inherits = TRUE)
    .Internal(get(x, envir, mode, inherits))

get0 <- function (x, envir = pos.to.env(-1L), mode = "any", inherits = TRUE,
                  ifnotfound = NULL)
    .Internal(get0(x, envir, mode, inherits, ifnotfound))

mget <- function(x, envir = as.environment(-1L), mode = "any",
                 ifnotfound, inherits = FALSE)
    .Internal(mget(x, envir, mode,
                   if(missing(ifnotfound))
                       list(function(x) stop(gettextf("value for %s not found", sQuote(x)),
                                             call. = FALSE))
                   else ifnotfound,
                   inherits))

## DB's proposed name "getSlotOrComponent" is more precise but harder to type
getElement <- function(object, name) {
    if(isS4(object)) slot(object, name) else object[[name, exact=TRUE]]
}
