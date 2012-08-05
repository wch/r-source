#  File src/library/base/R/notyet.R
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

.NotYetImplemented <- function ()
    stop(gettextf("'%s' is not implemented yet",
                  as.character(sys.call(sys.parent())[[1L]])), call. = FALSE)

.NotYetUsed <- function(arg, error = TRUE) {
    msg <- gettextf("argument '%s' is not used (yet)", arg)
    if(error) stop(msg, domain = NA, call. = FALSE)
    else warning(msg, domain = NA, call. = FALSE)
}
