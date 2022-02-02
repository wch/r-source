#  File src/library/base/R/options.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2018 The R Core Team
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

options <- function(...)
    .Internal(options(...))

## getOption_SAtk() (by Tomas K. from from Suharto A. from Matt D.): \PR{17394}
getOption <- function(x, default = NULL) {
     if (missing(default))
         .Internal(getOption(x))
     else {
         ans <- .Internal(getOption(x))
         if (is.null(ans)) default else ans
     }
}
