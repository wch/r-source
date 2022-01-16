#  File src/library/base/R/locales.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2021 The R Core Team
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

.LC.categories <- c("LC_ALL",      "LC_COLLATE", "LC_CTYPE",
                    "LC_MONETARY", "LC_NUMERIC", "LC_TIME",
                    ## if(.Platform$OS.type == "unix") -- but should not error on Windows
                        c("LC_MESSAGES", "LC_PAPER", "LC_MEASUREMENT"))

Sys.getlocale <- function(category = "LC_ALL")
{
    category <- match(category, .LC.categories)
    if(is.na(category)) stop("invalid 'category' argument")
    .Internal(Sys.getlocale(category))
}

Sys.setlocale <- function(category = "LC_ALL", locale = "")
{
    category <- match(category, .LC.categories)
    if(is.na(category)) stop("invalid 'category' argument")
    .Internal(Sys.setlocale(category, locale))
}

Sys.localeconv <- function() .Internal(Sys.localeconv())
