#  File src/library/base/R/callenv.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2020 The R Core Team
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

.addLocalToCallenv <- function(name, value, envir) {
    if (typeof(name) == "symbol")
        name <- as.character(name)
    stopifnot(typeof(name) == "character" && length(name) == 1)
    sym <- as.name(name)
    locs <- attr(envir, ".funVarsInfo")[[3]]
    if (is.null(Find(function(s) identical(s, sym), locs)))
        attr(envir, ".funVarsInfo")[[3]] <- c(list(sym), locs)
    if (missing(value))
        invisible(NULL)
    else
        assign(name, value, envir)
}

.getEnvVarsInfo <- function(envir) {
    info <- attr(envir, ".funVarsInfo")
    list(gvars = sapply(info[[1]], as.character),
         gfuns = sapply(info[[2]], as.character),
         locs = sapply(info[[3]], as.character))
}

.strictCallVars <- function(new) {
    if (missing(new))
        new <- NA
    .Internal(strictCallVars(new))
}
