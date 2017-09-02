#  File src/library/parallel/R/windows/mcdummies.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2017 The R Core Team
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

mclapply <- function(X, FUN, ..., mc.preschedule = TRUE, mc.set.seed = TRUE,
                     mc.silent = FALSE, mc.cores = 1L,
                     mc.cleanup = TRUE, mc.allow.recursive = TRUE, affinity.list = NULL)
{
    cores <- as.integer(mc.cores)
    if(cores < 1L) stop("'mc.cores' must be >= 1")
    if(cores > 1L) stop("'mc.cores' > 1 is not supported on Windows")
    lapply(X, FUN, ...)
}

pvec <- function(v, FUN, ..., mc.set.seed = TRUE, mc.silent = FALSE,
                 mc.cores = 1L, mc.cleanup = TRUE)
{
    if (!is.vector(v)) stop("'v' must be a vector")
    cores <- as.integer(mc.cores)
    if(cores < 1L) stop("'mc.cores' must be >= 1")
    if(cores > 1L) stop("'mc.cores' > 1 is not supported on Windows")
    FUN(v, ...)
}

mcmapply <-
    function(FUN, ..., MoreArgs = NULL, SIMPLIFY = TRUE, USE.NAMES = TRUE,
             mc.preschedule = TRUE, mc.set.seed = TRUE,
             mc.silent = FALSE, mc.cores  = 1L, mc.cleanup = TRUE, affinity.list = NULL)
{
    cores <- as.integer(mc.cores)
    if(cores < 1L) stop("'mc.cores' must be >= 1")
    if(cores > 1L) stop("'mc.cores' > 1 is not supported on Windows")
    mapply(FUN = FUN, ..., MoreArgs = MoreArgs, SIMPLIFY = SIMPLIFY,
           USE.NAMES = USE.NAMES)
}

mcMap <- function (f, ...) Map(f, ...)
