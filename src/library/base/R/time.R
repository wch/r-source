#  File src/library/base/R/time.R
#  Part of the R package, https://www.R-project.org
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
#  https://www.R-project.org/Licenses/

system.time <- function(expr, gcFirst = TRUE)
{
    ppt <- function(y) {
        if(!is.na(y[4L])) y[1L] <- y[1L] + y[4L]
        if(!is.na(y[5L])) y[2L] <- y[2L] + y[5L]
        y[1L:3L]
    }
    if(!exists("proc.time")) return(rep(NA_real_, 5L))
    if(gcFirst)  gc(FALSE)
    time <- proc.time()
    ## need on.exit after 'time' has been set:
    ## on some systems proc.time throws an error.
    on.exit(cat("Timing stopped at:", ppt(proc.time() - time), "\n"))
    expr # evaluated here because of lazy evaluation
    new.time <- proc.time()
    on.exit()
    structure(new.time - time, class="proc_time")
}
unix.time <- system.time

date <- function() .Internal(date())

summary.proc_time <-
function(object, ...)
{
    if(!is.na(object[4L]))
        object[1L] <- object[1L] + object[4L]
    if(!is.na(object[5L]))
        object[2L] <- object[2L] + object[5L]
    object <- object[1L : 3L]
    names(object) <-
        c(gettext("user"), gettext("system"), gettext("elapsed"))
    object
}

print.proc_time <-
function(x, ...)
{
    print(summary(x, ...))
    invisible(x)
}
