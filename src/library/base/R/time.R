#  File src/library/base/R/time.R
#  Part of the R package, http://www.R-project.org
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

system.time <- function(expr, gcFirst = TRUE)
{
    ppt <- function(y) {
        if(!is.na(y[4])) y[1] <- y[1] + y[4]
        if(!is.na(y[5])) y[2] <- y[2] + y[5]
        y[1:3]
    }
    if(!exists("proc.time")) return(rep(NA_real_, 5))
    loc.frame <- parent.frame()
    if(gcFirst)  gc(FALSE)
    expr <- substitute(expr)
    time <- proc.time()
    ## need on.exit after 'time' has been set:
    ## on some systems proc.time throws an error.
    on.exit(cat("Timing stopped at:", ppt(proc.time() - time), "\n"))
    eval(expr, envir = loc.frame)
    new.time <- proc.time()
    on.exit()
    structure(new.time - time, class="proc_time")
}
unix.time <- system.time

date <- function() .Internal(date())

print.proc_time <- function(x, ...)
{
    y <- x
    if(!is.na(y[4])) y[1] <- y[1] + y[4]
    if(!is.na(y[5])) y[2] <- y[2] + y[5]
    y <- y[1:3]
    names(y) <- c(gettext("user"), gettext("system"), gettext("elapsed"))
    print(y, ...)
    invisible(x)
}
