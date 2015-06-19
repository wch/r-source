#  File src/library/grDevices/R/recordplot.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1995-2013 The R Core Team
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

recordPlot <- function()
{
    if(dev.cur() == 1L)
        stop("no current device to record from")
    res <- .External2(C_getSnapshot)
    attr(res, "pid") <- Sys.getpid()
    class(res) <- "recordedplot"
    res
}

replayPlot <- function(x)
{
    if(!inherits(x, "recordedplot"))
        stop(gettextf("argument is not of class %s", dQuote("recordedplot")),
             domain = NA)
    pid <- attr(x, "pid") ## added after R 3.0.2
    if (is.null(pid) || pid != Sys.getpid())
        stop("loading snapshot from a different session")
    invisible(.External2(C_playSnapshot, x))
}

print.recordedplot <- function(x, ...)
{
    replayPlot(x)
    invisible(x)
}

