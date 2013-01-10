#  File src/library/grDevices/R/recordplot.R
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

rversion <- function() {
  paste(R.Version()[c("major", "minor")],
                                  collapse=".")
}

recordPlot <- function()
{
    if(dev.cur() == 1)
        stop("no current device to record from")
    res <- .External2(C_getSnapshot)
    attr(res, "version") <- rversion()
    class(res) <- "recordedplot"
    res
}

replayPlot <- function(x)
{
    if(!inherits(x, "recordedplot"))
        stop(gettextf("argument is not of class %s", dQuote("recordedplot")),
             domain = NA)
    nm <- names(x)
    version <- attr(x, "version") ## added in R 2.0.0.
    if (is.null(version) || version < as.numeric_version("3.0.0"))
        stop("loading snapshot from pre-3.0.0 R version")
    else if (version != rversion())
        warning(gettext("loading snapshot from different R version"),
                " (", version, ")", domain = NA)
    invisible(.External2(C_playSnapshot, x))
}

print.recordedplot <- function(x, ...)
{
    replayPlot(x)
    invisible(x)
}

