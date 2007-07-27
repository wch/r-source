#  File src/library/grDevices/R/recordplot.R
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

rversion <- function() {
  paste(R.Version()[c("major", "minor")],
                                  collapse=".")
}

recordPlot <- function()
{
    if(dev.cur() == 1)
        stop("no current device to record from")
    res <- .Internal(getSnapshot())
    attr(res, "version") <- rversion()
    class(res) <- "recordedplot"
    res
}

replayPlot <- function(x)
{
    if(class(x) != "recordedplot")
        stop("argument is not of class \"recordedplot\"")
    nm <- names(x)
    if(length(nm) == 2 && nm == c("displaylist", "gpar")) {
        ## pre-1.4.0 save
        .Internal(setGPar(x[[2]]))
        .Internal(playDL(x[[1]]))
    } else {
      version <- attr(x, "version")
      if (is.null(version))
        warning("loading snapshot from pre-2.0.0 R version")
      else if (version != rversion())
        warning(gettext("loading snapshot from different R version"),
                " (", version, ")", domain = NA)
      .Internal(playSnapshot(x))
    }
}

print.recordedplot <- function(x, ...)
{
    replayPlot(x)
    invisible(x)
}

