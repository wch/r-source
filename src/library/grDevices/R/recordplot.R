#  File src/library/grDevices/R/recordplot.R
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
    if (is.null(pid) || pid != Sys.getpid()) {
        # This is a "recordedplot" loaded from another session
        x <- restoreRecordedPlot(x)
    }
    invisible(.External2(C_playSnapshot, x))
}

print.recordedplot <- function(x, ...)
{
    replayPlot(x)
    invisible(x)
}

# If this is a "recordedplot" that has been saved and reloaded
# (possibly across sessions) then we need to ...
# - check that the graphics API version matches the current session
# - restore NativeSymbolInfo on each element of the snapshot display list
# - bail out gracefully if something is not right
restoreRecordedPlot <- function(x) {
    # The display list is the first component of the snapshot
    plot <- x
    for (i in 1:length(plot[[1]])) {
        # get the symbol then test if it's a native symbol
        symbol <- plot[[1]][[i]][[2]][[1]]
        if ("NativeSymbolInfo" %in% class(symbol)) {
            # determine the dll that the symbol lives in
            if (!is.null(symbol$package))
                name <- symbol$package[["name"]]
            else
                name <- symbol$dll[["name"]]
            pkgDLL <- getLoadedDLLs()[[name]]
            # reconstruct the native symbol and assign it into the plot
            nativeSymbol <- getNativeSymbolInfo(name = symbol$name,
                                                PACKAGE = pkgDLL,
                                                withRegistrationInfo = TRUE)
            plot[[1]][[i]][[2]][[1]] <- nativeSymbol
        }
    }
    plot
}
