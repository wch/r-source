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

# 'load' and 'attach' should be character vectors of package names
recordPlot <- function(load=NULL, attach=NULL)
{
    if(dev.cur() == 1L)
        stop("no current device to record from")
    res <- .External2(C_getSnapshot)
    attr(res, "pid") <- Sys.getpid()
    attr(res, "Rversion") <- getRversion()
    attr(res, "load") <- as.character(load)
    attr(res, "attach") <- as.character(attach)
    class(res) <- "recordedplot"
    res
}

replayPlot <- function(x, reloadPkgs=FALSE)
{
    if(!inherits(x, "recordedplot"))
        stop(gettextf("argument is not of class %s", dQuote("recordedplot")),
             domain = NA)
    pid <- attr(x, "pid") ## added after R 3.0.2
    if (is.null(pid) || pid != Sys.getpid()) {
        # This is a "recordedplot" loaded from another session
        x <- restoreRecordedPlot(x, reloadPkgs)
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
# - warn if have R version mismatch
# - restore NativeSymbolInfo on each element of the snapshot display list
# - bail out gracefully if something is not right
restoreRecordedPlot <- function(x, reloadPkgs) {
    snapshotRversion <- attr(x, "Rversion")
    if (is.null(snapshotRversion)) {
        warning("snapshot recorded in different R version (pre 3.3.0)")
    } else if (snapshotRversion != getRversion()) {
        warning(gettextf("snapshot recorded in different R version (%s)",
                         snapshotRversion))
    }
    # Ensure that all graphics systems in the snapshot are available
    # (snapshots only started recording pkgName in R 3.3.0)
    # Similar for any 'pkgs' saved with the snapshot
    n <- length(x)
    if (n > 1 &&
        !is.null(snapshotRversion) &&
        snapshotRversion >= R_system_version("3.3.0")) {
        for (i in 2:n) {
            library(attr(x[[i]], "pkgName"), character.only=TRUE)
        }
        if (reloadPkgs) {
            load <- attr(x, "load")
            for (i in load) {
                loadNamespace(i)
            }
            attach <- attr(x, "attach")
            for (i in attach) {
                library(i, character.only=TRUE)
            }
        }
    }
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
            # This will error out if it fails to find the symbol, which
            # is some protection against running "recordedplot" in
            # R session where the recorded function does not exist!
            nativeSymbol <- getNativeSymbolInfo(name = symbol$name,
                                                PACKAGE = pkgDLL,
                                                withRegistrationInfo = TRUE)
            # Check that the 'numParameters' matches.
            # If it does not, we would also receive a redundant WARNING
            # about R version or graphics engine version mismatch,
            # but this mismatch is serious enough to put a STOP to things.
            if (nativeSymbol$numParameters != symbol$numParameters) {
                stop("snapshot contains invalid graphics call")
            }
            plot[[1]][[i]][[2]][[1]] <- nativeSymbol
        }
    }
    plot
}
