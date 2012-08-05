#  File src/library/utils/R/windows/sysutils.R
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

memory.size <- function(max = FALSE) round(.Internal(memory.size(max)), 2L)

memory.limit <- function(size = NA) trunc(.Internal(memory.size(size)))

DLL.version <- function(path) .Internal(DLL.version(path))

getClipboardFormats <- function(numeric = FALSE)
{
    known <- c("text", "bitmap", "metafile PICT", "SYLK", "DIF",
    "TIFF", "OEM text", "DIB", "palette", "pendata", "RIFF", "audio",
    "Unicode text", "enhanced metafile", "drag-and-drop", "locale", "shell")
    ans <- sort(.Internal(getClipboardFormats()))
    if(numeric) ans else {
        res <- known[ans]
        res[is.na(res)] <- ans[is.na(res)]
        res
    }
}

readClipboard <- function(format = 1, raw = FALSE)
    .Internal(readClipboard(format, raw))

writeClipboard <- function(str, format = 1)
    .Internal(writeClipboard(str, format))

getIdentification <- function()
    .Internal(getIdentification())

setWindowTitle <- function(suffix, title = paste(getIdentification(), suffix))
    .Internal(setWindowTitle(title))

getWindowTitle <- function()
    .Internal(getWindowTitle())

setStatusBar <- function(text)
    .Internal(setStatusBar(text))

getWindowsHandle <- function(which = "Console") {
    if (is.numeric(which)) {
	which <- as.integer(which)
        if(!exists(".Devices")) .Devices <- list("null device")
        if(which > 0 && which <= length(.Devices) && .Devices[[which]] != "windows")
            return(NULL)
    }
    .Internal(getWindowHandle(which))
}

getWindowsHandles <- function(which = "R", pattern="", minimized=FALSE) {
    which <- match.arg(which, c("R", "all"), several.ok=TRUE)
    len <- max(length(which), length(pattern), length(minimized))
    which <- rep(which, length.out=len)
    pattern <- rep(pattern, length.out=len)
    minimized <- rep(minimized, length.out=len)
    result <- list()
    for (i in seq_along(which)) {
	res <- .Internal(getWindowHandles(which[i], minimized))
	if (nzchar(pattern[i]))
    	    res <- res[grep(pattern[i], names(res))]
    	result <- c(result, res)
    }
    dup <- duplicated(sapply(result, deparse))
    result[!dup]
}

arrangeWindows <- function(action=c("vertical", "horizontal","cascade",
                                    "minimize", "restore"),
                           windows, preserve=TRUE, outer=FALSE) {
    action <- match.arg(action)
    action <- which(action == c("cascade", "horizontal", "vertical", "minimize", "restore"))
    stopifnot(length(action) == 1 && !is.na(action))

    if (missing(windows)) {
    	args <- if (exists(".arrangeWindowsDefaults", globalenv()))
            get(".arrangeWindowsDefaults", globalenv())
        else
            list()
        if (action == 5) # restore
            args$minimized <- TRUE
    	windows <- do.call(getWindowsHandles, args)
    }
   .Internal(arrangeWindows(windows, action, preserve, outer))
}

menuShowCRAN <- function()
{
    CRAN <- getOption("repos")[["CRAN"]] # drop name for identical()
    if(is.na(CRAN) || identical(CRAN, "@CRAN@"))
        CRAN <- "http://cran.r-project.org"
    shell.exec(CRAN)
}

shortPathName <- function(path) .Internal(shortPathName(path))

readRegistry <-
    function(key, hive=c("HLM", "HCR", "HCU", "HU", "HCC", "HPD"),
             maxdepth = 1, view = c("default", "32-bit", "64-bit"))
{
    view <- match(match.arg(view), c("default", "32-bit", "64-bit"))
    .Internal(readRegistry(key, match.arg(hive), maxdepth, view))
}

setInternet2 <- function(use = TRUE)
    .Internal(useInternet2(use))
