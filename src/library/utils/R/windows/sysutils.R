#  File src/library/utils/R/windows/sysutils.R
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

menuShowCRAN <- function()
{
    CRAN <- as.vector(getOption("repos")["CRAN"])
    if(is.na(CRAN) || identical(CRAN, "@CRAN@"))
        CRAN <- "http://cran.r-project.org"
    shell.exec(CRAN)
}

shortPathName <- function(path) .Internal(shortPathName(path))

readRegistry <-
    function(key, hive=c("HLM", "HCR", "HCU", "HU", "HCC", "HPD"), maxdepth = 1)
    .Internal(readRegistry(key, match.arg(hive), maxdepth))
