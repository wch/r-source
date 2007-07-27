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

memory.size <- function(max = FALSE) .Internal(memory.size(max))

memory.limit <- function(size = NA) .Internal(memory.size(size))

DLL.version <- function(path) .Internal(DLL.version(path))

getClipboardFormats <- function()
    .Internal(getClipboardFormats())

readClipboard <- function(format = 1, raw = FALSE) {
    result <- .Internal(readClipboard(format, raw))
    if (!is.null(result) && !raw) {
    	result <- strsplit(result,"\r\n")[[1]]
    	if (length(result) == 1) result <- strsplit(result,"\n")[[1]]
    	if (length(result) == 1) result <- strsplit(result,"\r")[[1]]
    }
    result
}

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
