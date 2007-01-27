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
        if(which > 0 && .Devices[[which]] != "windows")
            return(0)
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
