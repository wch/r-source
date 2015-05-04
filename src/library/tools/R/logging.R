#  File src/library/tools/R/logging.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1995-2015 The R Core Team
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

### emulation of Perl Logfile.pm

newLog <-
function(filename = "")
{
    con <- if(nzchar(filename)) file(filename, "wt") else 0L

    Log <- new.env(parent = emptyenv())
    Log$con <- con
    Log$filename <- filename
    Log$stars <- "*"
    Log$errors <- 0L
    Log$warnings <- 0L
    Log$notes <- 0L

    Log
}

closeLog <-
function(Log)
    if(Log$con > 2L) close(Log$con)

printLog <-
function(Log, ...)
{
    quotes <- function(x) gsub("'([^']*)'", sQuote("\\1"), x)
    args <- lapply(list(...), quotes)
    do.call(cat, c(args, sep = ""))
    if (Log$con > 0L) do.call(cat, c(args, sep = "", file = Log$con))
}

printLog0 <-
function(Log, ...)
{
    cat(..., sep = "")
    if (Log$con > 0L) cat(..., file = Log$con, sep = "")
}

## unused
## setStars <- function(Log, stars) {Log$stars <- stars; Log}

checkingLog <-
function(Log, ...)
    printLog(Log, Log$stars, " checking ", ..., " ...")

creatingLog <-
function(Log, text)
    printLog(Log, Log$stars, " creating ", text, " ...")

messageLog <-
function(Log, ...)
    printLog(Log, Log$stars, " ", ..., "\n")

resultLog <-
function(Log, text)
    printLog(Log, " ", text, "\n")

errorLog <-
function(Log, ...)
{
    resultLog(Log, "ERROR")
    text <- paste0(...)
    if (length(text) && nzchar(text)) printLog(Log, ..., "\n")
    Log$errors <- Log$errors + 1L
}

## <NOTE>
## Perhaps the arguments to errorLog(), warningLog() and noteLog()
## should be synchronized?
## </NOTE>

warningLog <-
function(Log, text = "")
{
    resultLog(Log, "WARNING")
    if(nzchar(text)) printLog(Log, text, "\n")
    Log$warnings <- Log$warnings + 1L
}

noteLog <-
function(Log, text = "")
{
    resultLog(Log, "NOTE")
    if(nzchar(text)) printLog(Log, text, "\n")
    Log$notes <- Log$notes + 1L
}

summaryLog <-
function(Log)
{
    messageLog(Log, "DONE")
    message("")
    counts <- c(ERROR = Log$errors,
                WARNING = Log$warnings,
                NOTE = Log$notes)
    counts <- counts[counts > 0L]
    if(!length(counts))
        printLog(Log,
                 "Status: OK\n")
    else {
        printLog(Log,
                 sprintf("Status: %s\n",
                         paste(sprintf("%d %s%s",
                                       counts,
                                       names(counts),
                                       ifelse(counts > 1L, "s", "")),
                               collapse = ", ")))
        message(sprintf("See\n  %s\nfor details.\n", sQuote(Log$filename)))
    }
}
