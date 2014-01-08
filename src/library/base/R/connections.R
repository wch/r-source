#  File src/library/base/R/connections.R
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

stdin <- function() .Internal(stdin())
stdout <- function() .Internal(stdout())
stderr <- function() .Internal(stderr())

isatty <- function(con) {
    if (!inherits(con, "terminal")) FALSE
    else .Internal(isatty(con))
}

readLines <- function(con = stdin(), n = -1L, ok = TRUE, warn = TRUE,
                      encoding = "unknown", skipNul = FALSE)
{
    if(is.character(con)) {
        con <- file(con, "r")
        on.exit(close(con))
    }
    .Internal(readLines(con, n, ok, warn, encoding, skipNul))
}


writeLines <- function(text, con = stdout(), sep = "\n", useBytes = FALSE)
{
    if(is.character(con)) {
        con <- file(con, "w")
        on.exit(close(con))
    }
    .Internal(writeLines(text, con, sep, useBytes))
}

open <- function(con, ...)
    UseMethod("open")

open.connection <- function(con, open = "r", blocking = TRUE, ...)
    .Internal(open(con, open, blocking))

isOpen <- function(con, rw = "")
{
    rw <- pmatch(rw, c("read", "write"), 0L)
    .Internal(isOpen(con, rw))
}

isIncomplete <- function(con)
    .Internal(isIncomplete(con))

isSeekable <- function(con)
    .Internal(isSeekable(con))

close <- function(con, ...)
    UseMethod("close")

close.connection <- function (con, type = "rw", ...)
    .Internal(close(con, type))

flush <- function(con) UseMethod("flush")

flush.connection <- function (con)
    .Internal(flush(con))

file <- function(description = "", open = "", blocking = TRUE,
                 encoding = getOption("encoding"), raw = FALSE)
    .Internal(file(description, open, blocking, encoding, raw))

pipe <- function(description, open = "", encoding = getOption("encoding"))
    .Internal(pipe(description, open, encoding))

fifo <- function(description, open = "", blocking = FALSE,
                 encoding = getOption("encoding"))
    .Internal(fifo(description, open, blocking, encoding))

url <- function(description, open = "", blocking = TRUE,
                encoding = getOption("encoding"))
    .Internal(url(description, open, blocking, encoding))

gzfile <- function(description, open = "",
                   encoding = getOption("encoding"), compression = 6)
    .Internal(gzfile(description, open, encoding, compression))

unz <- function(description, filename, open = "",
                encoding = getOption("encoding"))
    .Internal(unz(paste(description, filename, sep=":"), open, encoding))

bzfile <- function(description, open = "", encoding = getOption("encoding"),
                   compression = 9)
    .Internal(bzfile(description, open, encoding, compression))

xzfile <- function(description, open = "", encoding = getOption("encoding"),
                   compression = 6)
    .Internal(xzfile(description, open, encoding, compression))

socketConnection <- function(host = "localhost", port, server = FALSE,
                             blocking = FALSE, open = "a+",
                             encoding = getOption("encoding"),
                             timeout = getOption("timeout"))
    .Internal(socketConnection(host, port, server, blocking, open, encoding,
                               timeout))

rawConnection <- function(object, open = "r") {
    .Internal(rawConnection(deparse(substitute(object)), object, open))
}

rawConnectionValue <- function(con) .Internal(rawConnectionValue(con))

textConnection <- function(object, open = "r", local = FALSE,
                           encoding = c("", "bytes", "UTF-8"))
{
    env <- if (local) parent.frame() else .GlobalEnv
    type <- match(match.arg(encoding), c("", "bytes", "UTF-8"))
    nm <- deparse(substitute(object))
    if(length(nm) != 1)
        stop("argument 'object' must deparse to a single character string")
    .Internal(textConnection(nm, object, open, env, type))
}

textConnectionValue <- function(con) .Internal(textConnectionValue(con))

seek <- function(con, ...)
    UseMethod("seek")

seek.connection <- function(con, where = NA, origin = "start", rw = "", ...)
{
    origin <- pmatch(origin, c("start", "current", "end"))
    rw <- pmatch(rw, c("read", "write"), 0L)
    if(is.na(origin))
        stop("'origin' must be one of 'start', 'current' or 'end'")
    .Internal(seek(con, as.double(where), origin, rw))
}

truncate <- function(con, ...)
    UseMethod("truncate")

truncate.connection <- function(con, ...)
{
    if(!isOpen(con)) stop("can only truncate an open connection")
    .Internal(truncate(con))
}

pushBack <- function(data, connection, newLine = TRUE)
    .Internal(pushBack(data, connection, newLine))

pushBackLength <- function(connection)
    .Internal(pushBackLength(connection))

clearPushBack <- function(connection)
    .Internal(clearPushBack(connection))

print.connection <- function(x, ...)
{
    print(unlist(summary(x)))
    invisible(x)
}

summary.connection <- function(object, ...)
    .Internal(summary.connection(object))

showConnections <- function(all = FALSE)
{
    set <- getAllConnections()
    if(!all) set <- set[set > 2L]
    ans <- matrix("", length(set), 7L)
    for(i in seq_along(set)) ans[i, ] <- unlist(summary.connection(set[i]))
    rownames(ans) <- set
    colnames(ans) <- c("description", "class", "mode", "text", "isopen",
                       "can read", "can write")
    if(!all) ans[ans[, 5L] == "opened", , drop = FALSE]
    else ans[, , drop = FALSE]
}

getAllConnections <- function()
    .Internal(getAllConnections())

getConnection <- function(what) .Internal(getConnection(what))

closeAllConnections <- function()
{
    # first re-divert any diversion of stderr.
    i <- sink.number(type = "message")
    if(i > 0L) sink(stderr(), type = "message")
    # now unwind the sink diversion stack.
    n <- sink.number()
    if(n > 0L) for(i in seq_len(n)) sink()
    # get all the open connections.
    set <- getAllConnections()
    set <- set[set > 2L]
    # and close all user connections.
    for(i in seq_along(set)) close(getConnection(set[i]))
    invisible()
}

readBin <- function(con, what, n = 1L, size = NA_integer_, signed = TRUE,
                    endian = .Platform$endian)
{
    if(is.character(con)) {
        con <- file(con, "rb")
        on.exit(close(con))
    }
    swap <- endian != .Platform$endian
    if(!is.character(what) || is.na(what) ||
       length(what) != 1L || ## hence length(what) == 1:
       !any(what == c("numeric", "double", "integer", "int", "logical",
	    "complex", "character", "raw")))
	what <- typeof(what)
    .Internal(readBin(con, what, n, size, signed, swap))
}

writeBin <-
    function(object, con, size = NA_integer_, endian = .Platform$endian,
             useBytes = FALSE)
{
    swap <- endian != .Platform$endian
    if(!is.vector(object) || mode(object) == "list")
        stop("can only write vector objects")
    if(is.character(con)) {
        con <- file(con, "wb")
        on.exit(close(con))
    }
    .Internal(writeBin(object, con, size, swap, useBytes))
}

readChar <- function(con, nchars, useBytes = FALSE)
{
    if(is.character(con)) {
        con <- file(con, "rb")
        on.exit(close(con))
    }
    .Internal(readChar(con, as.integer(nchars), useBytes))
}

writeChar <- function(object, con, nchars = nchar(object, type="chars"),
                      eos = "", useBytes = FALSE)
{
    if(!is.character(object))
        stop("can only write character objects")
    if(is.character(con)) {
        con <- file(con, "wb")
        on.exit(close(con))
    }
    .Internal(writeChar(object, con, as.integer(nchars), eos, useBytes))
}

gzcon <- function(con, level = 6, allowNonCompressed = TRUE)
    .Internal(gzcon(con, level, allowNonCompressed))

socketSelect <- function(socklist, write = FALSE, timeout = NULL) {
    if (is.null(timeout))
        timeout <- -1
    else if (timeout < 0)
        stop("'timeout' must be NULL or a non-negative number")
    if (length(write) < length(socklist))
        write <- rep_len(write, length(socklist))
    .Internal(sockSelect(socklist, write, timeout))
}

memCompress <-
    function(from, type = c("gzip", "bzip2", "xz", "none"))
{
    if(is.character(from))
        from <- charToRaw(paste(from, collapse = "\n"))
    else if(!is.raw(from)) stop("'from' must be raw or character")
    type <- match(match.arg(type), c("none", "gzip", "bzip2", "xz"))
    .Internal(memCompress(from, type))
}

memDecompress <-
    function(from,
             type = c("unknown", "gzip", "bzip2", "xz", "none"),
             asChar = FALSE)
{
    type <- match(match.arg(type),
                  c("none", "gzip", "bzip2", "xz", "unknown"))
    ans <- .Internal(memDecompress(from, type))
    if(asChar) rawToChar(ans) else ans
}
