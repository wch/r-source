stdin <- function() .Internal(stdin())
stdout <- function() .Internal(stdout())
stderr <- function() .Internal(stderr())

readLines <- function(con = stdin(), n = -1, ok = TRUE)
{
    if(is.character(con)) {
        con <- file(con, "r")
        on.exit(close(con))
    }
    .Internal(readLines(con, n, ok))
}


writeLines <- function(text, con = stdout(), sep = "\n")
{
    if(is.character(con)) {
        con <- file(con, "w")
        on.exit(close(con))
    }
    invisible(.Internal(writeLines(text, con, sep)))
}

open <- function(con, ...)
    UseMethod("open")

open.connection <- function(con, open = "r", blocking = TRUE, ...)
{
    invisible(.Internal(open(con, open, blocking)))
}

isOpen <- function(con, rw = "")
{
    rw <- pmatch(rw, c("read", "write"), 0)
    .Internal(isOpen(con, rw))
}

isIncomplete <- function(con)
    .Internal(isIncomplete(con))

isSeekable <- function(con)
    .Internal(isSeekable(con))

close <- function(con, ...)
    UseMethod("close")

close.connection <- function (con, type = "rw", ...)
    invisible(.Internal(close(con, type)))

flush <- function(con) UseMethod("flush")

flush.connection <- function (con)
    invisible(.Internal(flush(con)))

file <- function(description = "", open = "", blocking = TRUE,
                 encoding = getOption("encoding"))
    .Internal(file(description, open, blocking, encoding))

pipe <- function(description, open = "", encoding = getOption("encoding"))
    .Internal(pipe(description, open, encoding))

fifo <- function(description = "", open = "", blocking = FALSE,
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

bzfile <- function(description, open = "", encoding = getOption("encoding"))
    .Internal(bzfile(description, open, encoding))

socketConnection <- function(host= "localhost", port, server = FALSE,
                             blocking = FALSE, open = "a+",
                             encoding = getOption("encoding"))
    .Internal(socketConnection(host, port, server, blocking, open, encoding))

textConnection <- function(object, open = "r", local = FALSE) {
    if (local) env <- parent.frame()
    else env <- .GlobalEnv
    .Internal(textConnection(deparse(substitute(object)), object, open, env))
}

seek <- function(con, ...)
    UseMethod("seek")

seek.connection <- function(con, where = NA, origin = "start", rw = "", ...)
{
    origin <- pmatch(origin, c("start", "current", "end"))
    rw <- pmatch(rw, c("read", "write"), 0)
    if(is.na(origin))
        stop("`origin' must be one of `start', `current` or `end'")
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
    invisible(.Internal(pushBack(data, connection, newLine)))

pushBackLength <- function(connection)
    .Internal(pushBackLength(connection))

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
    if(!all) set <- set[set > 2]
    ans <- matrix("", length(set), 7)
    for(i in seq(along=set)) ans[i, ] <- unlist(summary.connection(set[i]))
    rownames(ans) <- set
    colnames(ans) <- c("description", "class", "mode", "text", "isopen",
                       "can read", "can write")
    if(!all) ans[ans[, 5] == "opened", , drop = FALSE]
    else ans[, , drop = FALSE]
}

getAllConnections <- function()
    .Internal(getAllConnections())

getConnection <- function(what)
{
    set <- getAllConnections()
    if(what %in% set) structure(what, class="connection")
    else NULL
}

closeAllConnections <- function()
{
    # first re-divert any diversion of stderr.
    i <- sink.number(type = "message")
    if(i > 0) sink(stderr(), type = "message")
    # now unwind the sink diversion stack.
    n <- sink.number()
    if(n > 0) for(i in 1:n) sink()
    # get all the open connections.
    set <- getAllConnections()
    set <- set[set > 2]
    # and close all user connections.
    for(i in seq(along=set)) close(getConnection(set[i]))
    invisible()
}

readBin <- function(con, what, n = 1, size = NA, signed = TRUE,
                    endian = .Platform$endian)
{
    if(is.character(con)) {
        con <- file(con, "rb")
        on.exit(close(con))
    }
    swap <- endian != .Platform$endian
    if(!is.character(what) || length(what) != 1
    	|| !(what %in% c("numeric", "double", "integer", "int", "logical",
                         "complex", "character", "raw")))
        what <- typeof(what)
    .Internal(readBin(con, what, n, size, signed, swap))
}

writeBin <- function(object, con, size = NA, endian = .Platform$endian)
{
    swap <- endian != .Platform$endian
    if(!is.vector(object) || mode(object) == "list")
        stop("can only write vector objects")
    if(is.character(con)) {
        con <- file(con, "wb")
        on.exit(close(con))
    }
    invisible(.Internal(writeBin(object, con, size, swap)))
}

## encoding vectors
native.enc <- 0:255
# rest in Rprofile.*

readChar <- function(con, nchars)
{
    if(is.character(con)) {
        con <- file(con, "rb")
        on.exit(close(con))
    }
    .Internal(readChar(con, as.integer(nchars)))
}

writeChar <- function(object, con, nchars = nchar(object, type="bytes"),
                      eos = "")
{
    if(!is.character(object))
        stop("can only write character objects")
    if(is.character(con)) {
        con <- file(con, "wb")
        on.exit(close(con))
    }
    invisible(.Internal(writeChar(object, con, as.integer(nchars), eos)))
}

gzcon <- function(con, level = 6, allowNonCompressed = TRUE)
    .Internal(gzcon(con, level, allowNonCompressed))

socketSelect <- function(socklist, write = FALSE, timeout = NULL) {
    if (is.null(timeout))
        timeout <- -1
    else if (timeout < 0)
        stop("supplied timeout must be NULL or a non-negative number")
    if (length(write) < length(socklist))
        write <- rep(write, length.out = length(socklist))
    .Internal(sockSelect(socklist, write, timeout))
}
