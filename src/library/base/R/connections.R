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

open.connection <- function(con, open = "r", blocking = TRUE)
{
    if(!inherits(con, "connection")) stop("argument is not a connection")
    invisible(.Internal(open(con, open, blocking)))
}

isOpen <- function(con, rw = "")
{
    if(!inherits(con, "connection")) stop("argument is not a connection")
    .Internal(isOpen(con, rw))
}

isIncomplete <- function(con)
    .Internal(isIncomplete(con))

isSeekable <- function(con)
    .Internal(isSeekable(con))

close <- function(con, ...)
    UseMethod("close")

close.connection <- function (con, type = "rw")
{
    if(!inherits(con, "connection")) stop("argument is not a connection")
    invisible(.Internal(close(con, type)))
}

file <- function(description, open = "", blocking = TRUE)
    .Internal(file(description, open, blocking))

pipe <- function(description, open = "")
    .Internal(pipe(description, open))

textConnection <- function(object, open = "r")
    .Internal(textConnection(deparse(substitute(object)), object, open))

seek <- function(con, ...)
    UseMethod("seek")

seek.connection <- function(con, where = NA, rw = "")
    .Internal(seek(con, as.integer(where), rw))

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
    sink() # might be on a user connection
    set <- getAllConnections()
    set <- set[set > 2]
    for(i in seq(along=set)) close(set[i])
    invisible()
}

readBin <- function(con, what, n = 1, size = NA, endian = .Platform$endian)
{
    if(is.character(con)) {
        con <- file(con, "rb")
        on.exit(close(con))
    }
    swap <- endian != .Platform$endian
    if(!is.character(what) || length(what) != 1) what <- typeof(what)
    .Internal(readBin(con, what, n, size, swap))
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
