stdin <- function() .Internal(stdin())
stdout <- function() .Internal(stdout())
stderr <- function() .Internal(stderr())

readLines <- function(con = stdin(), n = -1, ok = TRUE)
{
    if(is.character(con)) con <- file(con, "r")
    .Internal(readLines(con, n, ok))
}


writeLines <- function(text, con = stdout(), sep = "\n")
{
    if(is.character(con)) con <- file(con, "w")
    invisible(.Internal(writeLines(text, con, sep)))
}

open <- function(con, ...)
    UseMethod("open")

open.default <- function(con, open = "r", blocking = TRUE)
    invisible(.Internal(open(con, open, blocking)))

isOpen <- function(con, rw = "")
{
    if(!inherits(con, "connection")) stop("argument is not a connection")
    .Internal(isOpen(con, rw))
}

isIncomplete <- function(con)
    .Internal(isIncomplete(con))

close <- function(con, ...)
    UseMethod("close")

close.connection <- function (con, type = "rw")
{
    if(!inherits(con, "connection")) stop("argument is not a connection")
    invisible(.Internal(close(con, type)))
}

file <- function(description, open = "", blocking = TRUE)
    .Internal(file(description, open, blocking))

textConnection <- function(object, open = "")
    .Internal(textConnection(deparse(substitute(object)), object, open))

seek <- function(con, where = NA, rw = "")
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
    colnames(ans) <- c("class", "description", "mode", "text", "isopen",
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
