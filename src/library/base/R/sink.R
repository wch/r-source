sink <- function(file=NULL, append = FALSE, type = c("output", "message"))
{
    type <- match.arg(type)
    if(type == "message") {
        if(!inherits(file, "connection") || !isopen(connection))
            error("`file' must be an already open connection")
        .Internal(sink(file, FALSE, TRUE))
    } else {
        closeOnExit <- FALSE
        if(is.null(file)) file <- -1
        else if(is.character(file)) {
            file <- file(file, ifelse(append, "a", "w"))
            closeOnExit <- TRUE
        } else if(!inherits(file, "connection"))
            stop("`file' must be NULL, a connection or a character string")
        .Internal(sink(file, closeOnExit, FALSE))
    }
}

sink.number <- function(type = c("output", "message"))
{
    type <- match.arg(type)
    .Internal(sink.number(type == "message"))
}
