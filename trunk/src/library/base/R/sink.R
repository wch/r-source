sink <- function(file=NULL, append = FALSE, type = c("output", "message"),
                 split=FALSE)
{
    type <- match.arg(type)
    if(type == "message") {
        if(is.null(file)) file <- stderr()
        else if(!inherits(file, "connection") || !isOpen(file))
           stop("'file' must be NULL or an already open connection")
        if (split) stop("cannot split the message connection")
        .Internal(sink(file, FALSE, TRUE, FALSE))
    } else {
        closeOnExit <- FALSE
        if(is.null(file)) file <- -1
        else if(is.character(file)) {
            file <- file(file, ifelse(append, "a", "w"))
            closeOnExit <- TRUE
        } else if(!inherits(file, "connection"))
            stop("'file' must be NULL, a connection or a character string")
        .Internal(sink(file, closeOnExit, FALSE,split))
    }
}

sink.number <- function(type = c("output", "message"))
{
    type <- match.arg(type)
    .Internal(sink.number(type != "message"))
}
