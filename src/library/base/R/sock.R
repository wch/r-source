print.socket <- function(x, ...)
{
    cat("Socket connection #", x$socket, "to", x$host,
        "on port", x$port, "\n")
    invisible(x)
}

make.socket <-
    function(host = "localhost", port, fail = TRUE, server = FALSE)
{
    if (!server){
        tmp2 <- .C("Rsockconnect",port = as.integer(port), host = host)
    }
    else{
        if (host != "localhost")
            stop("Can only receive calls on local machine")
        tmp <- .C("Rsockopen", port = as.integer(port))
        buffer <- paste(rep("#",256), collapse = "")
        tmp2 <- .C("Rsocklisten", port = as.integer(tmp[[1]]),
                 buffer = buffer, len = as.integer(256))
        host <- substr(tmp2$buffer, 1, tmp2$len)
        .C("Rsockclose", as.integer(tmp$port))
    }
    if (tmp2$port <= 0){
        if (fail)
            stop("Socket not established")
        else
            warning("Socket not established")
    }
    rval <- list(socket = tmp2$port, host = host, port = port)
    class(rval) <- "socket"
    rval
}

close.socket <- function(socket)
{
    as.logical(.C("Rsockclose", as.integer(socket$socket))[[1]])
}

read.socket <- function(socket, maxlen=256, loop=FALSE)
{
    buffer <- paste(rep("#",maxlen), collapse="")
    repeat {
        tmp <- .C("Rsockread", as.integer(socket$socket),
                  buffer = buffer, len = as.integer(maxlen))
        rval <- substr(tmp$buffer, 1, tmp$len)
        if ((rval > 0) | !loop) break
  }
  rval
}

write.socket <- function(socket, string)
{
    strlen <- length(strsplit(string,NULL)[[1]])
    invisible(.C("Rsockwrite", as.integer(socket$socket), string,
                 as.integer(0), as.integer(strlen), as.integer(strlen))[[5]])
}


