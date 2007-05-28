print.socket <- function(x, ...)
{
    if(length(as.integer(x$socket)) != 1)
	stop("invalid 'socket' argument")
    cat("Socket connection #", x$socket, "to", x$host,
	"on port", x$port, "\n")
    invisible(x)
}

make.socket <- function(host = "localhost", port, fail = TRUE, server = FALSE)
{
    if(length(port <- as.integer(port)) != 1)
	stop("'port' must be integer of length 1")
    if(length(host <- as.character(host)) != 1)
	stop("'host' must be character of length 1")
    if (!server){
	tmp2 <- .C("Rsockconnect",
                   port = port,
                   host = host,
                   PACKAGE = "base")
    }
    else{
	if (host != "localhost")
	    stop("can only receive calls on local machine")
	tmp <- .C("Rsockopen", port = port, PACKAGE="base")
	buffer <- paste(rep.int("#",256), collapse = "")
	tmp2 <- .C("Rsocklisten", port = tmp$port,
                   buffer = buffer, len = as.integer(256), PACKAGE="base")
	host <- substr(tmp2$buffer, 1, tmp2$len)
	.C("Rsockclose", tmp$port, PACKAGE="base")
    }
    if (tmp2$port <= 0) {
	if (fail) stop("socket not established")
        else warning("socket not established")
    }
    rval <- list(socket = tmp2$port, host = host, port = port)
    class(rval) <- "socket"
    rval
}

close.socket <- function(socket, ...)
{
    if(length(port <- as.integer(socket$socket)) != 1)
	stop("invalid 'socket' argument")
    as.logical(.C("Rsockclose", port, PACKAGE="base")[[1]])
}

read.socket <- function(socket, maxlen=256, loop=FALSE)
{
    if(length(port <- as.integer(socket$socket)) != 1)
	stop("invalid 'socket' argument")
    maxlen <- as.integer(maxlen)
    buffer <- paste(rep.int("#",maxlen), collapse="")
    repeat {
	tmp <- .C("Rsockread", port,
		  buffer = buffer, len = maxlen, PACKAGE="base")
	rval <- substr(tmp$buffer, 1, tmp$len)
	if (nzchar(rval) || !loop) break
    }
    rval
}

write.socket <- function(socket, string)
{
    if(length(port <- as.integer(socket$socket)) != 1)
	stop("invalid 'socket' argument")
    strlen <- length(strsplit(string,NULL)[[1]])
    invisible(.C("Rsockwrite", port, string,
		 as.integer(0), strlen, strlen, PACKAGE="base")[[5]])
}


