#  File src/library/utils/R/sock.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1995-2012 The R Core Team
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

print.socket <- function(x, ...)
{
    if(length(as.integer(x$socket)) != 1L)
	stop("invalid 'socket' argument")
    cat("Socket connection #", x$socket, "to", x$host,
	"on port", x$port, "\n")
    invisible(x)
}

make.socket <- function(host = "localhost", port, fail = TRUE, server = FALSE)
{
    if(length(port <- as.integer(port)) != 1L)
	stop("'port' must be integer of length 1")
    if(length(host <- as.character(host)) != 1L)
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
	buffer <- paste(rep.int("#",256L), collapse = "")
	tmp2 <- .C("Rsocklisten", port = tmp$port,
                   buffer = buffer, len = 256L, PACKAGE="base")
	host <- substr(tmp2$buffer, 1L, tmp2$len)
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
    if(length(port <- as.integer(socket$socket)) != 1L)
	stop("invalid 'socket' argument")
    as.logical(.C("Rsockclose", port, PACKAGE="base")[[1L]])
}

read.socket <- function(socket, maxlen=256, loop=FALSE)
{
    if(length(port <- as.integer(socket$socket)) != 1L)
	stop("invalid 'socket' argument")
    maxlen <- as.integer(maxlen)
    buffer <- paste(rep.int("#",maxlen), collapse="")
    repeat {
	tmp <- .C("Rsockread", port,
		  buffer = buffer, len = maxlen, PACKAGE="base")
	rval <- substr(tmp$buffer, 1L, tmp$len)
	if (nzchar(rval) || !loop) break
    }
    rval
}

write.socket <- function(socket, string)
{
    if(length(port <- as.integer(socket$socket)) != 1L)
	stop("invalid 'socket' argument")
    strlen <- length(strsplit(string,NULL)[[1L]])
    invisible(.C("Rsockwrite", port, string, 0L, strlen, strlen,
                 PACKAGE="base")[[5]])
}


