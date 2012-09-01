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
    if(length(as.integer(x$socket)) != 1L) stop("invalid 'socket' argument")
    cat("Socket connection #", x$socket, "to", x$host, "on port", x$port, "\n")
    invisible(x)
}

make.socket <- function(host = "localhost", port, fail = TRUE, server = FALSE)
{
    if(length(port <- as.integer(port)) != 1L)
	stop("'port' must be integer of length 1")
    if(length(host <- as.character(host)) != 1L)
	stop("'host' must be character of length 1")
    if (!server){
	socket <- .Call("Rsockconnect", port, host, PACKAGE = "base")
    } else {
	if (host != "localhost") stop("can only receive calls on local machine")
	tmp <- .Call("Rsockopen", port, PACKAGE="base")
        socket <- .Call("Rsocklisten", tmp, PACKAGE="base")
	.Call("Rsockclose", tmp, PACKAGE="base")
    }
    if (socket <= 0) {
	if (fail) stop("socket not established")
        else warning("socket not established")
    }
    rval <- list(socket = socket, host = attr(socket, "host"), port = port)
    class(rval) <- "socket"
    rval
}

close.socket <- function(socket, ...)
    .Call("Rsockclose", socket$socket, PACKAGE = "base")

read.socket <- function(socket, maxlen = 256L, loop = FALSE)
{
    repeat {
	rval <- .Call("Rsockread", socket$socket, maxlen, PACKAGE = "base")
	if (nzchar(rval) || !loop) break
    }
    rval
}

write.socket <- function(socket, string)
    invisible(.Call("Rsockwrite", socket$socket, string, PACKAGE = "base"))

