#  File src/library/base/R/serialize.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2019 The R Core Team
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
#  https://www.R-project.org/Licenses/

saveRDS <-
    function(object, file = "", ascii = FALSE, version = NULL,
             compress = TRUE, refhook = NULL)
{
    if(is.character(file)) {
	if(file == "") stop("'file' must be non-empty string")
	object <- object # do not create corrupt file if object does not exist
	mode <- if(ascii %in% FALSE) "wb" else "w"
	con <- if (is.logical(compress))
		   if(compress) gzfile(file, mode) else file(file, mode)
	       else
		   switch(compress,
			  "bzip2" = bzfile(file, mode),
			  "xz"    = xzfile(file, mode),
			  "gzip"  = gzfile(file, mode),
			  stop("invalid 'compress' argument: ", compress))
        on.exit(close(con))
    }
    else if(inherits(file, "connection")) {
        if (!missing(compress))
            warning("'compress' is ignored unless 'file' is a file name")
        con <- file
    }
    else
        stop("bad 'file' argument")
    .Internal(serializeToConn(object, con, ascii, version, refhook))
}

readRDS <- function(file, refhook = NULL)
{
    if(is.character(file)) {
        con <- gzfile(file, "rb")
        on.exit(close(con))
    } else if (inherits(file, "connection"))
	con <- if(inherits(file, "url")) gzcon(file) else file
    else stop("bad 'file' argument")
    .Internal(unserializeFromConn(con, refhook))
}

infoRDS <- function(file)
{
    if(is.character(file)) {
        con <- gzfile(file, "rb")
        on.exit(close(con))
    } else if (inherits(file, "connection"))
        con <- if(inherits(file, "url")) gzcon(file) else file
    else stop("bad 'file' argument")
    .Internal(serializeInfoFromConn(con))
}

serialize <-
    function(object, connection, ascii = FALSE, xdr = TRUE,
             version = NULL, refhook = NULL)
{
    if (!is.null(connection)) {
        if (!inherits(connection, "connection"))
            stop("'connection' must be a connection")
        if (missing(ascii)) ascii <- summary(connection)$text == "text"
    }
    if (!ascii && inherits(connection, "sockconn"))
        .Internal(serializeb(object, connection, xdr, version, refhook))
    else {
	type <- if(is.na(ascii)) 2L else if(ascii) 1L else if(!xdr) 3L else 0L
        .Internal(serialize(object, connection, type, version, refhook))
    }
}

unserialize <- function(connection, refhook = NULL)
{
    if (typeof(connection) != "raw" &&
        !is.character(connection) &&
        !inherits(connection, "connection"))
        stop("'connection' must be a connection")
    .Internal(unserialize(connection, refhook))
}
