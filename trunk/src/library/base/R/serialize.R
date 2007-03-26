.saveRDS <-
function(object, file = "", ascii = FALSE, version = NULL,
         compress = TRUE, refhook = NULL)
{
    if(is.character(file)) {
        if(file == "") stop("'file' must be non-empty string")
        mode <- if(ascii) "w" else "wb"
        con <- if(compress) gzfile(file, mode) else file(file, mode)
        on.exit(close(con))
    }
    else if(inherits(file, "connection")) {
        con <- file
        if(missing(ascii)) ascii <- summary(con)$text == "text"
    }
    else
        stop("bad 'file' argument")
    invisible(.Internal(serializeToConn(object, con, ascii, version, refhook)))
}

.readRDS <-
function(file, refhook = NULL)
{
    if(is.character(file)) {
        con <- gzfile(file, "rb")
        on.exit(close(con))
    }
    else if(inherits(file, "connection"))
        con <- gzcon(file)
    else
        stop("bad 'file' argument")
    .Internal(unserializeFromConn(con, refhook))
}

serialize <- function(object, connection, ascii = FALSE, refhook = NULL)
{
    if (!is.null(connection)) {
        if (!inherits(connection, "connection"))
            stop("'connection' must be a connection")
        if (missing(ascii)) ascii <- summary(connection)$text == "text"
    }
    if (!ascii && inherits(connection, "sockconn"))
        .Call("R_serializeb", object, connection, refhook, PACKAGE="base")
    else
        .Call("R_serialize", object, connection, ascii, refhook,
              PACKAGE="base")
}

unserialize <- function(connection, refhook = NULL)
{
    if (typeof(connection) != "raw" &&
        !is.character(connection) &&
        !inherits(connection, "connection"))
        stop("'connection' must be a connection")
    .Call("R_unserialize", connection, refhook, PACKAGE="base")
}
