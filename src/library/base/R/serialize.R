.saveRDS <-
function(object, file = "", ascii = FALSE, version = NULL,
         compress = FALSE, refhook = NULL) 
{
    if(is.character(file)) {
        if(file == "") stop("'file' must be non-empty string")
        mode <- if(ascii) "w" else "wb"
        if(compress && capabilities("libz")) con <- gzfile(file, mode)
        else con <- file(file, mode)
        on.exit(close(con))
    }
    else if (inherits(file, "connection")) {
        con <- file
        if(missing(ascii)) 
            if(summary(con)$text == "text") 
                ascii <- TRUE
            else ascii <- FALSE
    }        
    else stop("bad 'file' argument")
    invisible(.Internal(serializeToConn(object, con, ascii, version,
                                        refhook)))
}

.readRDS <-
function(file, refhook = NULL) 
{
    if(is.character(file)) {
        if(capabilities("libz")) 
            con <- gzfile(file, "rb")
        else
            con <- file(file, "rb")
        on.exit(close(con))
    }
    else if (inherits(file, "connection")) 
        con <- file
    else stop("bad 'file' argument")
    .Internal(unserializeFromConn(con, refhook))
}

serialize <- function(object, connection, ascii = FALSE, refhook = NULL) {
    if (! is.null(connection)) {
        if (!inherits(connection, "connection")) 
            stop("`connection' must be a connection")
        if (missing(ascii))
            if (summary(connection)$text == "text")
                ascii <- TRUE
            else
                ascii <- FALSE
    }
    if (! ascii && inherits(connection, "sockconn"))
        .Call("R_serializeb", object, connection, refhook, PACKAGE="base")
    else
        .Call("R_serialize", object, connection, ascii, refhook,
              PACKAGE="base")
}

unserialize <- function(connection, refhook = NULL) {
    if (! is.character(connection) && !inherits(connection, "connection")) 
        stop("`connection' must be a connection")
    .Call("R_unserialize", connection, refhook, PACKAGE="base")
}
