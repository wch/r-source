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
