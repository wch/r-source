parse.dcf <- .Deprecated

read.dcf <- function(file, fields=NULL){
    if (is.character(file)){
        file <- file(file, "r")
        on.exit(close(file))
    }
    if (!inherits(file, "connection")) 
        stop("argument `file' must be a character string or connection")
    .Internal(readDCF(file ,fields))
}

