###----- NOTE:	../man/Deprecated.Rd   must be synchronized with this!
###		--------------------
.Deprecated <- function(new) {
    warning(paste("`", as.character(sys.call(sys.parent())[[1]]), "' ",
		  "is deprecated.\n",
		  if (!missing(new))
		  paste("Use `", new, "' instead.\n", sep = ""),
		  "See ?Deprecated.",
		  sep = ""))
}

## Deprecated in 1.3.0
"httpclient" <-
    function (url, port = 80, error.is.fatal = TRUE, check.MIME.type = TRUE,
              file = tempfile(), drop.ctrl.z = TRUE)
{
    .Deprecated()
    allowed.MIME.types <- c("text/", "application/postscript",
                            "application/x-latex")
    urlel <- strsplit(url, "/")[[1]]
    if (urlel[1] != "http:")
        stop("Not an http:// URL")
    host <- urlel[3]
    rurl <- paste(c("", urlel[-(1:3)]), collapse = "/")
    a <- make.socket(host, port = port)
    on.exit(close.socket(a))
    headreq <- paste("HEAD", rurl, "HTTP/1.0\r\nConnection: Keep-Alive\r\nAccept: text/plain\r\n\r\n")
    write.socket(a, headreq)
    head <- read.socket(a, maxlen = 8000)
    b <- strsplit(head, "\n")[[1]]
    if (length(grep("200 OK", b[1])) == 0) {
        if (error.is.fatal)
            stop(b[1])
        else warning(b[1])
        return(file)
    }
    if (check.MIME.type && length(unlist(lapply(allowed.MIME.types,
                                                function(x) grep(x, strsplit(grep("Content-Type:", b,
                                                                                  value = TRUE), ":")[[1]][2])))) == 0) {
        if (error.is.fatal)
            stop(grep("Content-Type:", b, value = TRUE))
        else warning(grep("Content-Type:", b, value = TRUE))
        return(file)
    }
    len <- as.numeric(strsplit(grep("Content-Length", b, value = TRUE),
                               ":")[[1]][2])
    getreq <- paste("GET", rurl, "HTTP/1.0\r\nConnection: Keep-Alive\r\nAccept: text/plain\r\n\r\n")
    write.socket(a, getreq)
    junk <- read.socket(a, maxlen = nchar(head))
    data <- ""
    b <- strsplit(c(head, junk), "\n")
    nn <- length(b[[1]])
    if (length(b[[2]]) > nn)
        data <- paste(b[[2]][-(1:nn)], collapse = "\n")
    while (nchar(data) < len) {
        data <- paste(data, read.socket(a, maxlen = len - nchar(data)),
                      sep = "")
    }
    if (drop.ctrl.z)
        data <- gsub("\032", "", data, extended = FALSE)
    cat(data, file = file)
    return(file)
}

"read.table.url" <-
    function (url, method, ...)
{
    .Deprecated("read.table()")
    f <- tempfile()
    if (download.file(url, destfile=f, method=method) == 0) {
        data <- read.table(f, ...)
        unlink(f)
    } else {
        unlink(f)
        stop("transfer failure")
    }
    return(data)
}

"scan.url" <-
    function (url, file=tempfile(), method, ...)
{
    .Deprecated("scan()")
    if (download.file(url, dest=file, method=method) != 0){
        unlink(file)
        stop("transfer failed")
    }
    data <- scan(file, ...)
    unlink(file)
    return(data)
}

"source.url" <-
    function (url, file=tempfile(), method, ...)
{
    .Deprecated("source()")
    if (download.file(url, dest=file, method=method) != 0) {
        unlink(file)
        stop("transfer failure")
    }
    m <- match.call()
    m[[1]] <- as.name("source")
    m$url <- NULL
    m$port <- NULL
    m$file <- file
    eval(m, parent.frame())
    unlink(file)
}

parse.dcf <- function(text=NULL, file="", fields=NULL, versionfix=FALSE)
{
    .Deprecated("read.dcf")

    parse.dcf.entry <- function(text, fields=NULL, versionfix=FALSE)
    {
        contlines <- grep("^[ \t]+", text)

        if(is.null(fields)){
            if(length(contlines))
                fields <- sub("^([^:]*):.*$", "\\1", text[-contlines])
            else
                fields <- sub("^([^:]*):.*$", "\\1", text)
        }

        retval <- as.list(rep(NA, length(fields)))
        names(retval) <- fields

        for(d in 1:length(text)){
            if(any(contlines == d))
                y <- sub("^[ \t]+(.*)$", "\\1", text[d])
            else{
                x <- sub("^([^:]*):.*$", "\\1", text[d])
                y <- sub("^[^:]*:[ \t]*(.*)$", "\\1", text[d])
            }

            if(versionfix & x=="Version")
                y <- unlist(strsplit(y, " "))[1]

            if(any(fields==x))
                retval[[x]] <-
                    if(is.na(retval[[x]])) y
                    else paste(retval[[x]], y, sep="\n")
        }
        retval
    }

    if(missing(text))
        text <- scan(file=file, what="",  quote="", sep="\n", quiet=TRUE)

    if(length(text) == 0) {
        warning("zero length `text'")
        return(list())
    }

    ## remove empty lines
    notok <- grep("^[ \t]+$", text)
    if (length(notok) > 0)
        text <- text[-notok]

    ## use the field name of the first line as record separator
    recsep <- sub("^([^:]*):.*$", "\\1", text[1])

    start <- grep(paste("^", recsep, ":", sep=""), text)
    start <- c(start, length(text)+1)
    retval <- list()
    for(k in seq(length = length(start)-1)) {
        retval[[k]] <- parse.dcf.entry(text[start[k]:(start[k+1]-1)],
                                       fields = fields,
                                       versionfix = versionfix)
    }
   if(!is.null(fields))
        return( t(sapply(retval, unlist)) )
    if(length(retval) == 1)
        return( unlist(retval, recursive=FALSE) )
    ## else
    retval
}
## </Deprecated>
