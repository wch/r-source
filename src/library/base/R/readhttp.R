"httpclient" <-
    function (url, port = 80, error.is.fatal = TRUE, check.MIME.type = TRUE, 
              file = tempfile(), drop.ctrl.z = TRUE) 
{
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
                                                                                  value = T), ":")[[1]][2])))) == 0) {
        if (error.is.fatal) 
            stop(grep("Content-Type:", b, value = T))
        else warning(grep("Content-Type:", b, value = T))
        return(file)
    }
    len <- as.numeric(strsplit(grep("Content-Length", b, value = T), 
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
        data <- gsub("\026", "", data, extended = FALSE)
    cat(data, file = file)
    return(file)
}
"read.table.url" <-
    function (url, method="auto", ...) 
{
    f<-tempfile()
    if (download.file(url, destfile=f,method=method)==0)
        data <- read.table(f, ...)
    else {
        unlink(f)
        stop("transfer failure")
    }
    return(data)
}
"scan.url" <-
    function (url, file=tempfile(),method="auto", ...) 
{
    if (download.file(url,dest=file,method=method)!=0){
        unlink(file)
        stop("transfer failed")
    }
    data <- scan(file, ...)
    unlink(file)
    return(data)
}
"source.url" <-
    function (url,file=tempfile(),...) 
{
    if (download.file(url,dest=file)!=0){
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
"url.show" <-
    function (url,  title = url, 
              delete.file = TRUE, file = tempfile(), method="auto",...) 
{
    if (download.file(url, dest = file,method=method)!=0)
        stop("transfer failure")
    file.show(file, delete.file = delete.file,title=title, ...)
}
