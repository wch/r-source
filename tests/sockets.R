## These are tests that require socket functionality, and a working
## Internet connection without a proxy.

if(.Platform$OS.type == "unix" &&
   is.null(nsl("cran.r-project.org"))) q()

# A simple http client via sockets
httpget <- function (url, port = 80)
{
    urlel <- strsplit(url, "/")[[1]]
    if (urlel[1] != "http:") stop("Not an http:// URL")
    host <- urlel[3]
    rurl <- paste(c("", urlel[-(1:3)]), collapse = "/")
    a <- make.socket(host, port = port)
    on.exit(close.socket(a))
    ## Technically, Host: is not defined in HTTP/1.0, but it is required
    ## to distinguish virtual servers on the same IP address. We could bump
    ## required HTTP version to 1.1, but since HTTP/1.0 defines that
    ## unrecognized headers should be ignored, it is legal as well (and works).
    headreq <- paste0("HEAD ", rurl, " HTTP/1.0\r\nHost: ", host, "\r\nConnection: Keep-Alive\r\nAccept: text/plain\r\n\r\n")
    write.socket(a, headreq)
    head <- read.socket(a, maxlen = 8000)
    b <- strsplit(head, "\n")[[1]]
    if (length(grep("200 OK", b[1])) == 0) stop(b[1])
    len <- as.numeric(strsplit(grep("Content-Length", b, value = TRUE),
                               ":")[[1]][2])
    getreq <- paste0("GET ", rurl, " HTTP/1.0\r\nHost: ", host, "\r\nConnection: Keep-Alive\r\nAccept: text/plain\r\n\r\n")
    write.socket(a, getreq)
    junk <- read.socket(a, maxlen = nchar(head))
    data <- ""
    b <- strsplit(c(head, junk), "\n")
    nn <- length(b[[1]])
    if (length(b[[2]]) > nn)
        data <- paste(b[[2]][-(1:nn)], collapse = "\n")
    while (nchar(data) < len) {
        new_data <- read.socket(a, maxlen = len - nchar(data))
        if (new_data == "") break
        data <- paste(data, new_data, sep = "")
    }
    strsplit(data, "\n")[[1]]
}

if(nzchar(Sys.getenv("http_proxy")) || nzchar(Sys.getenv("HTTP_PROXY"))) {
    cat("http proxy is set, so skip test of http over sockets\n")
} else {
    f <- httpget("http://developer.R-project.org/inet-tests/ch11b.dat")
    str(f)
    if (length(f) != 100L) stop("Data not fetched via socket")
}

proc.time()
