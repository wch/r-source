## These are tests that require socket and internet functionality, and
## a working Internet connection.
## We attempt to test for those.

if(!capabilities()["http/ftp"]) {
    warning("no internet capabilities")
    q()
}

if(.Platform$OS.type == "unix" &&
   is.null(nsl("cran.r-project.org"))) q()

if(.Platform$OS.type == "windows") options(repos="http://cran.r-project.org")

# test do_download.
nrow(CRAN.packages())

# test url connections on http
zz <- url("http://cran.r-project.org/")
readLines(zz)
close(zz)

## check graceful failure:
try(zz <- url("http://foo.bar", "r"))

# and via read.table, test http and ftp.

read.table("http://www.stats.ox.ac.uk/pub/datasets/csb/ch11b.dat")
read.table("ftp://ftp.stats.ox.ac.uk/pub/datasets/csb/ch11b.dat")


if(!capabilities()["sockets"]) stop("no socket capabilities")

# do the same thing via sockets (cut-down httpclient)
httpget <- function (url, port = 80)
{
    urlel <- strsplit(url, "/")[[1]]
    if (urlel[1] != "http:") stop("Not an http:// URL")
    host <- urlel[3]
    rurl <- paste(c("", urlel[-(1:3)]), collapse = "/")
    a <- make.socket(host, port = port)
    on.exit(close.socket(a))
    headreq <- paste("HEAD", rurl, "HTTP/1.0\r\nConnection: Keep-Alive\r\nAccept: text/plain\r\n\r\n")
    write.socket(a, headreq)
    head <- read.socket(a, maxlen = 8000)
    b <- strsplit(head, "\n")[[1]]
    if (length(grep("200 OK", b[1])) == 0) stop(b[1])
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
    strsplit(data, "\n")[[1]]
}

if(nchar(Sys.getenv("http_proxy")) > 0
   || nchar(Sys.getenv("HTTP_PROXY")) > 0) {
    cat("http proxy is set, so skip test of http over sockets\n")
} else {
    httpget("http://www.stats.ox.ac.uk/pub/datasets/csb/ch11b.dat")
}

finger <- function(user, host = "localhost", port = 79, print = TRUE)
{
    if (!is.character(user))
        stop("user name must be a string")
    user <- paste(user,"\r\n")
    socket <- make.socket(host, port)
    on.exit(close.socket(socket))
    write.socket(socket, user)
    output <- character(0)
    repeat{
        ss <- read.socket(socket)
        if (ss == "") break
        output <- paste(output, ss)
    }
    close.socket(socket)
    if (print) cat(output)
    invisible(output)
}
try(finger("root"))  ## only works if your site provides a finger daemon
