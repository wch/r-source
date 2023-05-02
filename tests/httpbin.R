## These are tests that require libcurl functionality and a working
## Internet connection.

if(!capabilities("libcurl")) {
    warning("no libcurl support")
    q()
}

## check basic Internet access
if(.Platform$OS.type == "unix" &&
   is.null(nsl("cran.r-project.org"))) q()


tf <- tempfile()
testDownloadFile404 <- tryCatch(suppressWarnings({
    download.file("http://developer.R-project.org/inet-tests/not-found", tf, method = "libcurl")
}), error=function(e) {
    conditionMessage(e) == "cannot open URL 'http://developer.R-project.org/inet-tests/not-found'"
})
stopifnot(testDownloadFile404, !file.exists(tf))

test404.1 <- tryCatch({
    open(zz <- url("http://developer.R-project.org/inet-tests/not-found", method = "libcurl"))
}, warning=function(w) {
    grepl("404 Not Found", conditionMessage(w))
})
close(zz)
stopifnot(test404.1)

## check option works
options(url.method = "libcurl")
test404.2 <- tryCatch({
    open(zz <- url("http://developer.R-project.org/inet-tests/not-found"))
}, warning = function(w) {
    grepl("404 Not Found", conditionMessage(w))
})
close(zz)
stopifnot(test404.2)

showConnections(all = TRUE)

proc.time()
