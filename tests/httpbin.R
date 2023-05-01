## These are tests that require libcurl functionality and a working
## Internet connection. Those that usw the often unreliable httpbin.org

if(!capabilities("libcurl")) {
    warning("no libcurl support")
    q()
}

## check basic Internet access
if(.Platform$OS.type == "unix" &&
   is.null(nsl("cran.r-project.org"))) q()


tf <- tempfile()
testDownloadFile404 <- tryCatch(suppressWarnings({
    download.file("http://httpbin.org/status/404", tf, method = "libcurl")
}), error=function(e) {
    conditionMessage(e) == "cannot open URL 'http://httpbin.org/status/404'"
})
stopifnot(testDownloadFile404, !file.exists(tf))

test404.1 <- tryCatch({
    open(zz <- url("http://httpbin.org/status/404", method = "libcurl"))
}, warning=function(w) {
    grepl("404 Not Found", conditionMessage(w))
})
close(zz)
stopifnot(test404.1)

## check option works
options(url.method = "libcurl")
test404.2 <- tryCatch({
    open(zz <- url("http://httpbin.org/status/404"))
}, warning = function(w) {
    grepl("404 Not Found", conditionMessage(w))
})
close(zz)
stopifnot(test404.2)

showConnections(all = TRUE)

proc.time()
