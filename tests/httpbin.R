## These are tests that require libcurl functionality and a working
## Internet connection.

## They formerly used http://httpbin.org/404

## check basic Internet access
if(.Platform$OS.type == "unix" &&
   is.null(nsl("developer.R-project.org"))) q()

site <- "https://developer.R-project.org/inet-tests/not-found"
tf <- tempfile()
testDownloadFile404 <- tryCatch(suppressWarnings({
    download.file(site, tf, method = "libcurl")
}), error=function(e) {
    conditionMessage(e) ==
        "cannot open URL 'https://developer.R-project.org/inet-tests/not-found'"
})
stopifnot(testDownloadFile404, !file.exists(tf))

test404.1 <- tryCatch({
    open(zz <- url(site, method = "libcurl"))
}, warning=function(w) {
    grepl("404 Not Found", conditionMessage(w))
})
close(zz)
stopifnot(test404.1)

## check option works
options(url.method = "libcurl")
test404.2 <- tryCatch({
    open(zz <- url(site))
}, warning = function(w) {
    grepl("404 Not Found", conditionMessage(w))
})
close(zz)
stopifnot(test404.2)

showConnections(all = TRUE)

proc.time()
