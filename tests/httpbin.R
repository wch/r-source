## These are tests that require libcurl functionality (always
## available as from R 4.2.0) and a working Internet connection.

## Nowadays method = "libcurl" is the default everywhere for https://

## They formerly used http://httpbin.org/404

## check basic Internet access
if(.Platform$OS.type == "unix" &&
   is.null(nsl("developer.R-project.org"))) q()

## Check that a non-existent download does not leave an empty file
site <- "https://developer.R-project.org/inet-tests/not-found"
tf <- tempfile()
testDownloadFile404 <- tryCatch(suppressWarnings({
    download.file(site, tf)
}), error = function(e) {
    conditionMessage(e) == sprintf("cannot open URL '%s'", site)
})
stopifnot(testDownloadFile404, !file.exists(tf))

test404.1 <- tryCatch({
    open(zz <- url(site))
}, warning = function(w) {
    grepl("404 Not Found", conditionMessage(w))
})
close(zz)
stopifnot(test404.1)

showConnections(all = TRUE)

proc.time()
