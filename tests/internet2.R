## These are tests that require internet functionality and a working
## Internet connection.
## We attempt to test for those on Unix.

onWindows <- .Platform$OS.type == "windows"

if(.Platform$OS.type == "unix" &&
   is.null(nsl("cran.r-project.org"))) q()


## check graceful failure:
try(url("http://foo.bar", "r"))

if(onWindows)
    try(url("http://foo.bar", "r", method = "wininet"))

## available.packages() (not) caching in case of errors
tools::assertWarning(ap1 <- available.packages(repos = "http://foo.bar"))
tools::assertWarning(ap2 <- available.packages(repos = "http://foo.bar"))
stopifnot(nrow(ap1) == 0, identical(ap1, ap2))
## had failed for a while in R-devel (left empty *.rds file)

## download.file(.. , headers = character()) - PR#17710
## https://bugs.r-project.org/show_bug.cgi?id=17710
## character() should be the same as NULL, but was not for wininet
tf <- tempfile()
download.file("https://cloud.r-project.org/src/base/THANKS", destfile = tf,
              method = if(onWindows) "wininet" else "libcurl",
              headers = character()) -> err.code
stopifnot(err.code == 0, file.size(tf) > 999) # when checked was 3914 bytes
## had failed for the Windows case

## ---- Checks which formerly used http://httpbin.org/404 ----

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
