## These are tests that require socket and internet functionality, and
## a working Internet connection.
## We attempt to test for those.

onWindows <- .Platform$OS.type == "windows"

if(.Platform$OS.type == "unix" &&
   is.null(nsl("cran.r-project.org"))) q()


## check graceful failure:
try(url("http://foo.bar", "r"))

if(onWindows)
    try(url("http://foo.bar", "r", method = "wininet"))

## download.file(.. , headers = character()) - PR#17710
tf <- tempfile()
download.file("https://cloud.r-project.org/src/base/THANKS", destfile = tf,
              method = if(onWindows) "wininet" else "libcurl",
              headers = character()) -> err.code
stopifnot(err.code == 0, file.size(tf) > 999)
## had failed for the Windows case

proc.time()
