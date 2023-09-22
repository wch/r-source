## These are tests that require libcurl functionality (available
## everywhere as from R 4.2.0) and a working Internet connection.

## Nowadays method = "libcurl" is the default everywhere for http[s]://

## This used to check that https:// worked and skip if not, but then
## all the tests redirected to https://

## check basic Internet access
if(.Platform$OS.type == "unix" &&
   is.null(nsl("cran.r-project.org"))) q()

tf <- tempfile()
download.file("https://cran.r-project.org/", tf)
file.size(tf)
unlink(tf)


## test url connections on http -- this now redirects to https://
str(readLines(zz <- url("http://cran.r-project.org/")))
zz
stopifnot(identical(summary(zz)$class, "url-libcurl"))
close(zz)
## and check that direct https:// works
str(readLines(zz <- url("https://cran.r-project.org/")))
close(zz)

##  via read.table (which closes the connection) -- this now redirects to https://
tail(read.table(url("http://www.stats.ox.ac.uk/pub/datasets/csb/ch11b.dat")))

## check option works
options(url.method = "libcurl")
zz <- url("http://www.stats.ox.ac.uk/pub/datasets/csb/ch11b.dat")
stopifnot(identical(summary(zz)$class, "url-libcurl"))
close(zz)

curlGetHeaders("https://developer.r-project.org")
example(curlGetHeaders, run.donttest = TRUE)

showConnections(all = TRUE)

proc.time()
