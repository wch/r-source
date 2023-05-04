## These are tests that require a working Internet connection.
## We attempt to test for that on Unix.

if(.Platform$OS.type == "unix" &&
   is.null(nsl("cran.r-project.org"))) q()

## As virtually all http sites redirect to https:, we only test the latter

# test do_download (and "record" #{packages}):
ap <- available.packages(contrib.url("https://cran.r-project.org"))
## IGNORE_RDIFF_BEGIN
nrow(ap)
## IGNORE_RDIFF_END

# test url connections
zz <- url("https://cran.r-project.org/")
readLines(zz)
close(zz)

# and via read.table, using an r-project.org site
read.table("https://developer.R-project.org/inet-tests/ch11b.dat")

showConnections(all = TRUE)
