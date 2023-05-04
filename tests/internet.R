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

# and via read.table.  Use a copy on a r-project.org site.
##read.table("https://www.stats.ox.ac.uk/pub/datasets/csb/ch11b.dat")
read.table("https://developer.r-project.org/pub/datasets/csb/ch11b.dat")
