## These are tests that require libcurl functionality (available
## everywhere as from R 4.2.0) and a working Internet connection.

## Nowadays method = "libcurl" is the default everwhere for http[s]://

## check basic Internet access
if(.Platform$OS.type == "unix" &&
   is.null(nsl("cran.r-project.org"))) q()

tf <- tempfile()
download.file("http://cran.r-project.org/", tf)
file.size(tf)
unlink(tf)


## test url connections on http -- this now redirects to http://
str(readLines(zz <- url("http://cran.r-project.org/")))
zz
stopifnot(identical(summary(zz)$class, "url-libcurl"))
close(zz)


##  via read.table (which closes the connection) -- this now redirects to http://
tail(read.table(url("http://www.stats.ox.ac.uk/pub/datasets/csb/ch11b.dat")))

## check option works
options(url.method = "libcurl")
zz <- url("http://www.stats.ox.ac.uk/pub/datasets/csb/ch11b.dat")
stopifnot(identical(summary(zz)$class, "url-libcurl"))
close(zz)

showConnections(all = TRUE)

## --------------------------------------------------------------
## Some platforms have problems with certificates,
## so allow them to skip the https tests
junk <- tryCatch(curlGetHeaders("http://bugs.r-project.org"),
                 error = function(e) {
			 message("Check for working https failed:\n\t",
				 conditionMessage(e),
				 "skipping https tests\n")
			 q()
		 })

example(curlGetHeaders, run.donttest = TRUE)

## https URL
head(readLines(zz <- url("https://cran.r-project.org"), warn = FALSE))
close(zz)

## redirection (to a https:// URL)
head(readLines(zz <- url("http://bugs.r-project.org"), warn = FALSE))
close(zz)

proc.time()
