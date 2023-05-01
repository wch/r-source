## These are tests that require libcurl functionality and a working
## Internet connection.

## As from R 3.4.0 method = "libcurl" is the default on a Unix-alike
## so this is in small part duplication -- but not on Windows.

if(!capabilities("libcurl")) {
    warning("no libcurl support")
    q()
}

## check basic Internet access
if(.Platform$OS.type == "unix" &&
   is.null(nsl("cran.r-project.org"))) q()

tf <- tempfile()
download.file("http://cran.r-project.org/", tf,  method = "libcurl")
file.size(tf)
unlink(tf)


## test url connections on http
str(readLines(zz <- url("http://cran.r-project.org/", method = "libcurl")))
zz
stopifnot(identical(summary(zz)$class, "url-libcurl"))
close(zz)


##  via read.table (which closes the connection)
tail(read.table(url("http://www.stats.ox.ac.uk/pub/datasets/csb/ch11b.dat",
                    method = "libcurl")))

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

## part of example(curlGetHeaders, run.donttest = TRUE)
curlGetHeaders("http://bugs.r-project.org")   ## this redirects to https://


## https URL
head(readLines(zz <- url("https://cran.r-project.org", method = "libcurl"),
               warn = FALSE))
close(zz)

## redirection (to a https:// URL)
head(readLines(zz <- url("http://bugs.r-project.org", method = "libcurl"),
               warn = FALSE))
close(zz)

options(url.method = "libcurl")
head(readLines("https://cran.r-project.org", warn = FALSE))

proc.time()
