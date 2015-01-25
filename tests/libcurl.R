## These are tests that require libcurl functionality and a working
## Internet connection.

if(!capabilities()["libcurl"]) {
    warning("no libcurl support")
    q()
}

## fails some of the time
if(.Platform$OS.type == "windows") q()

if(.Platform$OS.type == "unix" &&
   is.null(nsl("cran.r-project.org"))) q()

example(curlGetHeaders, run.dontcheck = TRUE)

tf <- tempfile()
download.file("http://cran.r-project.org/", tf,  method = "libcurl")
file.size(tf)
unlink(tf)

tf <- tempfile()
download.file("ftp://ftp.stats.ox.ac.uk/pub/datasets/csb/ch11b.dat",
              tf,  method = "libcurl")
file.size(tf) # 2102
unlink(tf)


## test url connections on http
str(readLines(zz <- url("http://cran.r-project.org/", method = "libcurl")))
zz
stopifnot(identical(summary(zz)$class, "url-libcurl"))
close(zz)

## https URL
head(readLines(zz <- url("https://httpbin.org", method = "libcurl")))
close(zz)

## redirection (to a https:// URL)
head(readLines(zz <- url("http://bugs.r-project.org", method = "libcurl"),
               warn = FALSE))
close(zz)


## check graceful failure:
try(zz <- url("http://foo.bar", "r", method = "libcurl"))
close(zz)

##  via read.table (which closes the connection)
tail(read.table(url("http://www.stats.ox.ac.uk/pub/datasets/csb/ch11b.dat",
                    method = "libcurl")))
tail(read.table(url("ftp://ftp.stats.ox.ac.uk/pub/datasets/csb/ch11b.dat",
                    method = "libcurl")))

showConnections(all = TRUE)

## check option works
options(url.method = "libcurl")
zz <- url("http://www.stats.ox.ac.uk/pub/datasets/csb/ch11b.dat",
          method = "libcurl")
stopifnot(identical(summary(zz)$class, "url-libcurl"))
close(zz)
head(readLines("https://httpbin.org", warn = FALSE))

