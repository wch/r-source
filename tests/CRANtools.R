### This needs a local (partial) CRAN mirror or Internet access
## Specifically, needs web/packages/packages.rds and web/checks/*.rds

## This may need to download, so increase the timeout.
options(warn = 1L, timeout = max(600, getOption('timeout')))

.ptime <- proc.time()

## Look up CRAN mirror in the same way the functions do.
## Uses R_CRAN_WEB if set, otherwise getOption('repos')["CRAN"]
## then the (possibly local) 'repositories' file
## and if still unset, https://CRAN.R-project.org
mirror <- tools:::CRAN_baseurl_for_web_area()
message("Using CRAN mirror ",  sQuote(mirror))

## Sanity check: the examples use /web/packages/packages.rds and
## web/checks/check_*.rds,
## but partial mirrors for package installation only need src/contrib.
## This would fail with a file:// mirror but provides a cheap check
## of the availability of a https:// one.
if(!startsWith(mirror, "file://")) {
    foo <- tryCatch(readLines(paste0(mirror, "/web/packages")),
                    error = function(e) {
                    message(conditionMessage(e))
                    ## and bail out
                    cat("Time elapsed: ", proc.time() - .ptime,"\n")
                    q("no")
    })
}

## no longer conditionalized on Internet access
library(tools)
example("CRAN_package_db", run.donttest = TRUE)

cat("Time elapsed: ", proc.time() - .ptime,"\n")
