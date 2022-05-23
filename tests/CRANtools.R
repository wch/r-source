### This needs a local CRAN mirror or Internet access

## This may need to download, so increase the timeout.
options(timeout = max(600, getOption('timeout')))

.ptime <- proc.time()

## Look up CRAN mirror in the same way the functions do.
## Uses R_CRAN_WEB if set, otherwise getOption('repos')["CRAN"]
## and if that is unset, https://CRAN.R-project.org
mirror <- tools:::CRAN_baseurl_for_web_area()
message("Using CRAN mirror ",  sQuote(mirror))

## Sanity check: we use /web/packages/packages.rds,
## but partial mirrors for package installation only need src/contrib,
options(warn = 1L)
foo <- tryCatch(readLines(paste0(mirror, "/web/packages")),
                error = function(e) {
                    message(conditionMessage(e))
                    cat("Time elapsed: ", proc.time() - .ptime,"\n")
                    ## and bail out:
                    q("no")
                })

## no longer conditionalized:
    library(tools)
    example("CRAN_package_db", run.donttest = TRUE)

cat("Time elapsed: ", proc.time() - .ptime,"\n")
