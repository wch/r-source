### This needs a full local CRAN mirror or Internet access

## This may need to download, so increase the timeout.
options(timeout = max(600, getOption('timeout')))

.ptime <- proc.time()

## look up CRAN mirror in the same way the functions do (uses R_CRAN_WEB)
mirror <- tools:::CRAN_baseurl_for_web_area()
message("Using CRAN mirror ",  sQuote(mirror))

## Sanity check
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
