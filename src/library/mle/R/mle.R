.noGenerics <- TRUE

.First.lib <- function(lib, pkg)
{
    have.stats4 <- "package:stats4" %in% search()
    if(!have.stats4) require("stats4")
    warning("package ", sQuote("mle"), " has been renamed to ",
            sQuote("stats4"), call. = FALSE)
}
