.noGenerics <- TRUE

.First.lib <- function(lib, pkg)
{
    have.stats <- "package:stats" %in% search()
    if(!have.stats) require("stats")
    warning("package ", sQuote("ctest"), " has been merged into ",
            sQuote("stats"), call. = FALSE)
}
