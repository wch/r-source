.noGenerics <- TRUE
.conflicts.OK <- TRUE

.onLoad <- .First.lib <- function(lib, pkg)
{
    have.stats <- "package:stats" %in% search()
    if(!have.stats) require("stats")
    warning("package ", sQuote("modreg"), " has been merged into ",
            sQuote("stats"), call. = FALSE)
}
