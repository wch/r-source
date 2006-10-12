.noGenerics <- TRUE

.onLoad <- function(libname, pkgname)
{
    op <- options()
    op.stats <-
        list(contrasts = c(unordered="contr.treatment",
             ordered="contr.poly"),
             na.action = "na.omit",
             show.coef.Pvalues = TRUE,
             show.signif.stars = TRUE,
             ts.eps = 1e-5,
             ts.S.compat = FALSE)
    toset <- !(names(op.stats) %in% names(op))
    if(any(toset)) options(op.stats[toset])
}

.onUnload <- function(libpath)
    library.dynam.unload("stats", libpath)
