.First.lib <- function(libname, pkgname)
{
    fullName <- paste("package", pkgname, sep=":")
    myEnv <- as.environment(match(fullName, search()))
    barepackage <- sub("([^-]+)_.*", "\\1", pkgname)
    dbbase <- file.path(libname, pkgname, "R", barepackage)
    rm(.First.lib, envir = myEnv)
    lazyLoad(dbbase, myEnv)
    if(exists(".First.lib", envir = myEnv, inherits = FALSE)) {
        f <- get(".First.lib", envir = myEnv, inherits = FALSE)
        if(is.function(f))
            f(libname, pkgname)
        else
            stop(gettextf("package '%s' has a non-function .First.lib",
                          pkgname),
                 domain = NA)
    }
}
