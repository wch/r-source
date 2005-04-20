.First.lib <- function(libname, pkgname)
{
    fullName <- paste("package", pkgname, sep=":")
    myEnv <- as.environment(match(fullName, search()))
    dataFile <- file.path(libname, pkgname, "R", "all.rda")
    rm(.First.lib, envir = myEnv)
    load(dataFile, myEnv)
    if(exists(".required", envir = myEnv, inherits = FALSE)) {
        required <- get(".required", envir = myEnv)
        for(pkg in required)
            require(pkg, quietly = TRUE, character.only = TRUE, save = FALSE)
    }
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
