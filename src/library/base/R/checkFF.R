checkFF <- function(file, package, lib.loc = .lib.loc) {
    if(missing(file)) {
        if(missing(package))
            stop("you must specify `file' or `package'")
        ## <FIXME>
        ## This could be in .find.package().
        fQuote <- function(s) paste("`", s, "'", sep = "")        
        which.lib.loc <-
            lib.loc[file.exists(file.path(lib.loc, package))]
        if(length(which.lib.loc) == 0)
            stop(paste("There is no package called", fQuote(package)))
        if(length(which.lib.loc) > 1) {
            which.lib.loc <- which.lib.loc[1]
            warning(paste("Package ",
                          fQuote(package),
                          "found more than once,\n",
                          "using the one found in",
                          fQuote(which.lib.loc)))
        }
        file <- file.path(which.lib.loc, package, "R", package)
    }
    if(!file.exists(file))
        stop("file", fQuote(file), "does not exist")
    checkFFPackageArg <- function(e) {
        if(is.call(e) || is.expression(e)) {
            if((e[[1]] == as.name(".C")) ||
               (e[[1]] == as.name(".Fortran"))) {
                parg <- e[["PACKAGE"]]
                if(is.null(parg)) parg <- "MISSING"
                cat(e[[1]], "(", deparse(e[[2]]), ", ...): ", parg,
                    "\n", sep = "")
            }
            for(i in seq(along = e)) Recall(e[[i]])
        }
    }
    exprs <- parse(file = file, n = -1)
    for(i in seq(along = exprs)) checkFFPackageArg(exprs[[i]])
}
