checkFF <- function(file, package, lib.loc = .lib.loc) {
    if(missing(file)) {
        if(missing(package))
            stop("you must specify `file' or `package'")
        file <- file.path(.find.package(package, lib.loc), "R", package)
    }
    if(!file.exists(file))
        stop(paste("file", fQuote(file), "does not exist"))
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
