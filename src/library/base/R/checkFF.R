checkFF <-
function(file, package, lib.loc = .lib.loc,
         verbose = getOption("verbose")) {
    fQuote <- function(s) paste("`", s, "'", sep = "")
    if(missing(file)) {
        if(missing(package))
            stop("you must specify `file' or `package'")
        file <- file.path(.find.package(package, lib.loc), "R", package)
    }
    if(!file.exists(file))
        stop(paste("file", fQuote(file), "does not exist"))
    FFfuns <- c(".C", ".Fortran", ".Call", ".External",
                ".Call.graphics", ".External.graphics")
    checkFFPackageArg <- function(e) {
        if(is.call(e) || is.expression(e)) {
            if(as.character(e[[1]]) %in% FFfuns) {
                parg <- e[["PACKAGE"]]
                if(is.null(parg)) parg <- "MISSING"
                if((parg == "MISSING") || verbose)
                    cat(e[[1]], "(", deparse(e[[2]]), ", ...): ", parg,
                        "\n", sep = "")
            }
            for(i in seq(along = e)) Recall(e[[i]])
        }
    }
    exprs <- parse(file = file, n = -1)
    for(i in seq(along = exprs)) checkFFPackageArg(exprs[[i]])
}
