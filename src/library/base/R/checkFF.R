checkFF <-
function(file, package, lib.loc = .lib.loc,
         verbose = getOption("verbose")) {
    fQuote <- function(s) paste("`", s, "'", sep = "")
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

    useSaveImage <- FALSE
    if(missing(file)) {
        if(missing(package))
            stop("you must specify `file' or `package'")
        packageDir <- .find.package(package, lib.loc)
        file <- file.path(packageDir, "R", "all.rda")
        if(file.exists(file))
            useSaveImage <- TRUE
        else
            file <- file.path(packageDir, "R", package)
    }
    
    if(!file.exists(file))
        stop(paste("file", fQuote(file), "does not exist"))
    FFfuns <- c(".C", ".Fortran", ".Call", ".External",
                ".Call.graphics", ".External.graphics")

    exprs <- if(useSaveImage) {
        writeLines("loading saved image ...")
        .CodeEnv <- new.env()
        load(file, envir = .CodeEnv)
        lapply(ls(envir = .CodeEnv, all.names = TRUE),
               function(f) {
                   f <- get(f, envir = .CodeEnv)
                   if(typeof(f) == "closure") body(f) else NULL
               })
    }
    else
        parse(file = file, n = -1)
    for(i in seq(along = exprs)) checkFFPackageArg(exprs[[i]])
}
