checkFF <-
function(file, package, dir, lib.loc = .lib.loc,
         verbose = getOption("verbose")) {
    fQuote <- function(s) paste("`", s, "'", sep = "")
    listFilesWithExts <- function(dir, exts, path = TRUE) {
        ## Return the paths or names of the files in `dir' with
        ## extension in `exts'.
        files <- list.files(dir)
        files <- files[sub(".*\\.", "", files) %in% exts]
        if(path)
            files <- if(length(files) > 0)
                file.path(dir, files)
            else
                character(0)
        files
    }
    
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
        if(!missing(package)) {
            packageDir <- .find.package(package, lib.loc)
            file <- file.path(packageDir, "R", "all.rda")
            if(file.exists(file))
                useSaveImage <- TRUE
            else
                file <- file.path(packageDir, "R", package)
        }
        else if(!missing(dir)) {
            if(!file.exists(dir))
                stop(paste("directory", fQuote(dir), "does not exist"))
            else
                ## tilde expansion
                dir <- file.path(dirname(dir), basename(dir))
            if(!file.exists(codeDir <- file.path(dir, "R")))
                stop(paste("directory", fQuote(dir),
                           "does not contain R code"))
            codeExts <- c("R", "r", "S", "s", "q")
            codeFiles <- listFilesWithExts(codeDir, codeExts)
            if(file.exists(codeOSDir <- file.path(codeDir, .Platform$OS)))
                codeFiles <- c(codeFiles,
                               listFilesWithExts(codeOSDir, codeExts))
            file <- tempfile()
            on.exit(unlink(file))
            file.create(file)
            file.append(file, codeFiles)
        }
        else
            stop("you must specify `file', `package' or `dir'")
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
