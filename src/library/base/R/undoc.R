undoc <- function(package, dir, lib.loc = .lib.loc)
{
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

    useSaveImage <- FALSE
    if(!missing(package)) {
        packageDir <- .find.package(package, lib.loc)
        isBase <- package == "base"
        objsdocs <- sort(scan(file = file.path(packageDir, "help",
                              "AnIndex"),
                              what = list("", ""),
                              quiet = TRUE, sep="\t")[[1]])
        codeFile <- file.path(packageDir, "R", "all.rda")
        if(file.exists(codeFile))
            useSaveImage <- TRUE
        else
            codeFile <- file.path(packageDir, "R", package)
        dataDir <- file.path(packageDir, "data")
    }
    else {
        if(missing(dir))
            stop("you must specify `package' or `dir'")
        if(!file.exists(dir))
            stop(paste("directory", fQuote(dir), "does not exist"))
        isBase <- basename(dir) == "base"
        if(!file.exists(docsDir <- file.path(dir, "man")))
            stop("no directory with Rd sources found")
        docsExts <- c("Rd", "rd")
        files <- listFilesWithExts(docsDir, docsExts)
        if(file.exists(docsOSDir <- file.path(docsDir, .Platform$OS)))
            files <- c(files, listFilesWithExts(docsOSDir, docsExts))
        aliases <- character(0)
        for(f in files) {
            aliases <- c(aliases,
                         grep("^\\\\alias", readLines(f), value = TRUE))
        }
        objsdocs <- gsub("\\\\alias{(.*)}.*", "\\1", aliases)
        objsdocs <- gsub("\\\\%", "%", objsdocs)
        objsdocs <- gsub(" ", "", objsdocs)
        objsdocs <- sort(unique(objsdocs))

        if(file.exists(codeDir <- file.path(dir, "R"))) {
            codeFile <- tempfile("Rcode")
            on.exit(unlink(codeFile))
            codeExts <- c("R", "r", "S", "s", "q")
            files <- listFilesWithExts(codeDir, codeExts, path = FALSE)
            if(length(files) > 0)
                files <- file.path(codeDir, files)
            if(file.exists(codeOSDir <- file.path(codeDir, .Platform$OS)))
                files <- c(files, listFilesWithExts(codeOSDir, codeExts))
            file.create(codeFile)
            file.append(codeFile, files)
        }
        else
            codeFile <- ""

        dataDir <- file.path(dir, "data")
    }

    lib.source <- function(file, envir) {
        oop <- options(keep.source = FALSE)
        on.exit(options(oop))
        assignmentSymbol <- as.name("<-")
        exprs <- parse(n = -1, file = file)
        if(length(exprs) == 0)
            return(invisible())
        for(e in exprs) {
            if(e[[1]] == assignmentSymbol)
                yy <- eval(e, envir)
        }
        invisible()
    }
    
    if(isBase)
        allObjs <- ls("package:base", all.names = TRUE)
    else if(file.exists(codeFile)) {
        codeEnv <- new.env()
        if(useSaveImage) {
            yy <- try(load(codeFile, envir = codeEnv))
            if(inherits(yy, "try-error")) {
                stop("cannot load package image")
            }
        }
        else {
            yy <- try(lib.source(codeFile, envir = codeEnv))
            if(inherits(yy, "try-error")) {
                stop("cannot source package code")
            }
        }
        allObjs <- ls(envir = codeEnv, all.names = TRUE)
    }
    else
        allObjs <- NULL

    if(file.exists(dataDir)) {
        dataExts <- c("R", "r", "RData", "rdata", "rda", "TXT", "txt",
                      "tab", "CSV", "csv")
        files <- listFilesWithExts(dataDir, dataExts, path = FALSE)
        files <- files[!duplicated(sub("\\.[A-Za-z]*$", "", files))]
        dataEnv <- new.env()
        dataObjs <- NULL
        if(any(i <- grep("\\.\(R\|r\)$", files))) {
            for (f in file.path(dataDir, files[i])) {
                yy <- try(sys.source(f, envir = dataEnv, chdir = TRUE))
                if(inherits(yy, "try-error"))
                    stop(paste("cannot source data file", fQuote(f)))
                new <- ls(envir = dataEnv, all.names = TRUE)
                dataObjs <- c(dataObjs, new)
                rm(list = new, envir = dataEnv)
            }
            files <- files[-i]
        }
        if(any(i <- grep("\\.\(RData\|rdata\|rda\)$", files))) {
            for (f in file.path(dataDir, files[i])) {
                yy <- try(load(f, envir = dataEnv))
                if(inherits(yy, "try-error"))
                    stop(paste("cannot load data file", fQuote(f)))
                new <- ls(envir = dataEnv, all.names = TRUE)
                dataObjs <- c(dataObjs, new)
                rm(list = new, envir = dataEnv)
            }
            files <- files[-i]
        }
        if(length(files) > 0)
            dataObjs <- c(dataObjs, sub("\\.[A-Za-z]*$", "", files))
        allObjs <- c(allObjs, dataObjs)
    }

    ## Undocumented objects?
    if(is.null(allObjs))
        warning("Neither code nor data objects found")
    else
        allObjs[! allObjs %in% c(objsdocs, ".First.lib", ".Last.lib")]
}
