undoc <- function(pkg, dir)
{
    paste0 <- function(...) paste(..., sep = "")
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
        
    if(!missing(pkg)) {
        pkgDir <- system.file(pkg = pkg)
        if(length(pkgDir) > 1) {
            warning(paste0("package `", pkg, "' found more than once"))
            pkgDir <- pkgDir[1]
        }
        if(pkgDir == "")
            stop(paste0("package `", pkg, "' is not installed"))
        objsdocs <- sort(scan(file = file.path(pkgDir, "help",
                              "AnIndex"),
                              what = list("", ""),
                              quiet = TRUE)[[1]])
        codeFile <- file.path(pkgDir, "R", pkg)
        dataDir <- file.path(pkgDir, "data")
    }
    else {
        if(missing(dir))
            stop("you must specify `pkg' or `dir'")
        if(!file.exists(dir))
            stop(paste0("directory `", dir, "' does not exist"))
        dir <- system(paste("cd", dir, "; pwd"), intern = TRUE)

        if(!file.exists(docsDir <- file.path(dir, "man")))
            stop("no directory with Rd sources found")
        docsExts <- c("Rd", "rd")
        files <- listFilesWithExts(docsDir, docsExts)
        if(file.exists(docsOSDir <- file.path(docsDir, .Platform$OS)))
            files <- c(files, listFilesWithExts(docsOSDir, docsExts))
        ## FIXME: Still unixy!
        cmd <- paste("grep -h '^\\\\\\(alias\\|name\\)'",
                     paste(files, collapse = " "),
                     "| sed 's/\\\\\\(alias\\|name\\){\\(.*\\)}/\\2/'")
        objsdocs <- system(cmd, intern = TRUE)
        objsdocs <- gsub("\\\\%", "%", objsdocs)
        objsdocs <- gsub(" ", "", objsdocs)
        objsdocs <- sort(unique(objsdocs))

        if(file.exists(codeDir <- file.path(dir, "R"))) {
            codeFile <- tempfile()
            on.exit(unlink(codeFile))
            codeExts <- c("R", "r", "S", "s", "q")
            files <- listFilesWithExts(codeDir, codeExts, path = FALSE)
            files <- file.path(codeDir, files[-grep("^zzz\\.", files)])
            if(file.exists(codeOSDir <- file.path(codeDir,
                                                  .Platform$OS)))
                files <- c(files,
                           listFilesWithExts(codeOSDir, codeExts))
            system(paste("cat", paste(files, collapse = " "), ">",
                         codeFile))
        }
        else
            codeFile <- ""

        dataDir <- file.path(dir, "data")        
    }

    lib.source <- function(file, env) {
        oop <- options(keep.source = FALSE)
        on.exit(options(oop))
        exprs <- parse(n = -1, file = file)
        if (length(exprs) == 0)
            return(invisible())
        for (i in exprs) yy <- eval(i, env)
        invisible()
    }

    if(file.exists(codeFile)) {
        codeEnv <- new.env()
        lib.source(codeFile, env = codeEnv)
        allObjs <- ls(envir = codeEnv, all.names = TRUE)
    }
    else
        allObjs <- NULL

    if(file.exists(dataDir)) {
        dataExts <- c("R", "r", "RData", "rdata", "rda", "TXT", "txt",
                      "tab", "CSV", "csv")
        dataObjs <- listFilesWithExts(dataDir, dataExts, path = FALSE)
        allObjs <- c(allObjs, sub("\\.[A-Za-z]*$", "", dataObjs))
    }
        
    ## Undocumented objects?
    if(is.null(allObjs))
        warning("Neither code nor data objects found")
    else
        allObjs[! allObjs %in% c(objsdocs, ".First.lib")]
}
