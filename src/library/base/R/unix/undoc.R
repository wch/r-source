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
                              quiet = TRUE, sep="\t")[[1]])
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
        ## FIXME: Still unixy, but this version is not GNUish!
        cmd <- paste("grep -h '^\\\\name'",
                     paste(files, collapse = " "),
                     "| sed 's/\\\\name{\\(.*\\)}.*/\\1/'")
        objsdocs <- system(cmd, intern = TRUE)
        cmd <- paste("grep -h '^\\\\alias'",
                     paste(files, collapse = " "),
                     "| sed 's/\\\\alias{\\(.*\\)}.*/\\1/'")
        objsdocs <- c(objsdocs, system(cmd, intern = TRUE))
        objsdocs <- gsub("\\\\%", "%", objsdocs)
        objsdocs <- gsub(" ", "", objsdocs)
        objsdocs <- sort(unique(objsdocs))

        if(file.exists(codeDir <- file.path(dir, "R"))) {
            codeFile <- tempfile()
            on.exit(unlink(codeFile))
            codeExts <- c("R", "r", "S", "s", "q")
            files <- listFilesWithExts(codeDir, codeExts, path = FALSE)
            if(any(i <- grep("^zzz\\.", files)))
               files <- files[-i]
            files <- file.path(codeDir, files)
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

    if(file.exists(codeFile)) {
        codeEnv <- new.env()
        sys.source(codeFile, envir = codeEnv)
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
                sys.source(f, envir = dataEnv, chdir = TRUE)
                new <- ls(all = TRUE, envir = dataEnv)
                dataObjs <- c(dataObjs, new)
                rm(list = new, envir = dataEnv)
            }
            files <- files[-i]
        }
        if(any(i <- grep("\\.\(RData\|rdata\|rda\)$", files))) {
            for (f in file.path(dataDir, files[i])) {
                load(f, envir = dataEnv)
                new <- ls(all = TRUE, envir = dataEnv)
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
