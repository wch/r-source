undoc <- function(pkg, dir)
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

    if(!missing(pkg)) {
        pkgDir <- system.file(pkg = pkg)
        if(length(pkgDir) > 1) {
            warning(paste("package", fQuote(pkg), "found more than once"))
            pkgDir <- pkgDir[1]
        }
        if(pkgDir == "")
            stop(paste("package", fQuote(pkg), "is not installed"))
        isBase <- pkg == "base"
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
            stop(paste("directory", fQuote(dir), "does not exist"))
        isBase <- basename(dir) == "base"
        if(!file.exists(docsDir <- file.path(dir, "man")))
            stop("no directory with Rd sources found")
        docsExts <- c("Rd", "rd")
        files <- listFilesWithExts(docsDir, docsExts)
        if(file.exists(docsOSDir <- file.path(docsDir, .Platform$OS)))
            files <- c(files, listFilesWithExts(docsOSDir, docsExts))
        files <- paste(files, collapse = " ")
        ## FIXME: Still unixy, but not GNUish anymore!
        fname  <- system(paste("grep -h '^\\\\name'",  files),intern=TRUE)
        falias <- system(paste("grep -h '^\\\\alias'", files),intern=TRUE)
        objsdocs <- c(gsub("\\\\name{(.*)}.*",  "\\1", fname),
                      gsub("\\\\alias{(.*)}.*", "\\1", falias))
        objsdocs <- gsub("\\\\%", "%", objsdocs)
        objsdocs <- gsub(" ", "", objsdocs)
        objsdocs <- sort(unique(objsdocs))

        if(file.exists(codeDir <- file.path(dir, "R"))) {
            codeFile <- tempfile("Rbuild")
            on.exit(unlink(codeFile))
            codeExts <- c("R", "r", "S", "s", "q")
            files <- listFilesWithExts(codeDir, codeExts, path = FALSE)
            if(any(i <- grep("^zzz\\.", files)))
               files <- files[-i]
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

    if(isBase)
        allObjs <- ls("package:base", all.name = TRUE)
    else if(file.exists(codeFile)) {
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
                new <- ls(envir = dataEnv, all.names = TRUE)
                dataObjs <- c(dataObjs, new)
                rm(list = new, envir = dataEnv)
            }
            files <- files[-i]
        }
        if(any(i <- grep("\\.\(RData\|rdata\|rda\)$", files))) {
            for (f in file.path(dataDir, files[i])) {
                load(f, envir = dataEnv)
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
