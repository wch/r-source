codoc <-
function(dir, use.values = FALSE, use.positions = TRUE,
         ignore.generic.functions = FALSE,
         keep.tempfiles = FALSE,
         verbose = getOption("verbose"))
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

    ## Argument handling.
    if(missing(dir))
        stop("no package directory given")
    if(!file.exists(dir))
        stop(paste("directory", fQuote(dir), "does not exist"))
    else
        ## tilde expansion
        dir <- file.path(dirname(dir), basename(dir))
    if(!file.exists(codeDir <- file.path(dir, "R")))
        stop(paste("directory", fQuote(dir),
                   "does not contain R code"))
    if(!file.exists(docsDir <- file.path(dir, "man")))
        stop(paste("directory", fQuote(dir),
                   "does not contain Rd sources"))
    isBase <- basename(dir) == "base"

    unlinkOnExitFiles <- NULL
    if(!keep.tempfiles)
        on.exit(unlink(unlinkOnExitFiles))

    ## Collect code into codeFile.
    codeFile <- tempfile("Rcode")
    unlinkOnExitFiles <- c(unlinkOnExitFiles, codeFile)
    codeExts <- c("R", "r", "S", "s", "q")
    files <- listFilesWithExts(codeDir, codeExts, path = FALSE)
    files <- file.path(codeDir, files)
    if(file.exists(codeOSDir <- file.path(codeDir, .Platform$OS)))
        files <- c(files, listFilesWithExts(codeOSDir, codeExts))
    file.create(codeFile)
    file.append(codeFile, files)

    ## Collect usages into docsFile.
    docsFile <- tempfile("Rdocs")
    unlinkOnExitFiles <- c(unlinkOnExitFiles, docsFile)
    docsExts <- c("Rd", "rd")
    files <- listFilesWithExts(docsDir, docsExts, path = FALSE)
    if(isBase) {
        baseStopList <- c("Devices.Rd") # add more if needed
        files <- files[!files %in% baseStopList]
    }
    files <- file.path(docsDir, files)
    if(file.exists(docsOSDir <- file.path(docsDir, .Platform$OS)))
        files <- c(files, listFilesWithExts(docsOSDir, docsExts))
    docsList <- tempfile("Rdocs")
    unlinkOnExitFiles <- c(unlinkOnExitFiles, docsList)
    writeLines(files, docsList)
    .Script("perl", "extract-usage.pl", paste(docsList, docsFile))

    ## Read code into .CodeEnv
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
    .CodeEnv <- new.env()
    if(verbose)
        cat("Reading code from", fQuote(codeFile), "\n")        
    lib.source(codeFile, env = .CodeEnv)
    lsCode <- ls(envir = .CodeEnv, all.names = TRUE)
    ## Find the functions objects to work on.
    funs <- lsCode[sapply(lsCode, function(f) {
        f <- get(f, envir = .CodeEnv)
        is.function(f) && (length(formals(f)) > 0)
    })]
    if(ignore.generic.functions) {
        isGeneric <- function(f) {
            any(grep("UseMethod",
                     deparse(body(get(f, envir = .CodeEnv)))))
        }
        funs <- funs[sapply(funs, isGeneric) == FALSE]
    }
    
    .DocsEnv <- new.env()
    checkCoDoc <- function(f) {
        ffc <- formals(get(f, envir = .CodeEnv))
        ffd <- formals(get(f, envir = .DocsEnv))
        if(!use.positions) {
            ffc <- ffc[sort(names(ffc))]
            ffd <- ffc[sort(names(ffd))]
        }
        if(!use.values) {
            ffc <- names(ffc)
            ffd <- names(ffd)
        }
        if(all(all.equal(ffc, ffd) == TRUE))
            NULL
        else {
            list(list(name = f, code = ffc, docs = ffd))
        }
    }

    ## Process the usages in the Rd files, one at a time.
    lsDocs <- character()
    badUsages <- list()
    if(verbose)
        cat("Reading docs from", fQuote(docsFile), "\n")
    txt <- readLines(docsFile)
    ind <- grep("^# usages in file", txt)
    ## Use a text connection for reading the blocks determined by ind.
    ## Alternatively, we could split txt into a list of the blocks.
    numOfUsageCodeLines <- diff(c(ind, length(txt) + 1)) - 1
    txtConn <- textConnection(paste(txt, collapse = "\n"))
    on.exit(close(txtConn), add = TRUE)
    for(n in numOfUsageCodeLines) {
        whereAmI <- readLines(txtConn, 1)
        exprs <- try(parse(n = -1, text = readLines(txtConn, n)))
        if(inherits(exprs, "try-error"))
            stop(paste("cannot source", gsub("^# ", "", whereAmI)))
        for(i in exprs) {
            yy <- try(eval(i, env = .DocsEnv))
            if(inherits(yy, "try-error"))
                stop(paste("cannot eval", gsub("^# ", "", whereAmI)))
        }

        badUsagesInFile <- list()
        file <- gsub("^# usages in file ", "", whereAmI)
        usages <- ls(envir = .DocsEnv, all.names = TRUE)
        for(f in usages[usages %in% funs])
            badUsagesInFile <- c(badUsagesInFile, checkCoDoc(f))
        if(length(badUsagesInFile) > 0)
            badUsages[[file]] <- badUsagesInFile
        
        lsDocs <- c(lsDocs, usages)
        rm(list = usages, envir = .DocsEnv)
    }

    ## Objects documented but missing from the code?
    overdocObjs <- lsDocs[!lsDocs %in% lsCode]
    if((length(overdocObjs) > 0) && !isBase) {
        writeLines("\nObjects documented but missing from the code:")
        print(unique(overdocObjs))
        writeLines("")
    }

    ## Objects without usage information.
    ## Could still be documented via \alias.
    undocObjs <- lsCode[!lsCode %in% lsDocs]
    if(verbose) {
        writeLines("\nObjects without usage information:")
        print(undocObjs)
        writeLines("")        
    }

    class(badUsages) <- "codoc"
    badUsages
}

print.codoc <-
function(x, ...)
{
    if(length(x) == 0)
        return(invisible())
    hasOnlyNames <- is.character(x[[1]][[1]][["code"]])
    formatArgs <- function(s) {
        if(hasOnlyNames) {
            paste("function(", paste(s, collapse = ", "), ")", sep = "")
        }
        else {
            s <- paste(deparse(s), collapse = "")
            s <- gsub(" = \([,\\)]\)", "\\1", s)
            gsub("^list", "function", s)
        }
    }
    for(fname in names(x)) {
        writeLines(paste("Codoc mismatches from file `", fname, "':",
                         sep = ""))
        xfname <- x[[fname]]
        for(i in seq(along = xfname))
            writeLines(c(xfname[[i]][["name"]],
                         strwrap(paste("Code:",
                                       formatArgs(xfname[[i]][["code"]])),
                                 indent = 2, exdent = 17),
                         strwrap(paste("Docs:",
                                       formatArgs(xfname[[i]][["docs"]])),
                                 indent = 2, exdent = 17)))
        writeLines("")
    }
    invisible(x)
}
