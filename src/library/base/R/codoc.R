codoc <- function(dir, use.values = FALSE, use.positions = TRUE,
                  ignore.generic.functions = FALSE,
                  keep.tempfiles = FALSE,
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

    unlinkOnExitFiles <- NULL
    if(!keep.tempfiles)
        on.exit(unlink(unlinkOnExitFiles))

    codeFile <- tempfile("Rcode")
    unlinkOnExitFiles <- c(unlinkOnExitFiles, codeFile)
    codeExts <- c("R", "r", "S", "s", "q")
    files <- listFilesWithExts(codeDir, codeExts, path = FALSE)
    if(any(i <- grep("^zzz\\.", files)))
        files <- files[-i]
    files <- file.path(codeDir, files)
    if(file.exists(codeOSDir <- file.path(codeDir, .Platform$OS)))
        files <- c(files, listFilesWithExts(codeOSDir, codeExts))
    file.create(codeFile)
    file.append(codeFile, files)

    docsFile <- tempfile("Rdocs")
    unlinkOnExitFiles <- c(unlinkOnExitFiles, docsFile)
    docsExts <- c("Rd", "rd")
    files <- listFilesWithExts(docsDir, docsExts, path = FALSE)
    if(basename(dir) == "base") {
        baseStopList <- c("Devices.Rd") # add more if needed
	files <- files[-grep(baseStopList, files, ignore.case = TRUE)]
    }
    files <- file.path(docsDir, files)
    if(file.exists(docsOSDir <- file.path(docsDir, .Platform$OS)))
        files <- c(files, listFilesWithExts(docsOSDir, docsExts))
    docsList <- tempfile("Rdocs")
    unlinkOnExitFiles <- c(unlinkOnExitFiles, docsList)
    cat(files, sep = "\n", file = docsList)
    .Script("perl", "extract-usage.pl", paste(docsList, docsFile))

    .DocsEnv <- new.env()
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
    }
    lsDocs <- ls(envir = .DocsEnv, all.names = TRUE)

    lib.source <- function(file, env) {
        oop <- options(keep.source = FALSE)
        on.exit(options(oop))
        exprs <- parse(n = -1, file = file)
        if(length(exprs) == 0)
            return(invisible())
        for(i in exprs) yy <- eval(i, env)
        invisible()
    }
    .CodeEnv <- new.env()
    if(verbose)
        cat("Reading code from", fQuote(codeFile), "\n")        
    lib.source(codeFile, env = .CodeEnv)
    lsCode <- ls(envir = .CodeEnv, all.names = TRUE)

    funs <- sapply(lsCode,
                   function(f) is.function(get(f, envir = .CodeEnv)))
    ## Undocumented variables?
    vars <- lsCode[funs == FALSE]
    undocVars <- vars[!vars %in% lsDocs]
    if(verbose) {
        cat("\nVariables without usage information:\n")
        print(undocVars)
    }
    ## Undocumented functions?
    funs <- lsCode[funs]
    undocFuns <- funs[!funs %in% lsDocs]
    if(verbose) {
        cat("\nFunctions without usage information:\n")
        print(undocFuns)
    }

    ## Function objects which are non-primitive (such that args() is
    ## non-NULL) and have wrong usage documentation
    args <- lapply(funs,
                   function(f) args(get(f, envir = .CodeEnv)))
    funs <- funs[(funs %in% lsDocs) & (sapply(args, length) > 0)]
    if(ignore.generic.functions) {
        isGeneric <- function(f) {
            any(grep("UseMethod",
                     deparse(body(get(f, envir = .CodeEnv)))))
        }
        funs <- funs[sapply(funs, isGeneric) == FALSE]
    }

    getCoDoc <- function(f) {
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
        list(code = ffc, docs = ffd)
    }
    wrongfuns <- lapply(funs, getCoDoc)
    names(wrongfuns) <- funs
    wrongfuns <-
        wrongfuns[sapply(wrongfuns,
                          function(u) {
                              all(all.equal(u$code, u$docs) == TRUE)
                          }) == FALSE]
    wrongfuns
}
