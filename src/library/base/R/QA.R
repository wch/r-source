checkDocArgs <-
function(dir)
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

    if(missing(dir))
        stop("no package directory given")
    if(!file.exists(dir))
        stop(paste("directory", fQuote(dir), "does not exist"))
    else
        ## tilde expansion
        dir <- file.path(dirname(dir), basename(dir))
    if(!file.exists(docsDir <- file.path(dir, "man")))
        stop(paste("directory", fQuote(dir),
                   "does not contain Rd sources"))

    docsFile <- tempfile("Rdocs")
    on.exit(unlink(docsFile))
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
    on.exit(unlink(docsList), add = TRUE)
    writeLines(files, docsList)
    .Script("perl", "extract-usage.pl",
            paste("--mode=args", docsList, docsFile))

    .ArgsEnv <- new.env()
    txt <- readLines(docsFile)
    ind <- grep("^# usages in file", txt)
    ## Use a text connection for reading the blocks determined by ind.
    ## Alternatively, we could split txt into a list of the blocks.
    numOfUsageCodeLines <- diff(c(ind, length(txt) + 1)) - 2
    txtConn <- textConnection(paste(txt, collapse = "\n"))
    on.exit(close(txtConn), add = TRUE)
    for(n in numOfUsageCodeLines) {
        whereAmI <- readLines(txtConn, 1)
        argList <- readLines(txtConn, 1)
        exprs <- try(parse(n = -1, text = readLines(txtConn, n)))
        if(inherits(exprs, "try-error"))
            stop(paste("cannot source", gsub("^# ", "", whereAmI)))
        for(i in exprs) {
            yy <- try(eval(i, env = .ArgsEnv))
            if(inherits(yy, "try-error"))
                stop(paste("cannot eval", gsub("^# ", "", whereAmI)))
        }

        lsArgs <- ls(envir = .ArgsEnv, all.names = TRUE)

        if(argList != "# arglist: *internal*") {
            argsInArgList <-
                unlist(strsplit(gsub("# arglist:", "", argList), " +"))
            argsInUsage <-
                unlist(lapply(lsArgs,
                              function(f)
                              names(formals(get(f, envir = .ArgsEnv)))))
            argsInUsageMissingInArgList <-
                argsInUsage[!argsInUsage %in% argsInArgList]
            if(length(argsInUsageMissingInArgList) > 0) {
                writeLines(paste("Undocumented arguments",
                                 gsub("^# usages", "", whereAmI),
                                 ":", sep = ""))
                print(unique(argsInUsageMissingInArgList))
            }
            if(any(duplicated(argsInArgList))) {
                writeLines(paste("Duplicated \\argument entries",
                                 gsub("^# usages", "", whereAmI),
                                 ":", sep = ""))
                print(argsInArgList[duplicated(argsInArgList)])
            }
        }

        ## Clean up .ArgsEnv
        rm(list = lsArgs, envir = .ArgsEnv)
    }

    return(invisible())
}

checkMethods <-
function(dir) {
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
    isBase <- basename(dir) == "base"

    codeFile <- tempfile("Rcode")
    on.exit(unlink(codeFile))
    codeExts <- c("R", "r", "S", "s", "q")
    files <- listFilesWithExts(codeDir, codeExts, path = FALSE)
    files <- file.path(codeDir, files)
    if(file.exists(codeOSDir <- file.path(codeDir, .Platform$OS)))
        files <- c(files, listFilesWithExts(codeOSDir, codeExts))
    file.create(codeFile)
    file.append(codeFile, files)

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
    lib.source(codeFile, env = .CodeEnv)
    lsCode <- ls(envir = .CodeEnv, all.names = TRUE)

    funs <-
        lsCode[sapply(lsCode, function(f)
                      is.function(get(f, envir = .CodeEnv)))]

    envList <- list(.CodeEnv)
    if(!isBase) envList <- c(envList, list(NULL))

    badMethods <- list()
    ## Explicitly deal with functions in base which `look' like S3
    ## methods, but are not.
    baseStopList <-
        c("boxplot.stats",
          "close.screen", "close.socket",
          "format.char", "format.info", "format.pval",
          "plot.new", "plot.window", "plot.xy",
          "split.screen",
          "update.packages")

    isGeneric <- function(fname, envir) {
        f <- get(fname, envir = envir)
        is.function(f) && any(grep("UseMethod", deparse(body(f))))
    }

    checkArgs <- function(g, m, env) {
        ## Do the arguments of method m (in .CodeEnv) `extend' those of
        ## the generic g from environment env?  The method must have all
        ## arguments the generic has, with positional arguments of g in
        ## the same positions for m.
        ## Exception: "..." in the method swallows anything
        gArgs <- ogArgs <- names(formals(get(g, envir = env)))
        mArgs <- omArgs <- names(formals(get(m, envir = .CodeEnv)))
        ## If m is a formula method, its first argument *may* be called
        ## formula.  (Note that any argument name mismatch throws an
        ## error in current S-PLUS versions.)
        if(length(grep("\\.formula$", m)) > 0) {
            gArgs <- gArgs[-1]
            mArgs <- mArgs[-1]
        }
        dotsPos <- which(gArgs == "...")
        ipos <- if(length(dotsPos) > 0)
            seq(from = 1, length = dotsPos - 1)
        else
            seq(along = gArgs)

        dotsPos <- which(mArgs == "...")
        if(length(dotsPos) > 0)
            ipos <- ipos[seq(from = 1, length = dotsPos - 1)]
        PosMatchOK <- all(gArgs[ipos] == mArgs[ipos])
        ArgMatchOK <- all(gArgs %in% mArgs) || length(dotsPos) > 0
        if(PosMatchOK && ArgMatchOK)
            NULL
        else {
            l <- list(ogArgs, omArgs)
            names(l) <- c(g, m)
            list(l)
        }
    }

    for(env in envList) {
        allObjs <- ls(envir = env, all.names = TRUE)
        ## <FIXME>
        genFuns <- allObjs[sapply(allObjs, isGeneric, env)]
        ## This is not good enough for base where we also have generics
        ## which dispatch in C code.  We should also add group methods.
        ## </FIXME>

        for(g in genFuns) {
            ## Find all methods in funs for generic g.  Taken from the
            ## current code for methods().
            name <- paste("^", g, ".", sep = "")
            methods <- grep(gsub("([.[])", "\\\\\\1", name),
                            funs, value = TRUE)
            if(isBase)
                methods <- methods[! methods %in% baseStopList]
            for(m in methods)
                badMethods <- c(badMethods, checkArgs(g, m, env))
        }
    }

    ## Output.
    formatArgs <- function(s)
        paste("function(", paste(s, collapse = ", "), ")", sep = "")
    for(entry in badMethods) {
        writeLines(c(paste(names(entry)[1], ":", sep = ""),
                     strwrap(formatArgs(entry[[1]]),
                             indent = 2, exdent = 11),
                     paste(names(entry)[2], ":", sep = ""),
                     strwrap(formatArgs(entry[[2]]),
                             indent = 2, exdent = 11),
                     ""))
    }

    invisible(badMethods)

}

checkTnF <-
function(file, package, lib.loc = .lib.loc)
{
    fQuote <- function(s) paste("`", s, "'", sep = "")
    if(missing(file)) {
        if(missing(package))
            stop("you must specify `file' or `package'")
        file <- file.path(.find.package(package, lib.loc), "R", package)
    }
    if(!file.exists(file))
        stop(paste("file", fQuote(file), "does not exist"))
    badTnF <- c("T", "F")
    checkTnFandPrint <- function(e, p) {
        if(is.name(e) && (as.character(e) %in% badTnF) && !is.null(p))
            writeLines(paste("found T/F in:", deparse(p)))
        else if(is.call(e) || is.expression(e)) {
            for(i in seq(along = e)) Recall(e[[i]], e)
        }
    }
    exprs <- parse(file = file, n = -1)
    for(i in seq(along = exprs)) checkTnFandPrint(exprs[[i]], NULL)
}
