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

    ## Argument handling.
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
    isBase <- basename(dir) == "base"

    ## Collect usages into docsFile.
    docsFile <- tempfile("Rdocs")
    on.exit(unlink(docsFile))
    docsExts <- c("Rd", "rd")
    files <- listFilesWithExts(docsDir, docsExts, path = FALSE)
    if(isBase) {
        baseStopList <- c("Defunct.Rd", "Deprecated.Rd", "Devices.Rd")
	files <- files[!files %in% baseStopList]
    }
    files <- file.path(docsDir, files)
    if(file.exists(docsOSDir <- file.path(docsDir, .Platform$OS)))
        files <- c(files, listFilesWithExts(docsOSDir, docsExts))
    docsList <- tempfile("Rdocs")
    on.exit(unlink(docsList), add = TRUE)
    writeLines(files, docsList)
    .Script("perl", "extract-usage.pl",
            paste("--mode=args", docsList, docsFile))

    ## Process the usages in the Rd files, one at a time.
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
        if(argList == "# arglist: *internal*") {
            readLines(txtConn, n); next
        }
        exprs <- try(parse(n = -1, text = readLines(txtConn, n)))
        if(inherits(exprs, "try-error"))
            stop(paste("cannot source", gsub("^# ", "", whereAmI)))
        for(i in exprs) {
            yy <- try(eval(i, env = .ArgsEnv))
            if(inherits(yy, "try-error"))
                stop(paste("cannot eval", gsub("^# ", "", whereAmI)))
        }

        lsArgs <- ls(envir = .ArgsEnv, all.names = TRUE)

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
    isBase <- basename(dir) == "base"

    ## Collect code into codeFile.
    codeFile <- tempfile("Rcode")
    on.exit(unlink(codeFile))
    codeExts <- c("R", "r", "S", "s", "q")
    files <- listFilesWithExts(codeDir, codeExts, path = FALSE)
    files <- file.path(codeDir, files)
    if(file.exists(codeOSDir <- file.path(codeDir, .Platform$OS)))
        files <- c(files, listFilesWithExts(codeOSDir, codeExts))
    file.create(codeFile)
    file.append(codeFile, files)

    ## Read code from codeFile into .CodeEnv.
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

    ## Find the function objects in the given package.
    funs <-
        lsCode[sapply(lsCode, function(f)
                      is.function(get(f, envir = .CodeEnv)))]

    isS3Generic <- function(fname, envir) {
        f <- get(fname, envir = envir)
        ((is.function(f)
          && any(grep("^UseMethod", deparse(body(f))[1])))
         || (fname %in% c("as.data.frame", "plot")))
    }

    methodStopList <-
        ## Explicitly deal with functions which `look' like S3 methods,
        ## but are not.
        switch(basename(dir),
               base = c("boxplot.stats",
               "close.screen", "close.socket",
               "format.char", "format.info", "format.pval",
               "plot.new", "plot.window", "plot.xy",
               "split.screen",
               "update.packages"),
               ts = "lag.plot",
               character(0))

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

    ## Now determine the `bad' methods in the function objects of the
    ## package.
    badMethods <- list()
    envList <- list(.CodeEnv)
    if(!isBase) envList <- c(envList, list(as.environment(NULL)))
    for(env in envList) {
        allObjs <- ls(envir = env, all.names = TRUE)
        ## <FIXME>
        genFuns <- allObjs[sapply(allObjs, isS3Generic, env)]
        ## This is not good enough for base where we also have generics
        ## which dispatch in C code.  We should also add group methods.
        ## </FIXME>

        for(g in genFuns) {
            ## Find all methods in funs for generic g.  Taken from the
            ## current code for methods().
            name <- paste("^", g, ".", sep = "")
            methods <- grep(gsub("([.[])", "\\\\\\1", name),
                            funs, value = TRUE)
            methods <- methods[! methods %in% methodStopList]
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
function(file, package, dir, lib.loc = .lib.loc)
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

    checkTnFandPrint <- function(e, p) {
        badTnF <- c("T", "F")
        if(is.name(e) && (as.character(e) %in% badTnF) && !is.null(p))
            writeLines(formatDL("found T/F in",
                                paste(deparse(p), collapse = ""),
                                style = "list"))
        else if(is.recursive(e)) {
            for(i in seq(along = e)) Recall(e[[i]], e)
        }
    }

    if(missing(file)) {
        if(!missing(package)) {
            packageDir <- .find.package(package, lib.loc)
            if(file.exists(file.path(packageDir, "R", "all.rda"))) {
                warning("cannot check R code installed as image")
                return(invisible())
            }
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
    exprs <- parse(file = file, n = -1)
    for(i in seq(along = exprs)) checkTnFandPrint(exprs[[i]], NULL)
}

checkDocStyle <-
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

    ## Collect code into codeFile.
    codeFile <- tempfile("Rcode")
    on.exit(unlink(codeFile))
    codeExts <- c("R", "r", "S", "s", "q")
    files <- listFilesWithExts(codeDir, codeExts, path = FALSE)
    files <- file.path(codeDir, files)
    if(file.exists(codeOSDir <- file.path(codeDir, .Platform$OS)))
        files <- c(files, listFilesWithExts(codeOSDir, codeExts))
    file.create(codeFile)
    file.append(codeFile, files)

    ## Collect usages into docsFile.
    docsFile <- tempfile("Rdocs")
    on.exit(unlink(docsFile), add = TRUE)
    docsExts <- c("Rd", "rd")
    files <- listFilesWithExts(docsDir, docsExts, path = FALSE)
    if(isBase) {
        baseStopList <- character()
	files <- files[!files %in% baseStopList]
    }
    files <- file.path(docsDir, files)
    if(file.exists(docsOSDir <- file.path(docsDir, .Platform$OS)))
        files <- c(files, listFilesWithExts(docsOSDir, docsExts))
    docsList <- tempfile("Rdocs")
    on.exit(unlink(docsList), add = TRUE)
    writeLines(files, docsList)
    .Script("perl", "extract-usage.pl",
            paste("--mode=style", docsList, docsFile))

    ## Read code from codeFile into .CodeEnv.
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

    ## Find the function objects in the given package.
    funs <-
        lsCode[sapply(lsCode, function(f)
                      is.function(get(f, envir = .CodeEnv)))]

    ## Find all generic functions in the given package and (the current)
    ## base package.
    isS3Generic <- function(fname, envir) {
        f <- get(fname, envir = envir)
        ((is.function(f)
          && any(grep("^UseMethod", deparse(body(f))[1])))
         || (fname %in% c("as.data.frame", "plot")))
    }
    allGenerics <- character()
    envList <- list(.CodeEnv)
    if(!isBase) envList <- c(envList, list(as.environment(NULL)))
    for(env in envList) {
        allObjs <- ls(envir = env, all.names = TRUE)        
        allGenerics <-
            c(allGenerics, allObjs[sapply(allObjs, isS3Generic, env)])
    }

    ## Find all methods in the given package for the generic functions
    ## determined above.  Store as a list indexed by the names of the
    ## generic functions.
    methodStopList <-
        ## Explicitly deal with functions which `look' like S3 methods,
        ## but are not.
        switch(basename(dir),
               base = c("boxplot.stats",
               "close.screen", "close.socket",
               "format.char", "format.info", "format.pval",
               "plot.new", "plot.window", "plot.xy",
               "split.screen",
               "update.packages"),
               ts = "lag.plot",
               character(0))
    methodsInPackage <- sapply(allGenerics, function(g) {
        name <- paste("^", g, ".", sep = "")
        methods <- grep(gsub("([.[])", "\\\\\\1", name),
                        funs, value = TRUE)
        methods <- methods[! methods %in% methodStopList]
        methods
    })
    allMethodsInPackage <- unlist(methodsInPackage)

    ## Process the usages in the Rd files, one at a time.
    .DocsEnv <- new.env()
    txt <- readLines(docsFile)
    ind <- grep("^# usages in file", txt)
    ## Use a text connection for reading the blocks determined by ind.
    ## Alternatively, we could split txt into a list of the blocks.
    numOfUsageCodeLines <- diff(c(ind, length(txt) + 1)) - 1
    txtConn <- textConnection(paste(txt, collapse = "\n"))
    on.exit(close(txtConn), add = TRUE)
    for(n in numOfUsageCodeLines) {
        whereAmI <- readLines(txtConn, 1)
        usageTxt <- readLines(txtConn, n)
        ## Note: Special \method{GENERIC}{CLASS} Rd markup was preserved
        ## by calling extract-usage in mode `style'.  We keep this in
        ## usageTxt for later, but of course need to replace it by the
        ## GENERIC.CLASS S3 function names for parsin.
        exprs <- try(parse(n = -1,
                           text = gsub(paste("\\\\method",
                                             "{([a-zA-Z0-9.]+)}",
                                             "{([a-zA-Z0-9.]+)}",
                                             sep = ""),
                                       "\\1.\\2", usageTxt)))
        if(inherits(exprs, "try-error"))
            stop(paste("cannot source", gsub("^# ", "", whereAmI)))
        for(i in exprs) {
            yy <- try(eval(i, env = .DocsEnv))
            if(inherits(yy, "try-error"))
                stop(paste("cannot eval", gsub("^# ", "", whereAmI)))
        }

        usages <- ls(envir = .DocsEnv, all.names = TRUE)

        methodsWithGeneric <-
            sapply(usages[usages %in% allGenerics],
                   function(g) usages[usages %in%
                                      methodsInPackage[[g]]],
                   simplify = FALSE)
        methodsWithGeneric <-
            methodsWithGeneric[lapply(methodsWithGeneric, length) > 0]
            
        methodsWithFullName <-
            sapply(usages[usages %in% allMethodsInPackage],
                   function(f) any(grep(paste(f, "*<-"), usageTxt)))
        methodsWithFullName <-
            methodsWithFullName[methodsWithFullName == TRUE]

        ## Output.
        if((length(methodsWithGeneric) > 0) ||
           (length(methodsWithFullName > 0))) {
            writeLines(paste("Usages in",
                             gsub("^# usages in", "", whereAmI),
                             ":", sep = ""))
            if(length(methodsWithGeneric) > 0) {
                writeLines("Methods shown alongside generic:")
                for(g in names(methodsWithGeneric)) {
                    methods <- paste(methodsWithGeneric[[g]],
                                     collapse = " ")
                    writeLines(strwrap(paste(g, ": ", methods,
                                             sep = ""),
                                       indent = 2, exdent = 4))
                }
            }
            if(length(methodsWithFullName > 0)) {
                writeLines("Methods shown with their own name:")
                writeLines(strwrap(paste(names(methodsWithFullName)),
                                   indent = 2, exdent = 4))
            }
            writeLines("")
        }
                
        rm(list = usages, envir = .DocsEnv)
    }

    return(invisible())
}
