### Internal functions.

sQuote <- function(s) paste("`", s, "'", sep = "")
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

### The real stuff.

undoc <-
function(package, dir, lib.loc = NULL)
{
    useSaveImage <- FALSE
    if(!missing(package)) {
        if(length(package) != 1)
            stop("argument `package' must be of length 1")
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
            stop(paste("directory", sQuote(dir), "does not exist"))
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
            files <- listFilesWithExts(codeDir, codeExts)
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
        files <- listFilesWithExts(dataDir, dataExts)
        files <- files[!duplicated(sub("\\.[A-Za-z]*$", "", files))]
        dataEnv <- new.env()
        dataObjs <- NULL
        if(any(i <- grep("\\.\(R\|r\)$", files))) {
            for(f in files[i]) {
                yy <- try(sys.source(f, envir = dataEnv, chdir = TRUE))
                if(inherits(yy, "try-error"))
                    stop(paste("cannot source data file", sQuote(f)))
                new <- ls(envir = dataEnv, all.names = TRUE)
                dataObjs <- c(dataObjs, new)
                rm(list = new, envir = dataEnv)
            }
            files <- files[-i]
        }
        if(any(i <- grep("\\.\(RData\|rdata\|rda\)$", files))) {
            for(f in files[i]) {
                yy <- try(load(f, envir = dataEnv))
                if(inherits(yy, "try-error"))
                    stop(paste("cannot load data file", sQuote(f)))
                new <- ls(envir = dataEnv, all.names = TRUE)
                dataObjs <- c(dataObjs, new)
                rm(list = new, envir = dataEnv)
            }
            files <- files[-i]
        }
        if(length(files) > 0)
            dataObjs <- c(dataObjs,
                          basename(sub("\\.[A-Za-z]*$", "", files)))
        allObjs <- c(allObjs, dataObjs)
    }

    ## Undocumented objects?
    if(is.null(allObjs))
        warning("Neither code nor data objects found")
    else
        allObjs[! allObjs %in% c(objsdocs, ".First.lib", ".Last.lib")]
}

codoc <-
function(package, dir, lib.loc = NULL,
         use.values = FALSE, use.positions = TRUE,
         ignore.generic.functions = FALSE,
         keep.tempfiles = FALSE,
         verbose = getOption("verbose"))
{
    ## Argument handling.
    if(!missing(keep.tempfiles))
        warning("argument `keep.tempfiles' is deprecated")
    unlinkOnExitFiles <- NULL
    on.exit(unlink(unlinkOnExitFiles))

    if(!missing(package)) {
        if(length(package) != 1)
            stop("argument `package' must be of length 1")
        dir <- .find.package(package, lib.loc)
        ## Using package installed in `dir' ...
        if(!file.exists(codeDir <- file.path(dir, "R")))
            stop(paste("directory", sQuote(dir),
                       "does not contain R code"))
        if(!file.exists(docsDir <- file.path(dir, "man")))
            stop(paste("directory", sQuote(dir),
                       "does not contain Rd sources"))
        isBase <- basename(dir) == "base"

        ## Load package into codeEnv
        if(!isBase) {
            ## Need to capture all output and messages.
            outConn <- textConnection("out", "w")
            sink(outConn, type = "output")
            sink(outConn, type = "message")
            yy <- try({
                pos <- match(paste("package", package, sep = ":"),
                             search())
                if(!is.na(pos))
                    detach(pos = pos)
                library(package, lib.loc = lib.loc,
                        character.only = TRUE)
            })
            if(inherits(yy, "try-error"))
                stop(yy)
            sink(type = "message")
            sink(type = "output")
            close(outConn)
        }
        codeEnv <-
            as.environment(match(paste("package", package, sep = ":"),
                                 search()))
    }
    else {
        if(missing(dir))
            stop("you must specify `package' or `dir'")
        ## Using sources from directory `dir' ...
        if(!file.exists(dir))
            stop(paste("directory", sQuote(dir), "does not exist"))
        else
            ## maybe perform tilde expansion on `dir'
            dir <- file.path(dirname(dir), basename(dir))
        if(!file.exists(codeDir <- file.path(dir, "R")))
            stop(paste("directory", sQuote(dir),
                       "does not contain R code"))
        if(!file.exists(docsDir <- file.path(dir, "man")))
            stop(paste("directory", sQuote(dir),
                       "does not contain Rd sources"))
        isBase <- basename(dir) == "base"
        
        ## Collect code in codeFile.
        codeFile <- tempfile("Rcode")
        unlinkOnExitFiles <- c(unlinkOnExitFiles, codeFile)
        codeExts <- c("R", "r", "S", "s", "q")
        files <- listFilesWithExts(codeDir, codeExts)
        if(file.exists(codeOSDir <- file.path(codeDir, .Platform$OS)))
            files <- c(files, listFilesWithExts(codeOSDir, codeExts))
        file.create(codeFile)
        file.append(codeFile, files)
        
        ## Read code from codeFile into codeEnv
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
        codeEnv <- new.env()
        if(verbose)
            cat("Reading code from", sQuote(codeFile), "\n")        
        yy <- try(lib.source(codeFile, env = codeEnv))
        if(inherits(yy, "try-error")) {
            stop("cannot source package code")
        }

    }
        
    lsCode <- ls(envir = codeEnv, all.names = TRUE)
    
    ## Find the function objects to work on.
    funs <- lsCode[sapply(lsCode, function(f) {
        f <- get(f, envir = codeEnv)
        is.function(f) && (length(formals(f)) > 0)
    }) == TRUE]
    if(ignore.generic.functions) {
        isS3Generic <- function(f) {
            any(grep("^UseMethod",
                     deparse(body(get(f, envir = codeEnv)))[1]))
        }
        funs <- funs[sapply(funs, isS3Generic) == FALSE]
    }
    ## <FIXME>
    ## Sourcing all R code files in the package is a problem for base,
    ## where this misses the .Primitive functions.  Hence, when checking
    ## base for objects shown in \usage but missing from the code, we
    ## get the primitive functions from the version of R we are using.
    ## Maybe one day we will have R code for the primitives as well ...
    if(isBase) {
        baseObjs <- ls(envir = as.environment(NULL), all.names = TRUE)
        isPrimitive <- function(fname, envir) {
            f <- get(fname, envir = envir)
            is.function(f) && any(grep("^\\.Primitive", deparse(f)))
        }
        lsCode <-
            c(lsCode, baseObjs[sapply(baseObjs, isPrimitive, NULL)],
              c(".First.lib", ".Last.lib", ".Random.seed"))
    }
    ## </FIXME>

    docsEnv <- new.env()
    checkCoDoc <- function(f) {
        ffc <- formals(get(f, envir = codeEnv))
        ffd <- formals(get(f, envir = docsEnv))
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

    ## Collect usages into docsFile.
    docsFile <- tempfile("Rdocs")
    unlinkOnExitFiles <- c(unlinkOnExitFiles, docsFile)
    docsExts <- c("Rd", "rd")
    files <- listFilesWithExts(docsDir, docsExts)
    if(file.exists(docsOSDir <- file.path(docsDir, .Platform$OS)))
        files <- c(files, listFilesWithExts(docsOSDir, docsExts))
    docsList <- tempfile("Rdocs")
    unlinkOnExitFiles <- c(unlinkOnExitFiles, docsList)
    writeLines(files, docsList)
    .Script("perl", "extract-usage.pl", paste(docsList, docsFile))

    ## Process the usages in the documentation objects, one at a time.
    lsDocs <- character()
    badUsages <- list()
    if(verbose)
        cat("Reading docs from", sQuote(docsFile), "\n")
    txt <- readLines(docsFile)
    ind <- grep("^# usages in documentation object", txt)
    ## Use a text connection for reading the blocks determined by ind.
    ## Alternatively, we could split txt into a list of the blocks.
    numOfUsageCodeLines <- diff(c(ind, length(txt) + 1)) - 1
    txtConn <- textConnection(paste(txt, collapse = "\n"))
    on.exit(close(txtConn), add = TRUE)
    for(n in numOfUsageCodeLines) {
        docObj <- gsub("^# usages in documentation object ", "",
                       readLines(txtConn, 1))
        if(isBase && docObj %in% c("Defunct", "Devices")) {
            readLines(txtConn, n); next
        }
        exprs <- try(parse(n = -1, text = readLines(txtConn, n)))
        if(inherits(exprs, "try-error"))
            stop(paste("cannot source usages in documentation object",
                       sQuote(docObj)))
        for(i in exprs) {
            yy <- try(eval(i, env = docsEnv))
            if(inherits(yy, "try-error"))
                stop(paste("cannot eval usages in documentation object",
                           sQuote(docObj)))
        }

        badUsagesInFile <- list()
        usages <- ls(envir = docsEnv, all.names = TRUE)
        for(f in usages[usages %in% funs])
            badUsagesInFile <- c(badUsagesInFile, checkCoDoc(f))
        if(length(badUsagesInFile) > 0)
            badUsages[[docObj]] <- badUsagesInFile

        usagesNotInCode <- usages[! usages %in% lsCode]
        if(length(usagesNotInCode) > 0) {
            writeLines(paste("Objects with usage in documentation",
                             "object", sQuote(docObj),
                             "but missing from code:"))
            print(unique(usagesNotInCode))
            writeLines("")
        }

        lsDocs <- c(lsDocs, usages)
        rm(list = usages, envir = docsEnv)
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
        writeLines(paste("Codoc mismatches from documentation object ",
                         sQuote(fname), ":", sep = ""))
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

checkAssignFuns <-
function(package, dir, lib.loc = NULL)
{
    ## Argument handling.
    if(!missing(package)) {
        if(length(package) != 1)
            stop("argument `package' must be of length 1")
        dir <- .find.package(package, lib.loc)
        ## Using package installed in `dir' ...
        if(!file.exists(codeDir <- file.path(dir, "R")))
            stop(paste("directory", sQuote(dir),
                       "does not contain R code"))
        isBase <- basename(dir) == "base"

        ## Load package into codeEnv
        if(!isBase) {
            ## Need to capture all output and messages.
            outConn <- textConnection("out", "w")
            sink(outConn, type = "output")
            sink(outConn, type = "message")
            yy <- try({
                pos <- match(paste("package", package, sep = ":"),
                             search())
                if(!is.na(pos))
                    detach(pos = pos)
                library(package, lib.loc = lib.loc,
                        character.only = TRUE)
            })
            if(inherits(yy, "try-error"))
                stop(yy)
            sink(type = "message")
            sink(type = "output")
            close(outConn)
        }
        codeEnv <-
            as.environment(match(paste("package", package, sep = ":"),
                                 search()))
    }
    else {
        if(missing(dir))
            stop("you must specify `package' or `dir'")
        ## Using sources from directory `dir' ...
        if(!file.exists(dir))
            stop(paste("directory", sQuote(dir), "does not exist"))
        else
            ## maybe perform tilde expansion on `dir'
            dir <- file.path(dirname(dir), basename(dir))
        if(!file.exists(codeDir <- file.path(dir, "R")))
            stop(paste("directory", sQuote(dir),
                       "does not contain R code"))
        isBase <- basename(dir) == "base"

        ## Collect code into codeFile.
        codeFile <- tempfile("Rcode")
        on.exit(unlink(codeFile))
        codeExts <- c("R", "r", "S", "s", "q")
        files <- listFilesWithExts(codeDir, codeExts)
        if(file.exists(codeOSDir <- file.path(codeDir, .Platform$OS)))
            files <- c(files, listFilesWithExts(codeOSDir, codeExts))
        file.create(codeFile)
        file.append(codeFile, files)

        ## Read code from codeFile into codeEnv.
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
        codeEnv <- new.env()
        yy <- try(lib.source(codeFile, env = codeEnv))
        if(inherits(yy, "try-error")) {
            stop("cannot source package code")
        }
    }
        
    lsCode <- ls(envir = codeEnv, all.names = TRUE)

    ## Find the assignment functions in the given package.
    assignFuns <- lsCode[grep("<-", lsCode)]
    ## Find the assignment functions with last arg not named `value'.
    badAssignFuns <-
        assignFuns[sapply(assignFuns, function(f) {
            argNames <- names(formals(get(f, envir = codeEnv)))
            argNames[length(argNames)] != "value"
        }) == TRUE]

    badAssignFuns
}

checkDocArgs <-
function(package, dir, lib.loc = NULL)
{
    ## Argument handling.
    if(!missing(package)) {
        if(length(package) != 1)
            stop("argument `package' must be of length 1")
        dir <- .find.package(package, lib.loc)
        ## Using package installed in `dir' ...
    }
    else {
        if(missing(dir))
            stop("you must specify `package' or `dir'")
        ## Using sources from directory `dir' ...
        if(!file.exists(dir))
            stop(paste("directory", sQuote(dir), "does not exist"))
        else
            ## maybe perform tilde expansion on `dir'
            dir <- file.path(dirname(dir), basename(dir))
    }

    if(!file.exists(docsDir <- file.path(dir, "man")))
        stop(paste("directory", sQuote(dir),
                   "does not contain Rd sources"))
    isBase <- basename(dir) == "base"

    ## Collect usages into docsFile.
    docsFile <- tempfile("Rdocs")
    on.exit(unlink(docsFile))
    docsExts <- c("Rd", "rd")
    files <- listFilesWithExts(docsDir, docsExts)
    if(file.exists(docsOSDir <- file.path(docsDir, .Platform$OS)))
        files <- c(files, listFilesWithExts(docsOSDir, docsExts))
    docsList <- tempfile("Rdocs")
    on.exit(unlink(docsList), add = TRUE)
    writeLines(files, docsList)
    .Script("perl", "extract-usage.pl",
            paste("--mode=args", docsList, docsFile))

    ## Process the usages in the documentation objects, one at a time.
    argsEnv <- new.env()
    txt <- readLines(docsFile)
    ind <- grep("^# usages in documentation object", txt)
    ## Use a text connection for reading the blocks determined by ind.
    ## Alternatively, we could split txt into a list of the blocks.
    numOfUsageCodeLines <- diff(c(ind, length(txt) + 1)) - 2
    txtConn <- textConnection(paste(txt, collapse = "\n"))
    on.exit(close(txtConn), add = TRUE)
    for(n in numOfUsageCodeLines) {
        docObj <- gsub("^# usages in documentation object ", "",
                       readLines(txtConn, 1))
        if(isBase
           && docObj %in% c("Defunct", "Deprecated", "Devices")) { 
            readLines(txtConn, n + 1); next
        }
        argList <- readLines(txtConn, 1)
        if(argList == "# arglist: *internal*") {
            readLines(txtConn, n); next
        }
        exprs <- try(parse(n = -1, text = readLines(txtConn, n)))
        if(inherits(exprs, "try-error"))
            stop(paste("cannot source usages in documentation object",
                       sQuote(docObj)))
        for(i in exprs) {
            yy <- try(eval(i, env = argsEnv))
            if(inherits(yy, "try-error"))
                stop(paste("cannot eval usages in documentation object",
                           sQuote(docObj)))
        }

        lsArgs <- ls(envir = argsEnv, all.names = TRUE)

        argsInArgList <-
            unlist(strsplit(gsub("# arglist:", "", argList), " +"))
        argsInUsage <-
            unlist(lapply(lsArgs,
                          function(f)
                          names(formals(get(f, envir = argsEnv)))))
        argsInUsageMissingInArgList <-
            argsInUsage[!argsInUsage %in% argsInArgList]
        if(length(argsInUsageMissingInArgList) > 0) {
            writeLines(paste("Undocumented arguments",
                             " in documentation object ",
                             sQuote(docObj), ":", sep = ""))
            print(unique(argsInUsageMissingInArgList))
        }
        if(any(duplicated(argsInArgList))) {
            writeLines(paste("Duplicated \\argument entries",
                             " in documentation object ",
                             sQuote(docObj), ":", sep = ""))
            print(argsInArgList[duplicated(argsInArgList)])
        }

        ## Clean up argsEnv
        rm(list = lsArgs, envir = argsEnv)
    }

    return(invisible())
}

checkDocStyle <-
function(package, dir, lib.loc = NULL)
{
    ## Argument handling.
    if(!missing(package)) {
        if(length(package) != 1)
            stop("argument `package' must be of length 1")
        dir <- .find.package(package, lib.loc)
        ## Using package installed in `dir' ...
        if(!file.exists(codeDir <- file.path(dir, "R")))
            stop(paste("directory", sQuote(dir),
                       "does not contain R code"))
        if(!file.exists(docsDir <- file.path(dir, "man")))
            stop(paste("directory", sQuote(dir),
                       "does not contain Rd sources"))
        isBase <- basename(dir) == "base"

        ## Load package into codeEnv
        if(!isBase) {
            ## Need to capture all output and messages.
            outConn <- textConnection("out", "w")
            sink(outConn, type = "output")
            sink(outConn, type = "message")
            yy <- try({
                pos <- match(paste("package", package, sep = ":"),
                             search())
                if(!is.na(pos))
                    detach(pos = pos)
                library(package, lib.loc = lib.loc,
                        character.only = TRUE)
            })
            if(inherits(yy, "try-error"))
                stop(yy)
            sink(type = "message")
            sink(type = "output")
            close(outConn)
        }
        codeEnv <-
            as.environment(match(paste("package", package, sep = ":"),
                                 search()))
    }
    else {
        if(missing(dir))
            stop("you must specify `package' or `dir'")
        ## Using sources from directory `dir' ...
        if(!file.exists(dir))
            stop(paste("directory", sQuote(dir), "does not exist"))
        else
            ## maybe perform tilde expansion on `dir'
            dir <- file.path(dirname(dir), basename(dir))
        if(!file.exists(codeDir <- file.path(dir, "R")))
            stop(paste("directory", sQuote(dir),
                       "does not contain R code"))
        if(!file.exists(docsDir <- file.path(dir, "man")))
            stop(paste("directory", sQuote(dir),
                       "does not contain Rd sources"))
        isBase <- basename(dir) == "base"

        ## Collect code into codeFile.
        codeFile <- tempfile("Rcode")
        on.exit(unlink(codeFile))
        codeExts <- c("R", "r", "S", "s", "q")
        files <- listFilesWithExts(codeDir, codeExts)
        if(file.exists(codeOSDir <- file.path(codeDir, .Platform$OS)))
            files <- c(files, listFilesWithExts(codeOSDir, codeExts))
        file.create(codeFile)
        file.append(codeFile, files)

        ## Read code from codeFile into codeEnv.
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
        codeEnv <- new.env()
        yy <- try(lib.source(codeFile, env = codeEnv))
        if(inherits(yy, "try-error")) {
            stop("cannot source package code")
        }
    }

    lsCode <- ls(envir = codeEnv, all.names = TRUE)

    ## Find the function objects in the given package.
    funs <-
        lsCode[sapply(lsCode, function(f)
                      is.function(get(f, envir = codeEnv))) == TRUE]

    ## Find all generic functions in the given package and (the current)
    ## base package.
    isS3Generic <- function(fname, envir) {
        f <- get(fname, envir = envir)
        ((is.function(f)
          && any(grep("^UseMethod", deparse(body(f))[1])))
         || (fname %in% c("as.data.frame", "plot")))
    }
    allGenerics <- character()
    envList <- list(codeEnv)
    if(!isBase) envList <- c(envList, list(as.environment(NULL)))
    for(env in envList) {
        allObjs <- ls(envir = env, all.names = TRUE)
        allGenerics <-
            c(allGenerics,
              allObjs[sapply(allObjs, isS3Generic, env) == TRUE])
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
    
    ## Collect usages into docsFile.
    docsFile <- tempfile("Rdocs")
    on.exit(unlink(docsFile), add = TRUE)
    docsExts <- c("Rd", "rd")
    files <- listFilesWithExts(docsDir, docsExts)
    if(file.exists(docsOSDir <- file.path(docsDir, .Platform$OS)))
        files <- c(files, listFilesWithExts(docsOSDir, docsExts))
    docsList <- tempfile("Rdocs")
    on.exit(unlink(docsList), add = TRUE)
    writeLines(files, docsList)
    .Script("perl", "extract-usage.pl",
            paste("--mode=style", docsList, docsFile))

    ## Process the usages in the documentation objects, one at a time.
    docsEnv <- new.env()
    txt <- readLines(docsFile)
    ind <- grep("^# usages in documentation object", txt)
    ## Use a text connection for reading the blocks determined by ind.
    ## Alternatively, we could split txt into a list of the blocks.
    numOfUsageCodeLines <- diff(c(ind, length(txt) + 1)) - 1
    txtConn <- textConnection(paste(txt, collapse = "\n"))
    on.exit(close(txtConn), add = TRUE)
    for(n in numOfUsageCodeLines) {
        docObj <- gsub("^# usages in documentation object ", "",
                       readLines(txtConn, 1))
        usageTxt <- readLines(txtConn, n)
        ## Note: Special \method{GENERIC}{CLASS} Rd markup was preserved
        ## by calling extract-usage in mode `style'.  We keep this in
        ## usageTxt for later, but of course need to replace it by the
        ## GENERIC.CLASS S3 function names for parsing.
        exprs <- try(parse(n = -1,
                           text = gsub(paste("\\\\method",
                                             "{([a-zA-Z0-9.]+)}",
                                             "{([a-zA-Z0-9.]+)}",
                                             sep = ""),
                                       "\\1.\\2", usageTxt)))
        if(inherits(exprs, "try-error"))
            stop(paste("cannot source usages in documentation object",
                       sQuote(docObj)))
        for(i in exprs) {
            yy <- try(eval(i, env = docsEnv))
            if(inherits(yy, "try-error"))
                stop(paste("cannot eval usages in documentation object",
                           sQuote(docObj)))
        }

        usages <- ls(envir = docsEnv, all.names = TRUE)

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
            writeLines(paste("Usages in documentation object",
                             sQuote(docObj), ":", sep = ""))
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

        rm(list = usages, envir = docsEnv)
    }

    return(invisible())
}

checkFF <-
function(package, dir, file, lib.loc = NULL,
         verbose = getOption("verbose"))
{
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

    if(!missing(package)) {
        if(length(package) != 1)
            stop("argument `package' must be of length 1")
        packageDir <- .find.package(package, lib.loc)
        file <- file.path(packageDir, "R", "all.rda")
        if(file.exists(file))
            useSaveImage <- TRUE
        else
            file <- file.path(packageDir, "R", package)
    }
    else if(!missing(dir)) {
        if(!file.exists(dir))
            stop(paste("directory", sQuote(dir), "does not exist"))
        else
            ## maybe perform tilde expansion on `dir'
            dir <- file.path(dirname(dir), basename(dir))
        if(!file.exists(codeDir <- file.path(dir, "R")))
            stop(paste("directory", sQuote(dir),
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
    else if(missing(file)) {
        stop("you must specify `package', `dir' or `file'")
    }
    
    if(!file.exists(file))
        stop(paste("file", sQuote(file), "does not exist"))
    FFfuns <- c(".C", ".Fortran", ".Call", ".External",
                ".Call.graphics", ".External.graphics")

    exprs <- if(useSaveImage) {
        writeLines("loading saved image ...")
        codeEnv <- new.env()
        load(file, envir = codeEnv)
        lapply(ls(envir = codeEnv, all.names = TRUE),
               function(f) {
                   f <- get(f, envir = codeEnv)
                   if(typeof(f) == "closure") body(f) else NULL
               })
    }
    else
        parse(file = file, n = -1)
    for(i in seq(along = exprs)) checkFFPackageArg(exprs[[i]])
}

checkMethods <-
function(package, dir, lib.loc = NULL)
{
    ## Argument handling.
    if(!missing(package)) {
        if(length(package) != 1)
            stop("argument `package' must be of length 1")
        dir <- .find.package(package, lib.loc)
        ## Using package installed in `dir' ...
        if(!file.exists(codeDir <- file.path(dir, "R")))
            stop(paste("directory", sQuote(dir),
                       "does not contain R code"))
        isBase <- basename(dir) == "base"

        ## Load package into codeEnv
        if(!isBase) {
            ## Need to capture all output and messages.
            outConn <- textConnection("out", "w")
            sink(outConn, type = "output")
            sink(outConn, type = "message")
            yy <- try({
                pos <- match(paste("package", package, sep = ":"),
                             search())
                if(!is.na(pos))
                    detach(pos = pos)
                library(package, lib.loc = lib.loc,
                        character.only = TRUE)
            })
            if(inherits(yy, "try-error"))
                stop(yy)
            sink(type = "message")
            sink(type = "output")
            close(outConn)
        }
        codeEnv <-
            as.environment(match(paste("package", package, sep = ":"),
                                 search()))
    }
    else {
        if(missing(dir))
            stop("you must specify `package' or `dir'")
        ## Using sources from directory `dir' ...
        if(!file.exists(dir))
            stop(paste("directory", sQuote(dir), "does not exist"))
        else
            ## maybe perform tilde expansion on `dir'
            dir <- file.path(dirname(dir), basename(dir))
        if(!file.exists(codeDir <- file.path(dir, "R")))
            stop(paste("directory", sQuote(dir),
                       "does not contain R code"))
        isBase <- basename(dir) == "base"

        ## Collect code into codeFile.
        codeFile <- tempfile("Rcode")
        on.exit(unlink(codeFile))
        codeExts <- c("R", "r", "S", "s", "q")
        files <- listFilesWithExts(codeDir, codeExts)
        if(file.exists(codeOSDir <- file.path(codeDir, .Platform$OS)))
            files <- c(files, listFilesWithExts(codeOSDir, codeExts))
        file.create(codeFile)
        file.append(codeFile, files)
        
        ## Read code from codeFile into codeEnv.
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
        codeEnv <- new.env()
        yy <- try(lib.source(codeFile, env = codeEnv))
        if(inherits(yy, "try-error")) {
            stop("cannot source package code")
        }
    }

    lsCode <- ls(envir = codeEnv, all.names = TRUE)

    ## Find the function objects in the given package.
    funs <-
        lsCode[sapply(lsCode, function(f)
                      is.function(get(f, envir = codeEnv))) == TRUE]

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
               sm = "print.graph",
               character(0))

    checkArgs <- function(g, m, env) {
        ## Do the arguments of method m (in codeEnv) `extend' those of
        ## the generic g from environment env?  The method must have all
        ## arguments the generic has, with positional arguments of g in
        ## the same positions for m.
        ## Exception: "..." in the method swallows anything
        gArgs <- ogArgs <- names(formals(get(g, envir = env)))
        mArgs <- omArgs <- names(formals(get(m, envir = codeEnv)))
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
    envList <- list(codeEnv)
    if(!isBase) envList <- c(envList, list(as.environment(NULL)))
    for(env in envList) {
        allObjs <- ls(envir = env, all.names = TRUE)
        ## <FIXME>
        genFuns <- allObjs[sapply(allObjs, isS3Generic, env) == TRUE]
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
function(package, dir, file, lib.loc = NULL)
{
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

    if(!missing(package)) {
        if(length(package) != 1)
            stop("argument `package' must be of length 1")
        packageDir <- .find.package(package, lib.loc)
        if(file.exists(file.path(packageDir, "R", "all.rda"))) {
            warning("cannot check R code installed as image")
            return(invisible())
        }
        file <- file.path(packageDir, "R", package)
    }
    else if(!missing(dir)) {
        if(!file.exists(dir))
            stop(paste("directory", sQuote(dir), "does not exist"))
        else
            ## maybe perform tilde expansion on `dir'
            dir <- file.path(dirname(dir), basename(dir))
        if(!file.exists(codeDir <- file.path(dir, "R")))
            stop(paste("directory", sQuote(dir),
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
    else if(missing(file)) {
        stop("you must specify `package', `dir' or `file'")
    }

    if(!file.exists(file))
        stop(paste("file", sQuote(file), "does not exist"))
    exprs <- parse(file = file, n = -1)
    for(i in seq(along = exprs)) checkTnFandPrint(exprs[[i]], NULL)
}
