### * undoc

undoc <-
function(package, dir, lib.loc = NULL)
{
    nsInfo <- NULL

    ## Argument handling.
    if(!missing(package)) {
        if(length(package) != 1)
            stop(paste("argument", sQuote("package"),
                       "must be of length 1"))

        dir <- .find.package(package, lib.loc)
        ## Using package installed in @code{dir} ...
        helpIndex <- file.path(dir, "help", "AnIndex")
        if(!.fileTest("-f", helpIndex))
            stop(paste("directory", sQuote(dir),
                       "contains no help index"))
        isBase <- package == "base"

        ## Find all documented topics from the help index.
        allDocTopics <- sort(scan(file = helpIndex,
                                  what = list("", ""),
                                  quiet = TRUE, sep="\t")[[1]])

        ## Load package into codeEnv.
        if(!isBase)
            .loadPackageQuietly(package, lib.loc)
        codeEnv <-
            as.environment(match(paste("package", package, sep = ":"),
                                 search()))
    }
    else {
        if(missing(dir))
            stop(paste("you must specify", sQuote("package"),
                       "or", sQuote("dir")))
        if(!.fileTest("-d", dir))
            stop(paste("directory", sQuote(dir), "does not exist"))
        else
            dir <- .convertFilePathToAbsolute(dir)
        docsDir <- file.path(dir, "man")
        if(!.fileTest("-d", docsDir))
            stop(paste("directory", sQuote(dir),
                       "does not contain Rd sources"))
        isBase <- basename(dir) == "base"

        ## Find all documented topics from the Rd sources.
        files <- .listFilesWithType(docsDir, "docs")
        aliases <- character(0)
        for(f in files) {
            aliases <- c(aliases,
                         grep("^\\\\alias", readLines(f), value = TRUE))
        }
        allDocTopics <- gsub("\\\\alias{(.*)}.*", "\\1", aliases)
        allDocTopics <- gsub("\\\\%", "%", allDocTopics)
        allDocTopics <- gsub(" ", "", allDocTopics)
        allDocTopics <- sort(unique(allDocTopics))

        codeEnv <- new.env()
        codeDir <- file.path(dir, "R")
        if(.fileTest("-d", codeDir)) {
            ## Collect code in codeFile.
            codeFile <- tempfile("Rcode")
            on.exit(unlink(codeFile))
            files <- .listFilesWithType(codeDir, "code")
            file.create(codeFile)
            file.append(codeFile, files)
            ## Read code from codeFile into codeEnv
            yy <- try(.sourceAssignments(codeFile, env = codeEnv))
            if(inherits(yy, "try-error")) {
                stop("cannot source package code")
            }
            if(file.exists(file.path(dir, "NAMESPACE")))
                nsInfo <- parseNamespaceFile(basename(dir), dirname(dir))
        }
    }

    codeObjs <- ls(envir = codeEnv, all.names = TRUE)
    if(length(nsInfo)) {
        ## Look only at exported objects (and not declared S3 methods).
        OK <- codeObjs[codeObjs %in% nsInfo$exports]
        for (p in nsInfo$exportPatterns)
            OK <- c(OK, grep(p, codeObjs, value = TRUE))
        codeObjs <- unique(OK)
    }

    dataObjs <- character(0)
    dataDir <- file.path(dir, "data")
    if(.fileTest("-d", dataDir)) {
        files <- .listFilesWithType(dataDir, "data")
        files <- files[!duplicated(sub("\\.[A-Za-z]*$", "", files))]
        dataEnv <- new.env()
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
			  sapply(sub("\\.[A-Za-z]*$", "", files), basename))
    }

    ## Undocumented objects?
    if((length(codeObjs) == 0) && (length(dataObjs) == 0))
        warning("Neither code nor data objects found")

    if(!isBase) {
        ## Code objects in add-on packages with names starting with a
        ## dot are considered 'internal' (not user-level) by
        ## convention.
        ## <FIXME>
        ## Not clear whether everyone believes in this convention.
        ## We used to have
        ##   allObjs[! allObjs %in% c(allDocTopics,
        ##                            ".First.lib", ".Last.lib")]
        ## i.e., only exclude '.First.lib' and '.Last.lib'.
        codeObjs <- grep("^[^.].*", codeObjs, value = TRUE)
        ## Note that this also allows us to get rid of S4 meta objects
        ## (with names starting with '.__C__' or '.__M__'; well, as long
        ## as there are none in base).
        ## </FIXME>

        ## <FIXME>
        ## Need to do something about S4 generic functions 'created' by
        ## setGeneric() or setMethod() on 'ordinary' functions.  Short
        ## term, we do this by checking for S4 generics in a package
        ## which come from another one.  Long term, we need dynamic
        ## documentation ...
        if(!is.na(match("package:methods", search()))) {
            codeObjs <-
                codeObjs[sapply(codeObjs, function(f) {
                    f <- get(f, envir = codeEnv)
                    fAttr <- c(class(f), attr(f, "package"))
                    (length(fAttr) == 2
                     && fAttr[1] == "genericFunction"
                     && fAttr[2] != basename(dir))
                }) == FALSE]
        }
        ## </FIXME>
        
        ## Allow group generics to be undocumented other than in base.
        ## In particular, those from methods partially duplicate base
        ## and are documented in base's groupGenerics.Rd.
        codeObjs <-
            codeObjs[! codeObjs %in%
                     c("Arith", "Compare", "Complex", "Math", "Math2",
                       "Ops", "Summary")]
    }

    undocObjs <- list(code = codeObjs[! codeObjs %in% allDocTopics],
                      data = dataObjs[! dataObjs %in% allDocTopics])

    if(!is.na(match("package:methods", search()))) {
        S4ClassObjs <- getClasses(codeEnv)
        ## Note that currently, topicName() is not vectorized in its
        ## 'topic' argument (so that it can perform mangling for S4
        ## methods), hence the unlist/lapply construction.
        undocObjs <-
            c(undocObjs,
              list("S4 class" =
                   S4ClassObjs[! unlist(lapply(S4ClassObjs,
                                               function(u)
                                               topicName("class", u)))
                               %in% allDocTopics]))
    }

    class(undocObjs) <- "undoc"
    undocObjs
}

print.undoc <-
function(x, ...)
{
    for(i in which(sapply(x, length) > 0)) {
        writeLines(paste("Undocumented", names(x)[i], "objects:"))
        print(x[[i]])
    }
    invisible(x)
}

### * codoc

codoc <-
function(package, dir, lib.loc = NULL,
         use.values = FALSE, use.positions = TRUE,
         ignore.generic.functions = FALSE,
         verbose = getOption("verbose"))
{
    S3reg <- character(0)
    nsInfo <- NULL
    
    ## Argument handling.
    if(!missing(package)) {
        if(length(package) != 1)
            stop(paste("argument", sQuote("package"),
                       "must be of length 1"))
        dir <- .find.package(package, lib.loc)
        ## Using package installed in @code{dir} ...
        codeDir <- file.path(dir, "R")
        if(!.fileTest("-d", codeDir))
            stop(paste("directory", sQuote(dir),
                       "does not contain R code"))
        docsDir <- file.path(dir, "man")
        if(!.fileTest("-d", docsDir))
            stop(paste("directory", sQuote(dir),
                       "does not contain Rd sources"))
        isBase <- basename(dir) == "base"

        ## Load package into codeEnv.
        if(!isBase)
            .loadPackageQuietly(package, lib.loc)
        codeEnv <-
            as.environment(match(paste("package", package, sep = ":"),
                                 search()))
        ## If there is a NAMESPACE, find non-exported methods declared
        ## in S3methods directives. 
        if(file.exists(file.path(dir, "NAMESPACE"))) {
            S3reg <- sapply(getNamespaceInfo(package, "S3methods"),
                            function(x) x[[3]])
            S3reg <- if(length(S3reg))
                S3reg[! S3reg %in% ls(codeEnv, all.names = TRUE) ]
            else character(0)           # sapply gives list()
        }
    }
    else {
        if(missing(dir))
            stop(paste("you must specify", sQuote("package"),
                       "or", sQuote("dir")))
        ## Using sources from directory @code{dir} ...
        if(!.fileTest("-d", dir))
            stop(paste("directory", sQuote(dir), "does not exist"))
        else
            dir <- .convertFilePathToAbsolute(dir)
        codeDir <- file.path(dir, "R")
        if(!.fileTest("-d", codeDir))
            stop(paste("directory", sQuote(dir),
                       "does not contain R code"))
        docsDir <- file.path(dir, "man")
        if(!.fileTest("-d", docsDir))
            stop(paste("directory", sQuote(dir),
                       "does not contain Rd sources"))
        isBase <- basename(dir) == "base"

        ## Collect code in codeFile.
        codeFile <- tempfile("Rcode")
        on.exit(unlink(codeFile))
        files <- .listFilesWithType(codeDir, "code")
        file.create(codeFile)
        file.append(codeFile, files)

        ## Read code from codeFile into codeEnv.
        codeEnv <- new.env()
        if(verbose)
            cat("Reading code from", sQuote(codeFile), "\n")
        yy <- try(.sourceAssignments(codeFile, env = codeEnv))
        if(inherits(yy, "try-error")) {
            stop("cannot source package code")
        }
        if(file.exists(file.path(dir, "NAMESPACE")))
            nsInfo <- parseNamespaceFile(basename(dir), dirname(dir))
    }

    lsCode <- ls(envir = codeEnv, all.names = TRUE)
    if(length(nsInfo)) {
        ## Look only at exported objects.
        OK <- lsCode[lsCode %in% nsInfo$exports]
        for (p in nsInfo$exportPatterns)
            OK <- c(OK, grep(p, lsCode, value = TRUE))
        lsCode <- unique(OK)
        ## S3reg are unexported but declared S3 methods.
        S3m <- sapply(nsInfo$S3methods,
                      function(x) {
                          if(length(x) > 2) x[3] else paste(x, collapse=".")
                      })
        ## S3m = list() if empty.
        S3reg <- if(length(S3m)) S3m[! S3m %in% lsCode] else character(0)
    }

    ## Find the function objects to work on.
    funs <- lsCode[sapply(lsCode, function(f) {
        f <- get(f, envir = codeEnv)
        is.function(f) && (length(formals(f)) > 0)
    }) == TRUE]
    if(ignore.generic.functions) {
        funs <- funs[sapply(funs, .isS3Generic, codeEnv) == FALSE]
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
    on.exit(unlink(docsFile), add = TRUE)
    files <- .listFilesWithType(docsDir, "docs")
    docsList <- tempfile("Rdocs")
    on.exit(unlink(docsList), add = TRUE)
    writeLines(files, docsList)
    .Script("perl", "extract-usage.pl",
            paste(if(verbose) "--verbose", docsList, docsFile))

    ## Process the usages in the documentation objects, one at a time.
    badDocObjs <- list()
    lsDocs <- character()
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
            badDocObjs[[docObj]] <- badUsagesInFile

        usagesNotInCode <- usages[! usages %in% lsCode]
        usagesNotInCode <- usagesNotInCode[! usagesNotInCode %in% S3reg]
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

    class(badDocObjs) <- "codoc"
    badDocObjs
}

print.codoc <-
function(x, ...)
{
    if(length(x) == 0)
        return(invisible(x))
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

### * checkAssignFuns

checkAssignFuns <-
function(package, dir, lib.loc = NULL)
{
    S3reg <- character(0)
    
    ## Argument handling.
    if(!missing(package)) {
        if(length(package) != 1)
            stop(paste("argument", sQuote("package"),
                       "must be of length 1"))
        dir <- .find.package(package, lib.loc)
        ## Using package installed in @code{dir} ...
        codeDir <- file.path(dir, "R")
        if(!.fileTest("-d", codeDir))
            stop(paste("directory", sQuote(dir),
                       "does not contain R code"))
        isBase <- basename(dir) == "base"

        ## Load package into codeEnv.
        if(!isBase)
            .loadPackageQuietly(package, lib.loc)
        codeEnv <-
            as.environment(match(paste("package", package, sep = ":"),
                                 search()))
        ## If there is a NAMESPACE, we want to test any non-exported
        ## replacement methods declared in S3method directives.
        if(file.exists(file.path(dir, "NAMESPACE"))) {
            S3reg <- sapply(getNamespaceInfo(package, "S3methods"),
                             function(x) x[[3]])
            if(!length(S3reg)) S3reg <- character(0)
            S3reg <- S3reg[! S3reg %in% ls(codeEnv, all.names = TRUE)]
            S3reg <- grep("<-", S3reg, value = TRUE)
            if(length(S3reg) > 0) {
                S3Table <- get(".__S3MethodsTable__.", envir = NULL)
                ## <FIXME>
                ## methods are not in that table if their generic is in the
                ## package.  Let's ignore those for now.
                S3reg <- S3reg[S3reg %in% ls(S3Table, all.names = TRUE)]
                ## </FIXME>
            }
        }
    }
    else {
        if(missing(dir))
            stop(paste("you must specify", sQuote("package"),
                       "or", sQuote("dir")))
        ## Using sources from directory @code{dir} ...
        if(!.fileTest("-d", dir))
            stop(paste("directory", sQuote(dir), "does not exist"))
        else
            dir <- .convertFilePathToAbsolute(dir)
        codeDir <- file.path(dir, "R")
        if(!.fileTest("-d", codeDir))
            stop(paste("directory", sQuote(dir),
                       "does not contain R code"))
        isBase <- basename(dir) == "base"

        ## Collect code into codeFile.
        codeFile <- tempfile("Rcode")
        on.exit(unlink(codeFile))
        files <- .listFilesWithType(codeDir, "code")
        file.create(codeFile)
        file.append(codeFile, files)

        ## Read code from codeFile into codeEnv.
        codeEnv <- new.env()
        yy <- try(.sourceAssignments(codeFile, env = codeEnv))
        if(inherits(yy, "try-error")) {
            stop("cannot source package code")
        }
    }

    lsCode <- ls(envir = codeEnv, all.names = TRUE)

    ## Find the assignment functions in the given package.
    assignFuns <- c(lsCode[grep("<-", lsCode)], S3reg)
    ## Find the assignment functions with last arg not named 'value'.
    badAssignFuns <-
        assignFuns[sapply(assignFuns, function(f) {
            gf <- if(f %in% S3reg) get(f, envir = S3Table)
                  else get(f, envir = codeEnv)
            argNames <- names(formals(gf))
            argNames[length(argNames)] != "value"
        }) == TRUE]

    class(badAssignFuns) <- "checkAssignFuns"
    badAssignFuns
}

print.checkAssignFuns <-
function(x, ...)
{
    if(length(x) > 0) print(unclass(x), ...)
    invisible(x)
}

### * checkDocArgs

checkDocArgs <-
function(package, dir, lib.loc = NULL)
{
    ## Argument handling.
    if(!missing(package)) {
        if(length(package) != 1)
            stop(paste("argument", sQuote("package"),
                       "must be of length 1"))
        dir <- .find.package(package, lib.loc)
        ## Using package installed in @code{dir} ...
    }
    else {
        if(missing(dir))
            stop(paste("you must specify", sQuote("package"),
                       "or", sQuote("dir")))
        ## Using sources from directory @code{dir} ...
        if(!.fileTest("-d", dir))
            stop(paste("directory", sQuote(dir), "does not exist"))
        else
            dir <- .convertFilePathToAbsolute(dir)
    }

    docsDir <- file.path(dir, "man")
    if(!.fileTest("-d", docsDir))
        stop(paste("directory", sQuote(dir),
                   "does not contain Rd sources"))
    isBase <- basename(dir) == "base"

    ## Collect usages into docsFile.
    docsFile <- tempfile("Rdocs")
    on.exit(unlink(docsFile))
    files <- .listFilesWithType(docsDir, "docs")
    docsList <- tempfile("Rdocs")
    on.exit(unlink(docsList), add = TRUE)
    writeLines(files, docsList)
    .Script("perl", "extract-usage.pl",
            paste("--mode=args", docsList, docsFile))

    ## Process the usages in the documentation objects, one at a time.
    badDocObjs <- list()
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
            unlist(strsplit(gsub("# arglist: *", "", argList), " +"))
        argsInUsage <-
            unlist(lapply(lsArgs,
                          function(f)
                          names(formals(get(f, envir = argsEnv)))))

        argsInUsageMissingInArgList <-
            argsInUsage[!argsInUsage %in% argsInArgList]

        if((length(argsInUsageMissingInArgList) > 0)
           || any(duplicated(argsInArgList)))
            badDocObjs[[docObj]] <-
                list(missing = argsInUsageMissingInArgList,
                     duplicated =
                     argsInArgList[duplicated(argsInArgList)])

        ## Clean up argsEnv
        rm(list = lsArgs, envir = argsEnv)
    }

    class(badDocObjs) <- "checkDocArgs"
    badDocObjs
}

print.checkDocArgs <-
function(x, ...)
{
    for(docObj in names(x)) {
        argsInUsageMissingInArgList <- x[[docObj]][["missing"]]
        if(length(argsInUsageMissingInArgList) > 0) {
            writeLines(paste("Undocumented arguments",
                             " in documentation object ",
                             sQuote(docObj), ":", sep = ""))
            print(unique(argsInUsageMissingInArgList))
        }
        duplicatedArgsInArgList <- x[[docObj]][["duplicated"]]
        if(length(duplicatedArgsInArgList) > 0) {
            writeLines(paste("Duplicated \\argument entries",
                             " in documentation object ",
                             sQuote(docObj), ":", sep = ""))
            print(duplicatedArgsInArgList)
        }
        writeLines("")
    }
    invisible(x)
}

### * checkDocStyle

checkDocStyle <-
function(package, dir, lib.loc = NULL)
{
    ## Argument handling.
    if(!missing(package)) {
        if(length(package) != 1)
            stop(paste("argument", sQuote("package"),
                       "must be of length 1"))
        dir <- .find.package(package, lib.loc)
        ## Using package installed in 'dir' ...
        codeDir <- file.path(dir, "R")
        if(!.fileTest("-d", codeDir))
            stop(paste("directory", sQuote(dir),
                       "does not contain R code"))
        docsDir <- file.path(dir, "man")
        if(!.fileTest("-d", docsDir))
            stop(paste("directory", sQuote(dir),
                       "does not contain Rd sources"))
        isBase <- basename(dir) == "base"

        ## Load package into codeEnv.
        if(!isBase)
            .loadPackageQuietly(package, lib.loc)
        codeEnv <-
            as.environment(match(paste("package", package, sep = ":"),
                                 search()))
    }
    else {
        if(missing(dir))
            stop(paste("you must specify", sQuote("package"),
                       "or", sQuote("dir")))
        ## Using sources from directory @code{dir} ...
        if(!.fileTest("-d", dir))
            stop(paste("directory", sQuote(dir), "does not exist"))
        else
            dir <- .convertFilePathToAbsolute(dir)
        codeDir <- file.path(dir, "R")
        if(!.fileTest("-d", codeDir))
            stop(paste("directory", sQuote(dir),
                       "does not contain R code"))
        docsDir <- file.path(dir, "man")
        if(!.fileTest("-d", docsDir))
            stop(paste("directory", sQuote(dir),
                       "does not contain Rd sources"))
        isBase <- basename(dir) == "base"

        ## Collect code into codeFile.
        codeFile <- tempfile("Rcode")
        on.exit(unlink(codeFile))
        files <- .listFilesWithType(codeDir, "code")
        file.create(codeFile)
        file.append(codeFile, files)

        ## Read code from codeFile into codeEnv.
        codeEnv <- new.env()
        yy <- try(.sourceAssignments(codeFile, env = codeEnv))
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
    allGenerics <- character()
    envList <- list(codeEnv)
    if(!isBase) envList <- c(envList, list(as.environment(NULL)))
    for(env in envList) {
        allObjs <- ls(envir = env, all.names = TRUE)
        allGenerics <-
            c(allGenerics,
              allObjs[sapply(allObjs, .isS3Generic, env) == TRUE])
    }

    ## Find all methods in the given package for the generic functions
    ## determined above.  Store as a list indexed by the names of the
    ## generic functions.
    methodsStopList <- .makeS3MethodsStopList(basename(dir))
    methodsInPackage <- sapply(allGenerics, function(g) {
        name <- paste("^", g, ".", sep = "")
        methods <- grep(gsub("([.[])", "\\\\\\1", name),
                        funs, value = TRUE)
        methods <- methods[! methods %in% methodsStopList]
        methods
    })
    allMethodsInPackage <- unlist(methodsInPackage)

    ## Collect usages into docsFile.
    docsFile <- tempfile("Rdocs")
    on.exit(unlink(docsFile), add = TRUE)
    files <- .listFilesWithType(docsDir, "docs")
    docsList <- tempfile("Rdocs")
    on.exit(unlink(docsList), add = TRUE)
    writeLines(files, docsList)
    .Script("perl", "extract-usage.pl",
            paste("--mode=style", docsList, docsFile))

    ## Process the usages in the documentation objects, one at a time.
    badDocObjs <- list()
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
        ## by calling extract-usage in mode @code{style}.  We keep this
        ## in usageTxt for later, but of course need to replace it by
        ## the GENERIC.CLASS S3 function names for parsing.
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

        if((length(methodsWithGeneric) > 0) ||
           (length(methodsWithFullName > 0)))
            badDocObjs[[docObj]] <-
                list(withGeneric  = methodsWithGeneric,
                     withFullName = methodsWithFullName)

        rm(list = usages, envir = docsEnv)
    }

    class(badDocObjs) <- "checkDocStyle"
    badDocObjs
}

print.checkDocStyle <-
function(x, ...) {
    for(docObj in names(x)) {
        writeLines(paste("Usages in documentation object ",
                         sQuote(docObj), ":", sep = ""))
        methodsWithGeneric <- x[[docObj]][["withGeneric"]]
        if(length(methodsWithGeneric) > 0) {
            writeLines("Methods shown alongside generic:")
            for(g in names(methodsWithGeneric)) {
                methods <- paste(methodsWithGeneric[[g]],
                                 collapse = " ")
                writeLines(strwrap(paste(g, ": ", methods, sep = ""),
                                   indent = 2, exdent = 4))
            }
        }
        methodsWithFullName <- x[[docObj]][["withFullName"]]
        if(length(methodsWithFullName > 0)) {
            writeLines("Methods shown with their own name:")
            writeLines(strwrap(paste(names(methodsWithFullName)),
                               indent = 2, exdent = 4))
        }
        writeLines("")
    }
    invisible(x)
}

### * checkFF

checkFF <-
function(package, dir, file, lib.loc = NULL,
         verbose = getOption("verbose"))
{
    useSaveImage <- FALSE

    ## Argument handling.
    if(!missing(package)) {
        if(length(package) != 1)
            stop(paste("argument", sQuote("package"),
                       "must be of length 1"))
        packageDir <- .find.package(package, lib.loc)
        file <- file.path(packageDir, "R", "all.rda")
        if(file.exists(file))
            useSaveImage <- TRUE
        else
            file <- file.path(packageDir, "R", package)
    }
    else if(!missing(dir)) {
        if(!.fileTest("-d", dir))
            stop(paste("directory", sQuote(dir), "does not exist"))
        else
            dir <- .convertFilePathToAbsolute(dir)
        codeDir <- file.path(dir, "R")
        if(!.fileTest("-d", codeDir))
            stop(paste("directory", sQuote(dir),
                       "does not contain R code"))
        codeFiles <- .listFilesWithType(codeDir, "code")
        file <- tempfile()
        on.exit(unlink(file))
        file.create(file)
        file.append(file, codeFiles)
    }
    else if(missing(file)) {
        stop(paste("you must specify ", sQuote("package"), ", ",
                   sQuote("dir"), " or ", sQuote("file"), sep = ""))
    }

    if(!.fileTest("-f", file))
        stop(paste("file", sQuote(file), "does not exist"))

    ## <FIXME>
    ## Should there really be a 'verbose' argument?
    ## It may be useful to extract all foreign function calls but then
    ## we would want the calls back ...
    ## What we currently do is the following: if 'verbose' is true, we
    ## show all foreign function calls in abbreviated form with the line
    ## ending in either 'OK' or 'MISSING', and we return the list of
    ## 'bad' FF calls (i.e., where the 'PACKAGE' argument is missing)
    ## *invisibly* (so that output is not duplicated).
    ## Otherwise, if not verbose, we return the list of bad FF calls.
    ## </FIXME>

    badExprs <- list()
    FFfuns <- c(".C", ".Fortran", ".Call", ".External",
                ".Call.graphics", ".External.graphics")
    findBadExprs <- function(e) {
        if(is.call(e) || is.expression(e)) {
            ## <NOTE>
            ## This picks up all calls, e.g. a$b, and they may convert
            ## to a vector.  The function is the first element in all
            ## the calls we are interested in.
            ## BDR 2002-11-28
            ## </NOTE>
            if(as.character(e[[1]])[1] %in% FFfuns) {
                parg <- if(is.null(e[["PACKAGE"]])) {
                    badExprs <<- c(badExprs, e)
                    "MISSING"
                }
                else
                    "OK"
                if(verbose) {
                    cat(e[[1]], "(", deparse(e[[2]]), ", ...): ", parg,
                        "\n", sep = "")
                }
            }
            for(i in seq(along = e)) Recall(e[[i]])
        }
    }

    exprs <- if(useSaveImage) {
        if(verbose) writeLines("loading saved image ...")
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
    for(i in seq(along = exprs)) findBadExprs(exprs[[i]])
    class(badExprs) <- "checkFF"
    if(verbose)
        invisible(badExprs)
    else
        badExprs
}

print.checkFF <-
function(x, ...)
{
    if(length(x) > 0) {
        writeLines(paste("Foreign function calls without",
                         sQuote("PACKAGE"), "argument:"))
        for(i in seq(along = x)) {
            writeLines(paste(deparse(x[[i]][[1]]),
                             "(",
                             deparse(x[[i]][[2]]),
                             ", ...)",
                             sep = ""))
        }
    }
    invisible(x)
}

### * checkMethods

checkMethods <-
function(package, dir, lib.loc = NULL)
{
    S3reg <- character(0)
    
    ## Argument handling.
    if(!missing(package)) {
        if(length(package) != 1)
            stop(paste("argument", sQuote("package"),
                       "must be of length 1"))
        dir <- .find.package(package, lib.loc)
        ## Using package installed in @code{dir} ...
        codeDir <- file.path(dir, "R")
        if(!.fileTest("-d", codeDir))
            stop(paste("directory", sQuote(dir),
                       "does not contain R code"))
        isBase <- basename(dir) == "base"

        ## Load package into codeEnv.
        if(!isBase)
            .loadPackageQuietly(package, lib.loc)
        codeEnv <-
            as.environment(match(paste("package", package, sep = ":"),
                                 search()))
        ## If there is a NAMESPACE, we want to test any non-exported
        ## methods declared in S3method directives.
        if(file.exists(file.path(dir, "NAMESPACE"))) {
            S3reg <- sapply(getNamespaceInfo(package, "S3methods"),
                            function(x) x[[3]])
            if(!length(S3reg)) S3reg <- character(0)
            S3reg <- S3reg[! S3reg %in% ls(codeEnv, all.names = TRUE)]
        }
    }
    else {
        if(missing(dir))
            stop(paste("you must specify", sQuote("package"),
                       "or", sQuote("dir")))
        ## Using sources from directory @code{dir} ...
        if(!.fileTest("-d", dir))
            stop(paste("directory", sQuote(dir), "does not exist"))
        else
            dir <- .convertFilePathToAbsolute(dir)
        codeDir <- file.path(dir, "R")
        if(!.fileTest("-d", codeDir))
            stop(paste("directory", sQuote(dir),
                       "does not contain R code"))
        isBase <- basename(dir) == "base"

        ## Collect code into codeFile.
        codeFile <- tempfile("Rcode")
        on.exit(unlink(codeFile))
        files <- .listFilesWithType(codeDir, "code")
        file.create(codeFile)
        file.append(codeFile, files)

        ## Read code from codeFile into codeEnv.
        codeEnv <- new.env()
        yy <- try(.sourceAssignments(codeFile, env = codeEnv))
        if(inherits(yy, "try-error")) {
            stop("cannot source package code")
        }
    }

    lsCode <- ls(envir = codeEnv, all.names = TRUE)

    ## Find the function objects in the given package.
    funs <-
        lsCode[sapply(lsCode, function(f)
                      is.function(get(f, envir = codeEnv))) == TRUE]
    funs <- c(funs, S3reg)

    methodsStopList <- .makeS3MethodsStopList(basename(dir))

    checkArgs <- function(g, m, env) {
        ## Do the arguments of method m (in codeEnv) 'extend' those of
        ## the generic g from environment env?  The method must have all
        ## arguments the generic has, with positional arguments of g in
        ## the same positions for m.
        ## Exception: '...' in the method swallows anything.
        genfun <- get(g, envir = env)
        gArgs <- names(formals(genfun))
        if(g == "plot") gArgs <- gArgs[-2]
        ogArgs <- gArgs
        gm <- if(m %in% S3reg) {
            ## See registerS3method() in namespaces.R.
            defenv <- if (typeof(genfun) == "closure") environment(genfun)
            else .BaseNamespaceEnv
            S3Table <- get(".__S3MethodsTable__.", envir = defenv)
            if(!exists(m, envir = S3Table)) {
                warning("declared S3 method `", m, "' not found",
                        call. = FALSE)
                return(NULL)
            } else get(m, envir = S3Table)
        } else get(m, envir = codeEnv)
        mArgs <- omArgs <- names(formals(gm))
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
        PosMatchOK <- identical(gArgs[ipos], mArgs[ipos])
        ArgMatchOK <- all(gArgs %in% mArgs) || length(dotsPos) > 0
        if(PosMatchOK && ArgMatchOK)
            NULL
        else {
            l <- list(ogArgs, omArgs)
            names(l) <- c(g, m)
            list(l)
        }
    }

    ## Now determine the 'bad' methods in the function objects of the
    ## package.
    badMethods <- list()
    envList <- list(codeEnv)
    if(!isBase) envList <- c(envList, list(as.environment(NULL)))
    for(env in envList) {
        allObjs <- ls(envir = env, all.names = TRUE)
        genFuns <- allObjs[sapply(allObjs, .isS3Generic, env) == TRUE]

        for(g in genFuns) {
            ## Find all methods in funs for generic g.  Taken from the
            ## current code for methods().
            name <- paste("^", g, ".", sep = "")
            methods <- grep(gsub("([.[])", "\\\\\\1", name),
                            funs, value = TRUE)
            methods <- methods[! methods %in% methodsStopList]
            for(m in methods)
                badMethods <- c(badMethods, checkArgs(g, m, env))
        }
    }

    class(badMethods) <- "checkMethods"
    badMethods
}

print.checkMethods <-
function(x, ...)
{
    formatArgs <- function(s)
        paste("function(", paste(s, collapse = ", "), ")", sep = "")
    for(entry in x) {
        writeLines(c(paste(names(entry)[1], ":", sep = ""),
                     strwrap(formatArgs(entry[[1]]),
                             indent = 2, exdent = 11),
                     paste(names(entry)[2], ":", sep = ""),
                     strwrap(formatArgs(entry[[2]]),
                             indent = 2, exdent = 11),
                     ""))
    }
    invisible(x)
}

### * checkTnF

checkTnF <-
function(package, dir, file, lib.loc = NULL)
{
    codeFiles <- docsFiles <- character(0)

    ## Argument handling.
    if(!missing(package)) {
        if(length(package) != 1)
            stop(paste("argument", sQuote("package"),
                       "must be of length 1"))
        packageDir <- .find.package(package, lib.loc)
        if(file.exists(file.path(packageDir, "R", "all.rda"))) {
            warning("cannot check R code installed as image")
        }
        codeFile <- file.path(packageDir, "R", package)
        if(file.exists(codeFile))       # could be data-only
            codeFiles <- codeFile
        exampleDir <- file.path(packageDir, "R-ex")
        if(.fileTest("-d", exampleDir)) {
            codeFiles <- c(codeFiles,
                           .listFilesWithExts(exampleDir, "R"))
        }
    }
    else if(!missing(dir)) {
        if(!.fileTest("-d", dir))
            stop(paste("directory", sQuote(dir), "does not exist"))
        else
            dir <- .convertFilePathToAbsolute(dir)
        codeDir <- file.path(dir, "R")
        if(.fileTest("-d", codeDir))    # could be data-only
            codeFiles <- .listFilesWithType(codeDir, "code")
        docsDir <- file.path(dir, "man")
        if(.fileTest("-d", docsDir))
            docsFiles <- .listFilesWithType(docsDir, "docs")
    }
    else if(!missing(file)) {
        if(!.fileTest("-f", file))
            stop(paste("file", sQuote(file), "does not exist"))
        else
            codeFiles <- file
    }
    else
        stop(paste("you must specify ", sQuote("package"), ", ",
                   sQuote("dir"), " or ", sQuote("file"), sep = ""))

    findTnFInFile <- function(file) {
        matches <- list()
        TnF <- c("T", "F")
        findBadExprs <- function(e, p) {
            if(is.name(e)
               && (as.character(e) %in% TnF)
               && !is.null(p)) {
                ## Need the 'list()' to deal with T/F in function
                ## arglists which are pairlists ...
                matches <<- c(matches, list(p))
            }
            else if(is.recursive(e)) {
                for(i in seq(along = e)) Recall(e[[i]], e)
            }
        }
        exprs <- parse(file = file, n = -1)
        for(i in seq(along = exprs))
            findBadExprs(exprs[[i]], NULL)
        matches
    }

    badExprs <- list()
    for(file in codeFiles) {
        exprs <- findTnFInFile(file)
        if(length(exprs) > 0) {
            exprs <- list(exprs)
            names(exprs) <- file
            badExprs <- c(badExprs, exprs)
        }
    }
    for(file in docsFiles) {
        exampleFile <- tempfile()
        .Script("perl", "extract-examples.pl", paste(file, exampleFile))
        if(file.exists(exampleFile)) {
            exprs <- findTnFInFile(exampleFile)
            if(length(exprs) > 0) {
                exprs <- list(exprs)
                names(exprs) <- file
                badExprs <- c(badExprs, exprs)
            }
            unlink(exampleFile)
        }
    }
    class(badExprs) <- "checkTnF"
    badExprs
}

print.checkTnF <-
function(x, ...)
{
    for(fname in names(x)) {
        writeLines(paste("File ", sQuote(fname), ":", sep = ""))
        xfname <- x[[fname]]
        for(i in seq(along = xfname)) {
            writeLines(strwrap(paste("found T/F in",
                                     paste(deparse(xfname[[i]]),
                                           collapse = "")),
                               exdent = 4))
        }
        writeLines("")
    }
    invisible(x)
}

### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "### [*]+" ***
### End: ***
