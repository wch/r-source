### * undoc

undoc <-
function(package, dir, lib.loc = NULL)
{
    ## Argument handling.
    if(!missing(package)) {
        if(length(package) != 1)
            stop(paste("argument", sQuote("package"),
                       "must be of length 1"))
        dir <- .find.package(package, lib.loc)
        ## Using package installed in @code{dir} ...
        helpIndex <- file.path(dir, "help", "AnIndex")
        if(!fileTest("-f", helpIndex))
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
            as.environment(paste("package", package, sep = ":"))

        codeObjs <- ls(envir = codeEnv, all.names = TRUE)
    }
    else {
        if(missing(dir))
            stop(paste("you must specify", sQuote("package"),
                       "or", sQuote("dir")))
        ## Using sources from directory @code{dir} ...
        if(!fileTest("-d", dir))
            stop(paste("directory", sQuote(dir), "does not exist"))
        else
            dir <- filePathAsAbsolute(dir)
        docsDir <- file.path(dir, "man")
        if(!fileTest("-d", docsDir))
            stop(paste("directory", sQuote(dir),
                       "does not contain Rd sources"))
        isBase <- basename(dir) == "base"

        ## Find all documented topics from the Rd sources.
        aliases <- character(0)
        for(f in listFilesWithType(docsDir, "docs")) {
            aliases <- c(aliases,
                         grep("^\\\\alias", readLines(f), value = TRUE))
        }
        allDocTopics <- gsub("\\\\alias{(.*)}.*", "\\1", aliases)
        allDocTopics <- gsub("\\\\%", "%", allDocTopics)
        allDocTopics <- gsub(" ", "", allDocTopics)
        allDocTopics <- sort(unique(allDocTopics))

        codeEnv <- new.env()
        codeDir <- file.path(dir, "R")
        if(fileTest("-d", codeDir)) {
            ## Collect code in codeFile.
            codeFile <- tempfile("Rcode")
            on.exit(unlink(codeFile))
            file.create(codeFile)
            file.append(codeFile, listFilesWithType(codeDir, "code"))
            ## Read code from codeFile into codeEnv.
            yy <- try(.sourceAssignments(codeFile, env = codeEnv))
            if(inherits(yy, "try-error")) {
                stop("cannot source package code")
            }
        }

        codeObjs <- ls(envir = codeEnv, all.names = TRUE)

        ## Does the package have a NAMESPACE file?  Note that when
        ## working on the sources we (currently?) cannot deal with the
        ## (experimental) alternative way of specifying the namespace.
        if(file.exists(file.path(dir, "NAMESPACE"))) {
            nsInfo <- parseNamespaceFile(basename(dir), dirname(dir))
            ## Look only at exported objects (and not declared S3
            ## methods).
            OK <- codeObjs[codeObjs %in% nsInfo$exports]
            for(p in nsInfo$exportPatterns)
                OK <- c(OK, grep(p, codeObjs, value = TRUE))
            codeObjs <- unique(OK)
        }
    }

    dataObjs <- character(0)
    dataDir <- file.path(dir, "data")
    if(fileTest("-d", dataDir)) {
        files <- listFilesWithType(dataDir, "data")
        files <- files[!duplicated(filePathSansExt(files))]
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
            dataObjs <- c(dataObjs, basename(filePathSansExt(files)))
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

    undocObjs <-
        list("code objects" = codeObjs[! codeObjs %in% allDocTopics],
             "data sets" = dataObjs[! dataObjs %in% allDocTopics])

    if(!is.na(match("package:methods", search()))) {
        ## Undocumented S4 classes?
        S4classes <- getClasses(codeEnv)
        undocObjs <-
            c(undocObjs,
              list("S4 classes" =
                   S4classes[!sapply(S4classes,
                                     function(u) topicName("class", u))
                             %in% allDocTopics]))
    }

    if(!is.na(match("package:methods", search()))
       && basename(dir) == "methods") {
        ## Undocumented S4 methods?
        ## Only do this for package methods itself, to get us started.
        ## Courtesy JMC for advice on finding all S4 methods ...
        S4methods <-
            sapply(getGenerics(codeEnv),
                   function(f) {
                       meths <-
                           linearizeMlist(getMethods(f, codeEnv))
                       sigs <-
                           sapply(meths@classes, paste, collapse = ",") 
                       if(length(sigs))
                           paste(f, ",", sigs, sep = "")
                       else
                           character()
                   })
        S4methods <- unlist(S4methods, use.names = FALSE)
        S4methods <-
            S4methods[!sapply(S4methods,
                              function(u) topicName("method", u))
                      %in% allDocTopics]
        undocObjs <-
            c(undocObjs, list("S4 methods" =
                              sub("([^,]*),(.*)",
                                  "\\\\S4method{\\1}{\\2}",
                                  S4methods)))
    }
                             
    class(undocObjs) <- "undoc"
    undocObjs
}

print.undoc <-
function(x, ...)
{
    for(i in which(sapply(x, length) > 0)) {
        writeLines(paste("Undocumented ", names(x)[i], ":", sep = ""))
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
    hasNamespace <- FALSE
    ## If a package has a namespace, we need to determine the S3 methods
    ## which are registered but not exported: these *may* have explicit
    ## usage documentation (e.g., if they have 'surprising arguments'),
    ## and hence not be included in the information about objects with
    ## usage but 'missing from the code'.
    S3reg <- character(0)

    ## Argument handling.
    if(!missing(package)) {
        if(length(package) != 1)
            stop(paste("argument", sQuote("package"),
                       "must be of length 1"))
        dir <- .find.package(package, lib.loc)
        ## Using package installed in @code{dir} ...
        codeDir <- file.path(dir, "R")
        if(!fileTest("-d", codeDir))
            stop(paste("directory", sQuote(dir),
                       "does not contain R code"))
        docsDir <- file.path(dir, "man")
        if(!fileTest("-d", docsDir))
            stop(paste("directory", sQuote(dir),
                       "does not contain Rd sources"))
        isBase <- basename(dir) == "base"

        ## Load package into codeEnv.
        if(!isBase)
            .loadPackageQuietly(package, lib.loc)
        codeEnv <-
            as.environment(paste("package", package, sep = ":"))

        lsCode <- ls(envir = codeEnv, all.names = TRUE)

        ## Does the package have a namespace?
        if(packageHasNamespace(package, dirname(dir))) {
            hasNamespace <- TRUE
            ## Determine unexported but declared S3 methods.
            nsS3methodsList <- getNamespaceInfo(package, "S3methods")
            S3reg <- as.character(sapply(nsS3methodsList, "[[", 3))
            S3reg <- S3reg[! S3reg %in% lsCode]
        }
    }
    else {
        if(missing(dir))
            stop(paste("you must specify", sQuote("package"),
                       "or", sQuote("dir")))
        ## Using sources from directory @code{dir} ...
        if(!fileTest("-d", dir))
            stop(paste("directory", sQuote(dir), "does not exist"))
        else
            dir <- filePathAsAbsolute(dir)
        codeDir <- file.path(dir, "R")
        if(!fileTest("-d", codeDir))
            stop(paste("directory", sQuote(dir),
                       "does not contain R code"))
        docsDir <- file.path(dir, "man")
        if(!fileTest("-d", docsDir))
            stop(paste("directory", sQuote(dir),
                       "does not contain Rd sources"))
        isBase <- basename(dir) == "base"

        ## Collect code in codeFile.
        codeFile <- tempfile("Rcode")
        on.exit(unlink(codeFile))
        file.create(codeFile)
        file.append(codeFile, listFilesWithType(codeDir, "code"))

        ## Read code from codeFile into codeEnv.
        codeEnv <- new.env()
        if(verbose)
            cat("Reading code from", sQuote(codeFile), "\n")
        yy <- try(.sourceAssignments(codeFile, env = codeEnv))
        if(inherits(yy, "try-error")) {
            stop("cannot source package code")
        }

        lsCode <- ls(envir = codeEnv, all.names = TRUE)

        ## Does the package have a NAMESPACE file?  Note that when
        ## working on the sources we (currently?) cannot deal with the
        ## (experimental) alternative way of specifying the namespace.
        if(file.exists(file.path(dir, "NAMESPACE"))) {
            hasNamespace <- TRUE
            nsInfo <- parseNamespaceFile(basename(dir), dirname(dir))
            ## Look only at exported objects.
            OK <- lsCode[lsCode %in% nsInfo$exports]
            for(p in nsInfo$exportPatterns)
                OK <- c(OK, grep(p, lsCode, value = TRUE))
            lsCode <- unique(OK)
            ## Determine unexported but declared S3 methods.
            nsS3methodsList <- .getNamespaceS3methodsList(nsInfo)
            S3reg <- as.character(sapply(nsS3methodsList, "[[", 3))
            S3reg <- S3reg[! S3reg %in% lsCode]
        }
    }

    ## Find the function objects to work on.
    funs <- lsCode[sapply(lsCode, function(f) {
        f <- get(f, envir = codeEnv)
        is.function(f) && (length(formals(f)) > 0)
    }) == TRUE]
    if(ignore.generic.functions) {
        ## ignore all generics, whatever name they dispatch on
        funs <- funs[sapply(funs, .isS3Generic, codeEnv, FALSE) == FALSE]
    }
    ## <FIXME>
    ## Sourcing all R code files in the package is a problem for base,
    ## where this misses the .Primitive functions.  Hence, when checking
    ## base for objects shown in \usage but missing from the code, we
    ## get the primitive functions from the version of R we are using.
    ## Maybe one day we will have R code for the primitives as well ...
    if(isBase) {
        baseObjs <- ls(envir = as.environment(NULL), all.names = TRUE)
        lsCode <-
            c(lsCode, baseObjs[sapply(baseObjs, .isPrimitive, NULL)],
              c(".First.lib", ".Last.lib", ".Random.seed"))
    }
    ## </FIXME>

    useS4methods <- !is.na(match("package:methods", search()))
    S4methods <- character()
    S4methodsEnv <- new.env()
    if(useS4methods) {
        lapply(getGenerics(codeEnv),
               function(f) {
                   meths <-
                       linearizeMlist(getMethods(f, codeEnv))
                   sigs <-
                       sapply(meths@classes, paste, collapse = ",")
                   if(!length(sigs)) return()
                   names <- paste("\\S4method{", f, "}{", sigs, "}",
                                  sep = "")
                   for(i in seq(along = sigs))
                       assign(names[i], meths@methods[[i]],
                              envir = S4methodsEnv)
                   S4methods <- c(S4methods, names)
               })
    }
                   
    docsEnv <- new.env()
    checkCoDoc <- function(f, envir = codeEnv) {
        ffc <- formals(get(f, envir = envir))
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
    docsList <- tempfile("Rdocs")
    on.exit(unlink(docsList), add = TRUE)
    writeLines(listFilesWithType(docsDir, "docs"), docsList)
    .Script("perl", "extract-usage.pl",
            paste(if(verbose) "--verbose", docsList, docsFile))

    ## Process the usages in the documentation objects, one at a time.
    badDocObjs <- list()
    lsDocs <- character()
    usagesNotInCode <- list()
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
            stop(paste("cannot parse usages in documentation object",
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
        if(useS4methods) {
            for(f in usages[usages %in% S4methods])
                badUsagesInFile <-
                    c(badUsagesInFile,
                      checkCoDoc(f, envir = S4methodsEnv))
        }
        if(length(badUsagesInFile) > 0)
            badDocObjs[[docObj]] <- badUsagesInFile

        ## Determine functions with a \usage entry in the documentation
        ## but 'missing from the code'.  Entries for S3 methods which
        ## are registered but not exported are ok (as these methods
        ## might have 'surprising' arguments).
        ## <NOTE>
        ## Older versions only printed this information without
        ## returning it.  We now aggregate it into usagesNotInCode and
        ## add this as an attribute to the badDocObjs object returned.
        ## It might be nicer to do this differently ...
        ## </NOTE>
        badUsagesInFile <-
            usages[! usages %in% c(lsCode, S3reg, ".__Usage__.")]
        if(length(badUsagesInFile) > 0)
            usagesNotInCode[[docObj]] <- badUsagesInFile

        lsDocs <- c(lsDocs, usages)
        rm(list = usages, envir = docsEnv)

    }

    ## Determine (function) objects in the code without a \usage entry.
    ## Of course, these could still be 'documented' via \alias.
    ## </NOTE>
    ## Older versions only printed this information without returning it
    ## (in case 'verbose' was true).  We now add this as an attribute to
    ## the badDocObjs returned.
    ## </NOTE>
    codeNotInUsages <- lsCode[! lsCode %in% lsDocs]
    funsNotInUsages <- funs[funs %in% codeNotInUsages]
    ## (Note that 'funs' does not necessarily contain all (exported)
    ## functions in the package.)

    attr(badDocObjs, "codeNotInUsages") <- codeNotInUsages
    attr(badDocObjs, "usagesNotInCode") <- usagesNotInCode
    attr(badDocObjs, "funsNotInUsages") <- funsNotInUsages
    attr(badDocObjs, "hasNamespace") <- hasNamespace

    class(badDocObjs) <- "codoc"
    badDocObjs
}

print.codoc <-
function(x, ...)
{
    ## In general, functions in the code which only have an \alias but
    ## no \usage entry are not necessarily a problem---they might be
    ## mentioned in other parts of the Rd object documenting them, or be
    ## 'internal'.  However, if a package has a namespace (and this was
    ## used in the codoc() computations), then clearly all *exported*
    ## functions should have \usage entries.
    ## <FIXME>
    ## Things are not quite that simple.
    ## E.g., for generic functions with just a default and a formula
    ## method we typically do not have \usage for the generic itself.
    ## Also, extract-usage.pl currently only provides code for functions
    ## (names of variables and data sets are available via comments) so
    ## variables will come out as 'without usage information' ...
    ## As we can always access the information via
    ##    attr(codoc("foo"), "codeNotInUsages")
    ## disable reporting this for the time being ...
    ## <COMMENT>
    ##     codeNotInUsages <- attr(x, "codeNotInUsages")
    ##     if(length(codeNotInUsages)
    ##        && identical(TRUE, attr(x, "hasNamespace"))) {
    ##         if(length(codeNotInUsages)) {
    ##             writeLines("Exported objects without usage information:")
    ##             print(codeNotInUsages)
    ##             writeLines("")
    ##         }
    ##     }
    ## </COMMENT>
    ## </FIXME>

    usagesNotInCode <- attr(x, "usagesNotInCode")
    if(length(usagesNotInCode) > 0) {
        for(fname in names(usagesNotInCode)) {
            writeLines(paste("Functions/methods with usage in",
                             "documentation object", sQuote(fname),
                             "but not in code:"))
            print(unique(usagesNotInCode[[fname]]))
            writeLines("")
        }
    }

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
        if(!fileTest("-d", dir))
            stop(paste("directory", sQuote(dir), "does not exist"))
        else
            dir <- filePathAsAbsolute(dir)
    }

    docsDir <- file.path(dir, "man")
    if(!fileTest("-d", docsDir))
        stop(paste("directory", sQuote(dir),
                   "does not contain Rd sources"))
    isBase <- basename(dir) == "base"

    ## Collect usages into docsFile.
    docsFile <- tempfile("Rdocs")
    on.exit(unlink(docsFile))
    docsList <- tempfile("Rdocs")
    on.exit(unlink(docsList), add = TRUE)
    writeLines(listFilesWithType(docsDir, "docs"), docsList)
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
            stop(paste("cannot parse usages in documentation object",
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
                          function(f) {
                              f <- get(f, envir = argsEnv)
                              if(is.function(f))
                                  names(formals(f))
                              else
                                  character()
                          }))

        argsInUsageMissingInArgList <-
            argsInUsage[!argsInUsage %in% argsInArgList]
        argsInArgListMissingInUsage <-
            argsInArgList[!argsInArgList %in% argsInUsage]
        if(length(argsInArgListMissingInUsage) > 0) {
            usageText <- get(".__Usage__.", envir = argsEnv)
            ## In the case of 'over-documented' arguments, try to be
            ## defensive and reduce to arguments that do not match the
            ## \usage text (modulo word boundaries).
            bad <- sapply(argsInArgListMissingInUsage,
                          function(x)
                          regexpr(paste("\\b", x, "\\b", sep = ""),
                                  usageText) == -1)
            argsInArgListMissingInUsage <-
                argsInArgListMissingInUsage[bad]
            ## Note that the fact that we can parse the raw \usage does
            ## not imply that over-documented arguments are a problem:
            ## this works for Rd files documenting e.g. shell utilities
            ## but fails for files with special syntax (Extract.Rd).
        }

        if((length(argsInUsageMissingInArgList) > 0)
           || any(duplicated(argsInArgList))
           || (length(argsInArgListMissingInUsage) > 0))
            badDocObjs[[docObj]] <-
                list(missing = argsInUsageMissingInArgList,
                     duplicated =
                     argsInArgList[duplicated(argsInArgList)],
                     overdoc = argsInArgListMissingInUsage)

        ## Clean up argsEnv.
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
        argsInArgListMissingInUsage <- x[[docObj]][["overdoc"]]
        if(length(argsInArgListMissingInUsage) > 0) {
            writeLines(paste("Documented arguments not in \\usage",
                             " in documentation object ",
                             sQuote(docObj), ":", sep = ""))
            print(unique(argsInArgListMissingInUsage))
        }
        writeLines("")
    }
    invisible(x)
}

### * checkDocStyle

checkDocStyle <-
function(package, dir, lib.loc = NULL)
{
    hasNamespace <- FALSE

    ## Argument handling.
    if(!missing(package)) {
        if(length(package) != 1)
            stop(paste("argument", sQuote("package"),
                       "must be of length 1"))
        dir <- .find.package(package, lib.loc)
        ## Using package installed in 'dir' ...
        codeDir <- file.path(dir, "R")
        if(!fileTest("-d", codeDir))
            stop(paste("directory", sQuote(dir),
                       "does not contain R code"))
        docsDir <- file.path(dir, "man")
        if(!fileTest("-d", docsDir))
            stop(paste("directory", sQuote(dir),
                       "does not contain Rd sources"))
        isBase <- basename(dir) == "base"

        ## Load package into codeEnv.
        if(!isBase)
            .loadPackageQuietly(package, lib.loc)
        codeEnv <-
            as.environment(paste("package", package, sep = ":"))

        lsCode <- ls(envir = codeEnv, all.names = TRUE)

        ## Does the package have a namespace?
        if(packageHasNamespace(package, dirname(dir))) {
            hasNamespace <- TRUE
            ## Determine names of declared S3 methods and associated S3
            ## generics. 
            nsS3methodsList <- getNamespaceInfo(package, "S3methods")
            nsS3generics <-
                as.character(sapply(nsS3methodsList, "[[", 1))
            nsS3methods <-
                as.character(sapply(nsS3methodsList, "[[", 3))                
        }
    }
    else {
        if(missing(dir))
            stop(paste("you must specify", sQuote("package"),
                       "or", sQuote("dir")))
        ## Using sources from directory @code{dir} ...
        if(!fileTest("-d", dir))
            stop(paste("directory", sQuote(dir), "does not exist"))
        else
            dir <- filePathAsAbsolute(dir)
        codeDir <- file.path(dir, "R")
        if(!fileTest("-d", codeDir))
            stop(paste("directory", sQuote(dir),
                       "does not contain R code"))
        docsDir <- file.path(dir, "man")
        if(!fileTest("-d", docsDir))
            stop(paste("directory", sQuote(dir),
                       "does not contain Rd sources"))
        isBase <- basename(dir) == "base"

        ## Collect code into codeFile.
        codeFile <- tempfile("Rcode")
        on.exit(unlink(codeFile))
        file.create(codeFile)
        file.append(codeFile, listFilesWithType(codeDir, "code"))

        ## Read code from codeFile into codeEnv.
        codeEnv <- new.env()
        yy <- try(.sourceAssignments(codeFile, env = codeEnv))
        if(inherits(yy, "try-error")) {
            stop("cannot source package code")
        }

        lsCode <- ls(envir = codeEnv, all.names = TRUE)

        ## Does the package have a NAMESPACE file?  Note that when
        ## working on the sources we (currently?) cannot deal with the
        ## (experimental) alternative way of specifying the namespace.
        if(file.exists(file.path(dir, "NAMESPACE"))) {
            hasNamespace <- TRUE
            nsInfo <- parseNamespaceFile(basename(dir), dirname(dir))
            ## Determine exported objects.
            OK <- lsCode[lsCode %in% nsInfo$exports]
            for(p in nsInfo$exportPatterns)
                OK <- c(OK, grep(p, lsCode, value = TRUE))
            lsCode <- unique(OK)
            ## Determine names of declared S3 methods and associated S3
            ## generics. 
            nsS3methodsList <- .getNamespaceS3methodsList(nsInfo)
            nsS3generics <-
                as.character(sapply(nsS3methodsList, "[[", 1))
            nsS3methods <-
                as.character(sapply(nsS3methodsList, "[[", 3))                
        }
        
    }

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
        ## Find all available S3 generics.
        allObjs <- if(identical(env, codeEnv)) {
            ## We only want the exported ones anyway ...
            funs
        }
        else
            ls(envir = env, all.names = TRUE)
        allGenerics <-
            c(allGenerics,
              allObjs[sapply(allObjs, .isS3Generic, env) == TRUE])
    }
    ## Add internal S3 generics and S3 group generics.
    allGenerics <-
        c(allGenerics,
          .getInternalS3generics(),
          c("Math", "Ops", "Summary"))

    ## Find all methods in the given package for the generic functions
    ## determined above.  Store as a list indexed by the names of the
    ## generic functions.
    methodsStopList <- .makeS3MethodsStopList(basename(dir))
    methodsInPackage <- sapply(allGenerics, function(g) {
        ## <FIXME>
        ## We should really determine the name g dispatches for, see
        ## a current version of methods() [2003-07-07].  (Care is needed
        ## for internal generics and group generics.)
        ## Matching via grep() is tricky with e.g. a '$' in the name of
        ## the generic function ... hence substr().
        name <- paste(g, ".", sep = "")
        methods <- funs[substr(funs, 1, nchar(name)) == name]
        ## </FIXME>
        methods <- methods[! methods %in% methodsStopList]
        if(hasNamespace) {
            ## Find registered methods for generic g.
            methods <- c(methods, nsS3methods[nsS3generics == g])
        }
        methods
    })
    allMethodsInPackage <- unlist(methodsInPackage)

    ## Collect usages into docsFile.
    docsFile <- tempfile("Rdocs")
    on.exit(unlink(docsFile), add = TRUE)
    docsList <- tempfile("Rdocs")
    on.exit(unlink(docsList), add = TRUE)
    writeLines(listFilesWithType(docsDir, "docs"), docsList)
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
        ## <NOTE>
        ## Special \method{GENERIC}{CLASS} Rd markup was preserved by
        ## calling extract-usage in mode @code{style}.  We keep this in
        ## usageTxt for later, but of course need to replace it by the
        ## GENERIC.CLASS S3 function names for parsing.
        ## </NOTE>
        ## <FIXME>
        ## Need to deal with possible S3 method renaming in namespaces.
        ## However, matching \method{} markup against the registration
        ## information seems rather costly: maybe record GENERIC.CLASS
        ## as the function name in methodsInPackage even in the case of
        ## renaming?
        exprs <- try(parse(n = -1,
                           text = gsub(paste("\\\\method",
                                             "{([a-zA-Z0-9.]+)}",
                                             "{([a-zA-Z0-9.]+)}",
                                             sep = ""),
                                       "\\1.\\2", usageTxt)))
        ## </FIXME>
        if(inherits(exprs, "try-error"))
            stop(paste("cannot parse usages in documentation object",
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
                   function(f)
                   any(grep(paste("\"?", f, "\"? *<-", sep = ""),
                            usageTxt)))
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
        ## <NOTE>
        ## With \method{GENERIC}{CLASS} now being transformed to show
        ## both GENERIC and CLASS info, documenting S3 methods on the
        ## same page as their generic is not necessarily a problem any
        ## more (as one can refer to the generic or the methods in the
        ## documentation, in particular for the primary argument).
        ## Hence, even if we still provide information about this, we
        ## no longer print it by default.  One can still access it via
        ##   lapply(checkDocStyle("foo"), "[[", "withGeneric")
        ## (but of course it does not print that nicely anymore),
        ## </NOTE>
        methodsWithFullName <- x[[docObj]][["withFullName"]]
        if(length(methodsWithFullName > 0)) {
            writeLines(paste("S3 methods shown with full name in ",
                             "documentation object ",
                             sQuote(docObj), ":", sep = ""))
            writeLines(strwrap(paste(names(methodsWithFullName),
                                     collapse = " "),
                               indent = 2, exdent = 2))
            writeLines("")
        }
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
        ## Using package installed in @code{dir} ...
        dir <- .find.package(package, lib.loc)
        file <- file.path(dir, "R", "all.rda")
        if(file.exists(file))
            useSaveImage <- TRUE
        else
            file <- file.path(dir, "R", package)
    }
    else if(!missing(dir)) {
        ## Using sources from directory @code{dir} ...
        if(!fileTest("-d", dir))
            stop(paste("directory", sQuote(dir), "does not exist"))
        else
            dir <- filePathAsAbsolute(dir)
        codeDir <- file.path(dir, "R")
        if(!fileTest("-d", codeDir))
            stop(paste("directory", sQuote(dir),
                       "does not contain R code"))
        file <- tempfile()
        on.exit(unlink(file))
        file.create(file)
        file.append(file, listFilesWithType(codeDir, "code"))
    }
    else if(missing(file)) {
        stop(paste("you must specify ", sQuote("package"), ", ",
                   sQuote("dir"), " or ", sQuote("file"), sep = ""))
    }

    if(!fileTest("-f", file))
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

    if(useSaveImage) {
        if(verbose) writeLines("loading saved image ...")
        codeEnv <- new.env()
        load(file, envir = codeEnv)
        exprs <- lapply(ls(envir = codeEnv, all.names = TRUE),
                        function(f) {
                            f <- get(f, envir = codeEnv)
                            if(typeof(f) == "closure")
                                body(f)
                            else
                                NULL
                        })
        if(!is.na(match("package:methods", search()))) {
            ## Also check the code in S4 methods.
            ## This may find things twice if a setMethod() with a bad FF
            ## call is from inside a function (e.g., InitMethods()).
            for(f in getGenerics(codeEnv)) {
                ## <FIXME>
                ## The get(f) should not be necessary, but without it,
                ## current versions of SparseM fail on "%x%"() ... otoh,
                ## we cannot use get on "%*%"() ...
                if(is(gf <- get(f, codeEnv), "genericFunction"))
                    f <- gf
                meths <- linearizeMlist(getMethods(f, codeEnv))
                ## </FIXME>
                exprs <- c(exprs, lapply(meths@methods, body))
            }
        }
    }
    else {
        exprs <- try(parse(file = file, n = -1))
        if(inherits(exprs, "try-error"))
            stop(paste("parse error in file", sQuote(file)))
    }
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
    hasNamespace <- FALSE
    ## If an installed package has a namespace, we need to record the S3
    ## methods which are registered but not exported (so that we can
    ## get() them from the right place).
    S3reg <- character(0)

    ## Argument handling.
    if(!missing(package)) {
        if(length(package) != 1)
            stop(paste("argument", sQuote("package"),
                       "must be of length 1"))
        dir <- .find.package(package, lib.loc)
        ## Using package installed in @code{dir} ...
        codeDir <- file.path(dir, "R")
        if(!fileTest("-d", codeDir))
            stop(paste("directory", sQuote(dir),
                       "does not contain R code"))
        isBase <- basename(dir) == "base"

        ## Load package into codeEnv.
        if(!isBase)
            .loadPackageQuietly(package, lib.loc)
        codeEnv <-
            as.environment(paste("package", package, sep = ":"))

        lsCode <- ls(envir = codeEnv, all.names = TRUE)

        ## Does the package have a namespace?
        if(packageHasNamespace(package, dirname(dir))) {
            hasNamespace <- TRUE
            ## Determine names of declared S3 methods and associated S3
            ## generics. 
            nsS3methodsList <- getNamespaceInfo(package, "S3methods")
            nsS3generics <-
                as.character(sapply(nsS3methodsList, "[[", 1))
            nsS3methods <-
                as.character(sapply(nsS3methodsList, "[[", 3))                
            ## Determine unexported but declared S3 methods.
            S3reg <- nsS3methods[! nsS3methods %in% lsCode]
        }
    }
    else {
        if(missing(dir))
            stop(paste("you must specify", sQuote("package"),
                       "or", sQuote("dir")))
        ## Using sources from directory @code{dir} ...
        if(!fileTest("-d", dir))
            stop(paste("directory", sQuote(dir), "does not exist"))
        else
            dir <- filePathAsAbsolute(dir)
        codeDir <- file.path(dir, "R")
        if(!fileTest("-d", codeDir))
            stop(paste("directory", sQuote(dir),
                       "does not contain R code"))
        isBase <- basename(dir) == "base"

        ## Collect code into codeFile.
        codeFile <- tempfile("Rcode")
        on.exit(unlink(codeFile))
        file.create(codeFile)
        file.append(codeFile, listFilesWithType(codeDir, "code"))

        ## Read code from codeFile into codeEnv.
        codeEnv <- new.env()
        yy <- try(.sourceAssignments(codeFile, env = codeEnv))
        if(inherits(yy, "try-error")) {
            stop("cannot source package code")
        }

        lsCode <- ls(envir = codeEnv, all.names = TRUE)

        ## Does the package have a NAMESPACE file?  Note that when
        ## working on the sources we (currently?) cannot deal with the
        ## (experimental) alternative way of specifying the namespace.
        if(file.exists(file.path(dir, "NAMESPACE"))) {
            hasNamespace <- TRUE
            nsInfo <- parseNamespaceFile(basename(dir), dirname(dir))
            ## Determine exported objects.
            OK <- lsCode[lsCode %in% nsInfo$exports]
            for(p in nsInfo$exportPatterns)
                OK <- c(OK, grep(p, lsCode, value = TRUE))
            lsCode <- unique(OK)
            ## Determine names of declared S3 methods and associated S3
            ## generics. 
            nsS3methodsList <- .getNamespaceS3methodsList(nsInfo)
            nsS3generics <-
                as.character(sapply(nsS3methodsList, "[[", 1))
            nsS3methods <-
                as.character(sapply(nsS3methodsList, "[[", 3))                
        }
        
    }

    ## Find the function objects in the given package.
    funs <-
        lsCode[sapply(lsCode, function(f)
                      is.function(get(f, envir = codeEnv))) == TRUE]

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
            ## See registerS3method() in namespace.R.
            defenv <- if (typeof(genfun) == "closure") environment(genfun)
            else .BaseNamespaceEnv
            S3Table <- get(".__S3MethodsTable__.", envir = defenv)
            if(!exists(m, envir = S3Table)) {
                warning(paste("declared S3 method", sQuote(m),
                              "not found"),
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
        posMatchOK <- identical(gArgs[ipos], mArgs[ipos])
        argMatchOK <- all(gArgs %in% mArgs) || length(dotsPos) > 0
        if(posMatchOK && argMatchOK)
            NULL
        else {
            l <- list(ogArgs, omArgs)
            names(l) <- c(g, m)
            list(l)
        }
    }

    ## Deal with S3 group methods.  We create a separate environment
    ## with pseudo-definitions for these.
    S3groupGenericsEnv <- new.env()
    assign("Math",
           function(x, ...) UseMethod("Math"),
           envir = S3groupGenericsEnv)
    assign("Ops",
           function(e1, e2) UseMethod("Ops"),
           envir = S3groupGenericsEnv)
    assign("Summary",
           function(x, ...) UseMethod("Summary"),
           envir = S3groupGenericsEnv)

    ## Now determine the 'bad' methods in the function objects of the
    ## package.
    badMethods <- list()
    envList <- list(codeEnv, S3groupGenericsEnv)
    if(!isBase) envList <- c(envList, list(as.environment(NULL)))
    for(env in envList) {
        ## Find all available S3 generics.
        allObjs <- if(identical(env, codeEnv)) {
            ## We only want the exported ones anyway ...
            funs
        }
        else
            ls(envir = env, all.names = TRUE)
        genFuns <- allObjs[sapply(allObjs, .isS3Generic, env) == TRUE]

        ## For base, also add the internal S3 generics which are not
        ## .Primitive (as checkArgs() does not deal with these).
        if(identical(env, as.environment(NULL))) {
            internalS3generics <- .getInternalS3generics()
            internalS3generics <-
                internalS3generics[sapply(internalS3generics,
                                          .isPrimitive, NULL)
                                   == FALSE]
            genFuns <- c(genFuns, internalS3generics)
        }

        for(g in genFuns) {
            ## Find all methods in funs for generic g.
            ## <FIXME>
            ## We should really determine the name g dispatches for, see
            ## a current version of methods() [2003-07-07].  (Care is
            ## needed for internal generics and group generics.)
            ## Matching via grep() is tricky with e.g. a '$' in the name
            ## of the generic function ... hence substr().
            name <- paste(g, ".", sep = "")
            methods <- funs[substr(funs, 1, nchar(name)) == name]
            ## </FIXME>
            methods <- methods[! methods %in% methodsStopList]
            if(hasNamespace) {
                ## Find registered methods for generic g.
                methods <- c(methods, nsS3methods[nsS3generics == g])
            }

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

### * checkReplaceFuns

checkReplaceFuns <-
function(package, dir, lib.loc = NULL)
{
    hasNamespace <- FALSE
    
    ## Argument handling.
    if(!missing(package)) {
        if(length(package) != 1)
            stop(paste("argument", sQuote("package"),
                       "must be of length 1"))
        dir <- .find.package(package, lib.loc)
        ## Using package installed in @code{dir} ...
        codeDir <- file.path(dir, "R")
        if(!fileTest("-d", codeDir))
            stop(paste("directory", sQuote(dir),
                       "does not contain R code"))
        isBase <- basename(dir) == "base"

        ## Load package into codeEnv.
        if(!isBase)
            .loadPackageQuietly(package, lib.loc)
        ## In case the package has a namespace, we really want to check
        ## all replacement functions in the package.  (If not, we need
        ## to change the code for the non-installed case to only look at
        ## exported (replacement) functions.)
        if(packageHasNamespace(package, dirname(dir))) {
            hasNamespace <- TRUE
            codeEnv <- asNamespace(package)
            nsS3methodsList <- getNamespaceInfo(package, "S3methods")
        }
        else
            codeEnv <-
                as.environment(paste("package", package, sep = ":"))
    }

    else {
        if(missing(dir))
            stop(paste("you must specify", sQuote("package"),
                       "or", sQuote("dir")))
        ## Using sources from directory @code{dir} ...
        if(!fileTest("-d", dir))
            stop(paste("directory", sQuote(dir), "does not exist"))
        else
            dir <- filePathAsAbsolute(dir)
        codeDir <- file.path(dir, "R")
        if(!fileTest("-d", codeDir))
            stop(paste("directory", sQuote(dir),
                       "does not contain R code"))
        isBase <- basename(dir) == "base"

        ## Collect code into codeFile.
        codeFile <- tempfile("Rcode")
        on.exit(unlink(codeFile))
        file.create(codeFile)
        file.append(codeFile, listFilesWithType(codeDir, "code"))

        ## Read code from codeFile into codeEnv.
        codeEnv <- new.env()
        yy <- try(.sourceAssignments(codeFile, env = codeEnv))
        if(inherits(yy, "try-error")) {
            stop("cannot source package code")
        }

        ## Does the package have a NAMESPACE file?  Note that when
        ## working on the sources we (currently?) cannot deal with the
        ## (experimental) alternative way of specifying the namespace.
        if(file.exists(file.path(dir, "NAMESPACE"))) {
            hasNamespace <- TRUE
            nsInfo <- parseNamespaceFile(basename(dir), dirname(dir))
            nsS3methodsList <- .getNamespaceS3methodsList(nsInfo)
        }
    }
    
    lsCode <- ls(envir = codeEnv, all.names = TRUE)
    replaceFuns <- character()

    if(hasNamespace) {
        nsS3generics <-
            as.character(sapply(nsS3methodsList, "[[", 1))
        nsS3methods <-
            as.character(sapply(nsS3methodsList, "[[", 3))
        ## S3 replacement methods from namespace registration?
        idx <- grep("<-$", nsS3generics)
        if(any(idx)) replaceFuns <- nsS3methods[idx]
        ## Now remove the functions registered as S3 methods.
        lsCode <- lsCode[! lsCode %in% nsS3methods]
    }

    replaceFuns <- c(replaceFuns, grep("<-", lsCode, value = TRUE))

    .checkLastFormalArg <- function(f) {
        argNames <- names(formals(f))
        if(!length(argNames))
            TRUE                        # most likely a .Primitive()
        else
            identical(argNames[length(argNames)], "value")
    }
    
    ## Find the replacement functions (which have formal arguments) with
    ## last arg not named 'value'.
    badReplaceFuns <-
        replaceFuns[sapply(replaceFuns, function(f) {
            ## Always get the functions from codeEnv ...
            ## Should maybe get S3 methods from the registry ...
            f <- get(f, envir = codeEnv)
            if(!is.function(f)) return(TRUE)
            .checkLastFormalArg(f)
        }) == FALSE]

    if(!is.na(match("package:methods", search()))) {
        S4generics <- getGenerics(codeEnv)
        ## Assume that the ones with names ending in '<-' are always
        ## replacement functions.
        S4generics <- grep("<-$", S4generics, value = TRUE)
        badS4ReplaceMethods <-
            sapply(S4generics,
                   function(f) {
                       meths <- linearizeMlist(getMethods(f, codeEnv))
                       ind <- which(sapply(meths@methods,
                                           .checkLastFormalArg)
                                    == FALSE)
                       if(!length(ind))
                           character()
                       else {
                           sigs <- sapply(meths@classes[ind], paste,
                                          collapse = ",")
                           paste("\\S4method{", f, "}{", sigs, "}",
                                 sep = "")
                       }
                   })
        badReplaceFuns <-
            c(badReplaceFuns,
              unlist(badS4ReplaceMethods, use.names = FALSE))
    }
        

    class(badReplaceFuns) <- "checkReplaceFuns"
    badReplaceFuns
}

print.checkReplaceFuns <-
function(x, ...)
{
    if(length(x) > 0) print(unclass(x), ...)
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
        ## Using package installed in @code{dir} ...
        dir <- .find.package(package, lib.loc)
        if(file.exists(file.path(dir, "R", "all.rda"))) {
            warning("cannot check R code installed as image")
        }
        codeFile <- file.path(dir, "R", package)
        if(file.exists(codeFile))       # could be data-only
            codeFiles <- codeFile
        exampleDir <- file.path(dir, "R-ex")
        if(fileTest("-d", exampleDir)) {
            codeFiles <- c(codeFiles,
                           listFilesWithExts(exampleDir, "R"))

        }
    }
    else if(!missing(dir)) {
        ## Using sources from directory @code{dir} ...
        if(!fileTest("-d", dir))
            stop(paste("directory", sQuote(dir), "does not exist"))
        else
            dir <- filePathAsAbsolute(dir)
        codeDir <- file.path(dir, "R")
        if(fileTest("-d", codeDir))    # could be data-only
            codeFiles <- listFilesWithType(codeDir, "code")
        docsDir <- file.path(dir, "man")
        if(fileTest("-d", docsDir))
            docsFiles <- listFilesWithType(docsDir, "docs")
    }
    else if(!missing(file)) {
        if(!fileTest("-f", file))
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
        exprs <- try(parse(file = file, n = -1))
        if(inherits(exprs, "try-error"))
            stop(paste("parse error in file", sQuote(file)))
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
