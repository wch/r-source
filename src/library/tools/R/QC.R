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
        dataEnv <- new.env()
        files <- listFilesWithType(dataDir, "data")
        files <- unique(basename(filePathSansExt(files)))
        for(f in files) {
            yy <- try(data(list = f,
                           package = basename(dir),
                           lib.loc = dirname(dir),
                           envir = dataEnv))
            if(inherits(yy, "try-error"))
                stop(paste("cannot load data set", sQuote(f)))
            new <- ls(envir = dataEnv, all.names = TRUE)
            dataObjs <- c(dataObjs, new)
            rm(list = new, envir = dataEnv)
        }
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
        ## setGeneric() or setMethod() on 'ordinary' functions.
        ## The test below exempts objects that are generic functions if
        ## there is a visible nongeneric function and the default method
        ## is "derived", by a call to setGeneric.  This test allows
        ## nondocumentd generics in some cases (e.g., the generic was
        ## created locally from an inconsistent version).
        ## In the long run we need dynamic documentation.
        if(.isMethodsDispatchOn()) {
            codeObjs <-
                codeObjs[sapply(codeObjs, function(f) {
                    fdef <- get(f, envir = codeEnv)
                    if(methods::is(fdef, "genericFunction")) {
                        fOther <-
                            methods::getFunction(f, generic = FALSE,
                                                 mustFind = FALSE,
                                                 where = topenv(environment(fdef)))
                        if(is.null(fOther))
                            TRUE
                        else 
                            !methods::is(methods::finalDefaultMethod(methods::getMethodsMetaData(f, codeEnv)),
                                         "derivedDefaultMethod")
                    }
                    else
                        TRUE
                }) == TRUE]
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

    ## <FIXME>
    ## Currently, loading data from an R file via sys.source() puts
    ## .required into the load environment if the R code has a call to
    ## require().
    dataObjs <- dataObjs[! dataObjs %in% c(".required")]
    ## </FIXME>

    undocThings <-
        list("code objects" =
             unique(codeObjs[! codeObjs %in% allDocTopics]),
             "data sets" =
             unique(dataObjs[! dataObjs %in% allDocTopics]))

    if(.isMethodsDispatchOn()) {
        ## Undocumented S4 classes?
        S4classes <- methods::getClasses(codeEnv)
        ## <NOTE>
        ## There is no point in worrying about exportClasses directives
        ## in a NAMESPACE file when working on a package source dir, as
        ## we only source the assignments, and hence do not get any
        ## S4 classes or methods.
        ## </NOTE>
        ## The bad ones:
        S4classes <-
            S4classes[!sapply(S4classes,
                              function(u) topicName("class", u))
                      %in% allDocTopics]
        undocThings <-
            c(undocThings, list("S4 classes" = unique(S4classes)))
    }

    if(.isMethodsDispatchOn()) {
        ## Undocumented S4 methods?
        ## <NOTE>
        ## There is no point in worrying about exportMethods directives
        ## in a NAMESPACE file when working on a package source dir, as
        ## we only source the assignments, and hence do not get any
        ## S4 classes or methods.
        ## </NOTE>
        methodsSignatures <- function(f) {
            mlist <- methods::getMethodsMetaData(f, codeEnv)
            meths <- methods::linearizeMlist(mlist, FALSE)
            classes <- methods::slot(meths, "classes")
            default <-
                as.logical(lapply(classes,
                                  function(x)
                                  identical(all(x == "ANY"), TRUE)))
            if(any(default)) {
                ## Don't look for doc on a generated default method.
                if(methods::is(methods::finalDefaultMethod(mlist),
                               "derivedDefaultMethod"))
                    classes <- classes[!default]
            }
            sigs <- sapply(classes, paste, collapse = ",")
            if(length(sigs))
                paste(f, ",", sigs, sep = "")
            else
                character()
        }
        S4methods <-
            sapply(methods::getGenerics(codeEnv), methodsSignatures)
        S4methods <- as.character(unlist(S4methods, use.names = FALSE))
        ## The bad ones:
        S4methods <-
            S4methods[!sapply(S4methods,
                              function(u) topicName("method", u))
                      %in% allDocTopics]
        undocThings <-
            c(undocThings,
              list("S4 methods" =
                   unique(sub("([^,]*),(.*)",
                              "\\\\S4method{\\1}{\\2}",
                              S4methods))))
    }

    class(undocThings) <- "undoc"
    undocThings
}

print.undoc <-
function(x, ...)
{
    for(i in which(sapply(x, length) > 0)) {
        writeLines(paste("Undocumented ", names(x)[i], ":", sep = ""))
        .prettyPrint(x[[i]])
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
    ## <FIXME>
    ## Improvements worth considering:
    ## * Parallelize the actual checking (it is not necessary to loop
    ##   over the Rd files);
    ## * In case of a namespace, always use the namespace for codoc
    ##   computations (as it is also used for determining the usages for
    ##   which no corresponding object in the package exists), rather
    ##   than just the exported objects.
    ## </FIXME>

    hasNamespace <- FALSE

    ## Argument handling.
    ## <FIXME>
    ## Remove these arguments for 2.0.
    if(!missing(use.positions))
        warning("argument", sQuote("use.positions"),
                "is deprecated")
    if(!missing(ignore.generic.functions))
        warning("argument", sQuote("ignore.generic.functions"),
                "is deprecated")
    ## </FIXME>
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

        objectsInCode <- objects(envir = codeEnv, all.names = TRUE)

        ## Does the package have a namespace?
        if(packageHasNamespace(package, dirname(dir))) {
            hasNamespace <- TRUE
            objectsInCodeOrNamespace <-
                objects(envir = asNamespace(package), all.names = TRUE)
        }
        else
            objectsInCodeOrNamespace <- objectsInCode
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

        objectsInCode <- objects(envir = codeEnv, all.names = TRUE)
        objectsInCodeOrNamespace <- objectsInCode

        ## Does the package have a NAMESPACE file?  Note that when
        ## working on the sources we (currently?) cannot deal with the
        ## (experimental) alternative way of specifying the namespace.
        if(file.exists(file.path(dir, "NAMESPACE"))) {
            hasNamespace <- TRUE
            nsInfo <- parseNamespaceFile(basename(dir), dirname(dir))
            ## Look only at exported objects.
            OK <- objectsInCode[objectsInCode %in% nsInfo$exports]
            for(p in nsInfo$exportPatterns)
                OK <- c(OK, grep(p, objectsInCode, value = TRUE))
            objectsInCode <- unique(OK)
        }
    }

    ## Find the function objects to work on.
    functionsInCode <-
        objectsInCode[sapply(objectsInCode,
                             function(f) {
                                 f <- get(f, envir = codeEnv)
                                 is.function(f) && (length(formals(f)) > 0)
                             }) == TRUE]
    if(ignore.generic.functions) {
        ## Ignore all generics, whatever name they dispatch on.
        functionsInCode <-
            functionsInCode[sapply(functionsInCode,
                                   .isS3Generic,
                                   codeEnv,
                                   FALSE)
                            == FALSE]
    }
    ## <FIXME>
    ## Sourcing all R code files in the package is a problem for base,
    ## where this misses the .Primitive functions.  Hence, when checking
    ## base for objects shown in \usage but missing from the code, we
    ## get the primitive functions from the version of R we are using.
    ## Maybe one day we will have R code for the primitives as well ...
    if(isBase) {
        objectsInBase <-
            objects(envir = as.environment(NULL), all.names = TRUE)
        objectsInCode <-
            c(objectsInCode,
              objectsInBase[sapply(objectsInBase,
                                   .isPrimitive,
                                   NULL)],
              c(".First.lib", ".Last.lib", ".Random.seed"))
        objectsInCodeOrNamespace <- objectsInCode
    }
    ## </FIXME>

    ## Build a list with the formals of the functions in the code
    ## indexed by the names of the functions.
    functionArgsInCode <-
        lapply(functionsInCode,
               function(f) formals(get(f, envir = codeEnv)))
    names(functionArgsInCode) <- functionsInCode
    if(.isMethodsDispatchOn()) {
        ## <NOTE>
        ## There is no point in worrying about exportMethods directives
        ## in a NAMESPACE file when working on a package source dir, as
        ## we only source the assignments, and hence do not get any
        ## S4 classes or methods.
        ## </NOTE>
        lapply(methods::getGenerics(codeEnv),
               function(f) {
                   meths <-
                       methods::linearizeMlist(methods::getMethodsMetaData(f, codeEnv))
                   sigs <- sapply(methods::slot(meths, "classes"),
                                  paste, collapse = ",")
                   if(!length(sigs)) return()
                   args <- lapply(methods::slot(meths, "methods"),
                                  formals)
                   names(args) <-
                       paste("\\S4method{", f, "}{", sigs, "}",
                             sep = "")
                   functionArgsInCode <<- c(functionArgsInCode, args)
               })
    }

    checkCoDoc <- function(fName, ffd) {
        ## Compare the formals of the function in the code named 'fName'
        ## and formals 'ffd' obtained from the documentation.
        ffc <- functionArgsInCode[[fName]]
        if(identical(use.positions, FALSE)) {
            ffc <- ffc[sort(names(ffc))]
            ffd <- ffc[sort(names(ffd))]
        }
        if(identical(use.values, FALSE)) {
            ffc <- names(ffc)
            ffd <- names(ffd)
            ok <- identical(ffc, ffd)
        } else {
            if(!identical(names(ffc), names(ffd)))
                ok <- FALSE
            else {
                vffc <- as.character(ffc) # values
                vffd <- as.character(ffd) # values
                if(!identical(use.values, TRUE)) {
                    ind <- nchar(as.character(ffd)) > 0
                    vffc <- vffc[ind]
                    vffd <- vffd[ind]
                }
                ok <- identical(vffc, vffd)
            }
        }
        if(ok)
            NULL
        else
            list(list(name = fName, code = ffc, docs = ffd))
    }

    db <- if(!missing(package))
        Rddb(package, lib.loc = dirname(dir))
    else
        Rddb(dir = dir)

    db <- lapply(db,
                 function(f) paste(Rdpp(f), collapse = "\n"))
    names(db) <- dbNames <- sapply(db, getRdSection, "name")
    if(isBase) {
        ind <- dbNames %in% c("Defunct", "Devices")
        db <- db[!ind]
        dbNames <- dbNames[!ind]
    }
    dbUsageTexts <- lapply(db, getRdSection, "usage")
    dbSynopses <- lapply(db, getRdSection, "synopsis")
    ind <- sapply(dbSynopses, length) > 0
    dbUsageTexts[ind] <- dbSynopses[ind]
    withSynopsis <- as.character(dbNames[ind])
    dbUsages <-
        lapply(dbUsageTexts,
               function(txt) {
                   methodRE <-
                       paste("(\\\\(S[34])?method",
                             "{([.[:alnum:]]*)}",
                             "{([.[:alnum:]]*)})",
                             sep = "")
                   txt <- gsub("\\\\l?dots", "...", txt)
                   txt <- gsub("\\\\%", "%", txt)
                   txt <- gsub(methodRE, "\"\\\\\\1\"", txt)
                   .parseTextAsMuchAsPossible(txt)
               })
    ind <- sapply(dbUsages,
                  function(x) !is.null(attr(x, "badLines")))
    badLines <- sapply(dbUsages[ind], attr, "badLines")

    ## <FIXME>
    ## Currently, there is no useful markup for S3 Ops group methods
    ## and S3 methods for subscripting and subassigning.  Hence, we
    ## cannot reliably distinguish between usage for the generic and
    ## that of a method ...
    functionsToBeIgnored <-
        c(.functionsToBeIgnoredFromUsage(basename(dir)),
          .functionsWithNoUsefulS3methodMarkup)
    ## </FIXME>

    badDocObjects <- list()
    functionsInUsages <- character()
    variablesInUsages <- character()
    dataSetsInUsages <- character()
    functionsInUsagesNotInCode <- list()

    for(docObj in dbNames) {

        exprs <- dbUsages[[docObj]]
        if(!length(exprs)) next

        ## Get variable names and data set usages first, mostly for
        ## curiosity.
        ## <FIXME>
        ## Use '<=' as we could get 'NULL' ... although of course this
        ## is not really a variable.
        ind <- sapply(exprs, length) <= 1
        ## </FIXME>
        if(any(ind)) {
            variablesInUsages <-
                c(variablesInUsages,
                  sapply(exprs[ind], deparse))
            exprs <- exprs[!ind]
        }
        ind <- as.logical(sapply(exprs,
                                 function(e)
                                 (length(e) == 2)
                                 && e[[1]] == as.symbol("data")))
        if(any(ind)) {
            dataSetsInUsages <-
                c(dataSetsInUsages,
                  sapply(exprs[ind], function(e) as.character(e[[2]])))
            exprs <- exprs[!ind]
        }
        functions <- sapply(exprs, function(e) as.character(e[[1]]))
        functions <- .transformS3methodMarkup(as.character(functions))
        ind <- (! functions %in% functionsToBeIgnored
                & functions %in% functionsInCode)
        badFunctions <-
            mapply(functions[ind],
                   exprs[ind],
                   FUN = function(x, y)
                   checkCoDoc(x, as.pairlist(as.alist.call(y[-1]))),
                   SIMPLIFY = FALSE)
        ## Replacement functions.
        ind <- as.logical(sapply(exprs,
                                 .isCallFromReplacementFunctionUsage))
        if(any(ind)) {
            exprs <- exprs[ind]
            replaceFuns <-
                paste(sapply(exprs,
                             function(e) as.character(e[[2]][[1]])),
                      "<-",
                      sep = "")
            replaceFuns <- .transformS3methodMarkup(replaceFuns)
            functions <- c(functions, replaceFuns)
            ind <- (replaceFuns %in% functionsInCode)
            if(any(ind)) {
                badReplaceFuns <-
                    mapply(replaceFuns[ind],
                           exprs[ind],
                           FUN = function(x, y)
                           checkCoDoc(x,
                                      as.pairlist(c(as.alist.call(y[[2]][-1]),
                                                    as.alist.symbol(y[[3]])))),
                           SIMPLIFY = FALSE)
                badFunctions <-
                    c(badFunctions, badReplaceFuns)
            }
        }

        badFunctions <- do.call("c", badFunctions)
        if(length(badFunctions) > 0)
            badDocObjects[[docObj]] <- badFunctions

        ## Determine functions with a \usage entry in the documentation
        ## but 'missing from the code'.  If a package has a namespace, we
        ## really need to look at all objects in the namespace (hence
        ## 'objectsInCodeOrNamespace'), as one can access the internal
        ## symbols via ':::' and hence package developers might want to
        ## provide function usages for some of the internal functions.
        badFunctions <-
            functions[! functions %in%
                      c(objectsInCodeOrNamespace, functionsToBeIgnored)]
        if(length(badFunctions) > 0)
            functionsInUsagesNotInCode[[docObj]] <- badFunctions

        functionsInUsages <- c(functionsInUsages, functions)
    }

    ## Determine (function) objects in the code without a \usage entry.
    ## Of course, these could still be 'documented' via \alias.
    ## </NOTE>
    ## Older versions only printed this information without returning it
    ## (in case 'verbose' was true).  We now add this as an attribute to
    ## the badDocObjects returned.
    ## </NOTE>
    objectsInCodeNotInUsages <-
        objectsInCode[! objectsInCode %in%
                      c(functionsInUsages, variablesInUsages)]
    functionsInCodeNotInUsages <-
        functionsInCode[functionsInCode %in% objectsInCodeNotInUsages]
    ## (Note that 'functionsInCode' does not necessarily contain all
    ## (exported) functions in the package.)

    attr(badDocObjects, "objectsInCodeNotInUsages") <-
        objectsInCodeNotInUsages
    attr(badDocObjects, "functionsInCodeNotInUsages") <-
        functionsInCodeNotInUsages
    attr(badDocObjects, "functionsInUsagesNotInCode") <-
        functionsInUsagesNotInCode
    attr(badDocObjects, "functionArgsInCode") <- functionArgsInCode
    attr(badDocObjects, "hasNamespace") <- hasNamespace
    attr(badDocObjects, "withSynopsis") <- withSynopsis
    attr(badDocObjects, "badLines") <- badLines
    class(badDocObjects) <- "codoc"
    badDocObjects
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
    ## (This will change now with the new \method{}{} transformation.)
    ## Also, earlier versions od codoc() based on extract-usage.pl only
    ## dealt with the *functions* so all variables would come out as
    ## 'without usage information' ...
    ## As we can always access the information via
    ##    attr(codoc("foo"), "codeNotInUsages")
    ## disable reporting this for the time being ...
    ## <COMMENT>
    ##     objectsInCodeNotInUsages <-
    ##         attr(x, "objectsInCodeNotInUsages")
    ##     if(length(objectsInCodeNotInUsages)
    ##        && identical(TRUE, attr(x, "hasNamespace"))) {
    ##         if(length(objectsInCodeNotInUsages)) {
    ##             writeLines("Exported objects without usage information:")
    ##             .prettyPrint(objectsInCodeNotInUsages)
    ##             writeLines("")
    ##         }
    ##     }
    ## </COMMENT>
    ## Hmm.  But why not mention the exported *functions* without \usage
    ## information?  Note that currently there is no useful markup for
    ## S3 Ops group methods and S3 methods for subscripting and
    ## subassigning, so the corresponding generics and methods cannot
    ## reliably be distinguished, and hence would need to be excluded
    ## here as well.
    ## <COMMENT>
    ##     functionsInCodeNotInUsages <-
    ##         attr(x, "functionsInCodeNotInUsages")
    ##     if(length(functionsInCodeNotInUsages)
    ##        && identical(TRUE, attr(x, "hasNamespace"))) {
    ##         if(length(functionsInCodeNotInUsages)) {
    ##             writeLines("Exported functions without usage information:")
    ##             .prettyPrint(functionsInCodeNotInUsages)
    ##             writeLines("")
    ##         }
    ##     }
    ## </COMMENT>
    ## </FIXME>

    functionsInUsagesNotInCode <-
        attr(x, "functionsInUsagesNotInCode")
    if(length(functionsInUsagesNotInCode) > 0) {
        for(fname in names(functionsInUsagesNotInCode)) {
            writeLines(paste("Functions/methods with usage in",
                             "documentation object", sQuote(fname),
                             "but not in code:"))
            .prettyPrint(unique(functionsInUsagesNotInCode[[fname]]))
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

### * codocClasses

codocClasses <-
function(package, lib.loc = NULL)
{
    ## Compare the 'structure' of S4 classes in an installed package
    ## between code and documentation.
    ## Currently, only compares the slot names.

    ## <NOTE>
    ## This is patterned after the current codoc().
    ## It would be useful to return the whole information on class slot
    ## names found in the code and matching documentation (rather than
    ## just the ones with mismatches).
    ## Currently, we only return the names of all classes checked.
    ## </NOTE>

    badRdObjects <- list()
    class(badRdObjects) <- "codocClasses"

    ## Argument handling.
    if(length(package) != 1)
        stop(paste("argument", sQuote("package"),
                   "must be of length 1"))
    dir <- .find.package(package, lib.loc)
    if(!fileTest("-d", file.path(dir, "R")))
        stop(paste("directory", sQuote(dir),
                   "does not contain R code"))
    if(!fileTest("-d", file.path(dir, "man")))
        stop(paste("directory", sQuote(dir),
                   "does not contain Rd sources"))
    isBase <- basename(dir) == "base"

    ## Load package into codeEnv.
    if(!isBase)
        .loadPackageQuietly(package, lib.loc)
    codeEnv <-
        as.environment(paste("package", package, sep = ":"))

    if(!.isMethodsDispatchOn())
        return(badRdObjects)

    S4classes <- methods::getClasses(codeEnv)
    if(!length(S4classes)) return(badRdObjects)

    ## Build Rd data base.
    db <- Rddb(package, lib.loc = dirname(dir))
    db <- lapply(db, Rdpp)

    ## Need some heuristics now.  When does an Rd object document just
    ## one S4 class so that we can compare (at least) the slot names?
    ## Try the following:
    ## * \docType{} identical to "class";
    ## * just one \alias{} (could also check whether it ends in
    ##   "-class");
    ## * a non-empty user-defined section 'Slots'.

    ## As going through the db to extract sections can take some time,
    ## we do the vectorized metadata computations first, and try to
    ## subscript whenever possible.

    aliases <- lapply(db, .getRdMetaDataFromRdLines, "alias")
    idx <- (sapply(aliases, length) == 1)
    if(!any(idx)) return(badRdObjects)
    db <- db[idx]; aliases <- aliases[idx]
    idx <- sapply(lapply(db, .getRdMetaDataFromRdLines, "docType"),
                  identical, "class")
    if(!any(idx)) return(badRdObjects)
    db <- db[idx]; aliases <- aliases[idx]
    ## Now collapse.
    db <- lapply(db, paste, collapse = "\n")
    RdSlots <- lapply(db, getRdSection, "Slots", FALSE)
    idx <- !sapply(RdSlots, identical, character())
    if(!any(idx)) return(badRdObjects)
    db <- db[idx]
    aliases <- unlist(aliases[idx])
    RdSlots <- RdSlots[idx]

    dbNames <- sapply(db, .getRdName)
    if(length(dbNames) < length(db)) {
        ## <FIXME>
        ## What should we really do in this case?
        ## (We cannot refer to the bad Rd objects because we do not know
        ## their names, and have no idea which file they came from ...)
        stop("cannot deal with Rd objects with missing/empty names")
        ## </FIXME>
    }
    names(db) <- dbNames

    .getSlotNamesFromSlotSectionText <- function(txt) {
        ## Get \describe (inside user-defined section 'Slots'
        txt <- unlist(sapply(txt, getRdSection, "describe"))
        ## Suppose this worked ...
        ## Get the \items inside \describe
        txt <- unlist(sapply(txt, getRdItems))
        if(!length(txt)) return(character())
        ## And now strip enclosing '\code{...}:'
        txt <- gsub("\\\\code\{(.*)\}:?", "\\1", as.character(txt))
        txt <- unlist(strsplit(txt, ", *"))
        txt <- sub("^[[:space:]]*", "", txt)
        txt <- sub("[[:space:]]*$", "", txt)
        txt
    }

    S4classesChecked <- character()
    for(cl in S4classes) {
        idx <- which(topicName("class", cl) == aliases)
        if(length(idx) == 1) {
            ## Add sanity checking later ...
            S4classesChecked <- c(S4classesChecked, cl)
            slotsInCode <-
                sort(names(methods::slot(methods::getClass(cl, where =
                                                           codeEnv),
                                         "slots")))
            slotsInDocs <-
                sort(.getSlotNamesFromSlotSectionText(RdSlots[[idx]]))
            if(!identical(slotsInCode, slotsInDocs)) {
                badRdObjects[[names(db)[idx]]] <-
                    list(name = cl,
                         code = slotsInCode,
                         docs = slotsInDocs)
            }
        }
    }

    attr(badRdObjects, "S4classesChecked") <-
        as.character(S4classesChecked)
    badRdObjects
}

print.codocClasses <-
function(x, ...)
{
    if (length(x) == 0)
        return(invisible(x))
    formatArgs <- function(s) paste(s, collapse = " ")
    for (docObj in names(x)) {
        writeLines(paste("S4 class codoc mismatches from ",
                         "documentation object ", sQuote(docObj), ":",
                         sep = ""))
        docObj <- x[[docObj]]
        writeLines(c(paste("Slots for class", sQuote(docObj[["name"]])),
                     strwrap(paste("Code:",
                                   formatArgs(docObj[["code"]])),
                             indent = 2, exdent = 8),
                     strwrap(paste("Docs:",
                                   formatArgs(docObj[["docs"]])),
                             indent = 2, exdent = 8)))
        writeLines("")
    }
    invisible(x)
}

### * codocData

codocData <-
function(package, lib.loc = NULL)
{
    ## Compare the 'structure' of 'data' objects (variables or data
    ## sets) in an installed package between code and documentation.
    ## Currently, only compares the variable names of data frames found.

    ## <NOTE>
    ## This is patterned after the current codoc().
    ## It would be useful to return the whole information on data frame
    ## variable names found in the code and matching documentation
    ## (rather than just the ones with mismatches).
    ## Currently, we only return the names of all data frames checked.
    ## </NOTE>

    badRdObjects <- list()
    class(badRdObjects) <- "codocData"

    ## Argument handling.
    if(length(package) != 1)
        stop(paste("argument", sQuote("package"),
                   "must be of length 1"))
    
    dir <- .find.package(package, lib.loc)
    if(!fileTest("-d", file.path(dir, "man")))
       stop(paste("directory", sQuote(dir),
                  "does not contain Rd sources"))
    isBase <- basename(dir) == "base"

    ## Load package into codeEnv.
    if(!isBase)
        .loadPackageQuietly(package, lib.loc)
    codeEnv <-
        as.environment(paste("package", package, sep = ":"))

    ## Could check here whether the package has any variables or data
    ## sets (and return if not).

    ## Build Rd data base.
    db <- Rddb(package, lib.loc = dirname(dir))
    db <- lapply(db, Rdpp)

    ## Need some heuristics now.  When does an Rd object document a
    ## data.frame (could add support for other classes later) variable
    ## or data set so that we can compare (at least) the names of the
    ## variables in the data frame?  Try the following:
    ## * just one \alias{};
    ## * if documentation was generated via prompt, there is a \format
    ##   section starting with 'A data frame with' (but many existing Rd
    ##   files instead have 'This data frame contains' and containing
    ##   one or more \describe sections inside.

    ## As going through the db to extract sections can take some time,
    ## we do the vectorized metadata computations first, and try to
    ## subscript whenever possible.
    aliases <- lapply(db, .getRdMetaDataFromRdLines, "alias")
    idx <- sapply(aliases, length) == 1
    if(!any(idx)) return(badRdObjects)
    db <- db[idx]; aliases <- aliases[idx]
    ## Now collapse.
    db <- lapply(db, paste, collapse = "\n")

    .getDataFrameVarNamesFromRdText <- function(txt) {
        txt <- getRdSection(txt, "format")
        ## Was there just one \format section?
        if(length(txt) != 1) return(character())
        ## What did it start with?
        if(!length(grep("^[ \n\t]*(A|This) data frame", txt)))
            return(character())
        ## Get \describe inside \format
        txt <- getRdSection(txt, "describe")
        ## Suppose this worked ...
        ## Get the \items inside \describe
        txt <- unlist(sapply(txt, getRdItems))
        if(!length(txt)) return(character())
        txt <- gsub("(.*):$", "\\1", as.character(txt))
        txt <- gsub("\\\\code\{(.*)\}:?", "\\1", txt)
        txt <- unlist(strsplit(txt, ", *"))
        txt <- sub("^[[:space:]]*", "", txt)
        txt <- sub("[[:space:]]*$", "", txt)
        txt
    }

    RdVarNames <- lapply(db, .getDataFrameVarNamesFromRdText)
    idx <- (sapply(RdVarNames, length) > 0)
    if(!length(idx)) return(badRdObjects)
    aliases <- unlist(aliases[idx])
    RdVarNames <- RdVarNames[idx]

    dbNames <- sapply(db[idx], .getRdName)
    if(length(dbNames) < length(aliases)) {
        ## <FIXME>
        ## What should we really do in this case?
        ## (We cannot refer to the bad Rd objects because we do not know
        ## their names, and have no idea which file they came from ...)
        stop("cannot deal with Rd objects with missing/empty names")
        ## </FIXME>
    }

    dataEnv <- new.env()
    dataDir <- file.path(dir, "data")
    hasData <- fileTest("-d", dataDir)
    dataExts <- .makeFileExts("data")

    ## Now go through the aliases.
    dataFramesChecked <- character()
    for(i in seq(along = aliases)) {
        ## Store the documented variable names.
        varNamesInDocs <- sort(RdVarNames[[i]])
        ## Try finding the variable or data set given by the alias.
        al <- aliases[i]
        if(exists(al, envir = codeEnv, mode = "list",
                  inherits = FALSE)) {
            al <- get(al, envir = codeEnv, mode = "list")
        }
        else if(hasData) {
            ## Should be a data set.
            if(!length(dir(dataDir)
                       %in% paste(al, dataExts, sep = "."))) {
                next                    # What the hell did we pick up?
            }
            ## Try loading the data set into dataEnv.
            data(list = al, envir = dataEnv)
            if(exists(al, envir = dataEnv, mode = "list",
                      inherits = FALSE)) {
                al <- get(al, envir = dataEnv, mode = "list")
            }
            ## And clean up dataEnv.
            rm(list = ls(envir = dataEnv, all.names = TRUE),
               envir = dataEnv)
        }
        if(!is.data.frame(al)) next
        ## Now we should be ready:
        dataFramesChecked <- c(dataFramesChecked, aliases[i])
        varNamesInCode <- sort(variable.names(al))
        if(!identical(varNamesInCode, varNamesInDocs))
            badRdObjects[[dbNames[i]]] <-
                list(name = aliases[i],
                     code = varNamesInCode,
                     docs = varNamesInDocs)
    }

    attr(badRdObjects, "dataFramesChecked") <-
        as.character(dataFramesChecked)
    badRdObjects
}

print.codocData <-
function(x, ...)
{
    formatArgs <- function(s) paste(s, collapse = " ")
    for (docObj in names(x)) {
        writeLines(paste("Data codoc mismatches from ",
                         "documentation object ", sQuote(docObj), ":",
                         sep = ""))
        docObj <- x[[docObj]]
        writeLines(c(paste("Variables in data frame",
                           sQuote(docObj[["name"]])),
                     strwrap(paste("Code:",
                                   formatArgs(docObj[["code"]])),
                             indent = 2, exdent = 8),
                     strwrap(paste("Docs:",
                                   formatArgs(docObj[["docs"]])),
                             indent = 2, exdent = 8)))
        writeLines("")
    }
    invisible(x)
}

### * checkDocFiles

checkDocFiles <-
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

    db <- if(!missing(package))
        Rddb(package, lib.loc = dirname(dir))
    else
        Rddb(dir = dir)

    db <- lapply(db, Rdpp)
    ## Do vectorized computations for metadata first.
    dbAliases <- lapply(db, .getRdMetaDataFromRdLines, "alias")
    dbKeywords <- lapply(db, .getRdMetaDataFromRdLines, "keyword")
    ## Now collapse.
    db <- lapply(db, paste, collapse = "\n")
    dbNames <- sapply(db, .getRdName)
    ## Safeguard against missing/empty names.
    if(length(dbNames) < length(db)) {
        ## <FIXME>
        ## What should we really do in this case?
        ## (We cannot refer to the bad Rd objects because we do not know
        ## their names, and have no idea which file they came from ...)
        stop("cannot deal with Rd objects with missing/empty names")
        ## </FIXME>
    }
    ind <- sapply(dbKeywords,
                  function(x) any(grep("^ *internal *$", x)))
    if(isBase)
        ind <- ind | dbNames %in% c("Defunct", "Deprecated", "Devices")
    if(any(!ind)) {
        db <- db[!ind]
        dbNames <- dbNames[!ind]
        dbAliases <- dbAliases[!ind]
    }
    names(db) <- names(dbAliases) <- dbNames
    dbUsageTexts <- lapply(db, getRdSection, "usage")
    dbUsages <-
        lapply(dbUsageTexts,
               function(txt) {
                   methodRE <-
                       paste("(\\\\(S[34])?method",
                             "{([.[:alnum:]]*)}",
                             "{([.[:alnum:]]*)})",
                             sep = "")
                   txt <- gsub("\\\\l?dots", "...", txt)
                   txt <- gsub("\\\\%", "%", txt)
                   txt <- gsub(methodRE, "\"\\\\\\1\"", txt)
                   .parseTextAsMuchAsPossible(txt)
               })
    ind <- sapply(dbUsages,
                  function(x) !is.null(attr(x, "badLines")))
    badLines <- sapply(dbUsages[ind], attr, "badLines")
    dbArgumentNames <- lapply(db, .getRdArgumentNames)

    functionsToBeIgnored <-
        .functionsToBeIgnoredFromUsage(basename(dir))

    badDocObjs <- list()

    for(docObj in dbNames) {

        exprs <- dbUsages[[docObj]]
        if(!length(exprs)) next

        aliases <- dbAliases[[docObj]]
        argNamesInArgList <- dbArgumentNames[[docObj]]

        ## Determine function names ('functions') and corresponding
        ## arguments ('argNamesInUsage') in the \usage.  Note how we
        ## try to deal with data set documentation.
        ind <- as.logical(sapply(exprs,
                                 function(e)
                                 ((length(e) > 1) &&
                                  !((length(e) == 2)
                                    && e[[1]] == as.symbol("data")))))
        exprs <- exprs[ind]
        ## Ordinary functions.
        functions <- as.character(sapply(exprs,
                                         function(e)
                                         as.character(e[[1]])))
        ## (Note that as.character(sapply(exprs, "[[", 1)) does not do
        ## what we want due to backquotifying.)
        ind <- ! functions %in% functionsToBeIgnored
        functions <- functions[ind]
        argNamesInUsage <-
            unlist(sapply(exprs[ind],
                          function(e) .argNamesFromCall(e[-1])))
        ## Replacement functions.
        ind <- as.logical(sapply(exprs,
                                 .isCallFromReplacementFunctionUsage))
        if(any(ind)) {
            replaceFuns <-
                paste(sapply(exprs[ind],
                             function(e) as.character(e[[2]][[1]])),
                      "<-",
                      sep = "")
            functions <- c(functions, replaceFuns)
            argNamesInUsage <-
                c(argNamesInUsage,
                  unlist(sapply(exprs[ind],
                                function(e)
                                c(.argNamesFromCall(e[[2]][-1]),
                                  .argNamesFromCall(e[[3]])))))
        }
        ## And finally transform the S3 \method{}{} markup into the
        ## usual function names ...
        ## <NOTE>
        ## If we were really picky, we would worry about possible
        ## namespace renaming.
        functions <- .transformS3methodMarkup(functions)
        ## </NOTE>

        ## Now analyze what we found.
        argNamesInUsageMissingInArgList <-
            argNamesInUsage[!argNamesInUsage %in% argNamesInArgList]
        argNamesInArgListMissingInUsage <-
            argNamesInArgList[!argNamesInArgList %in% argNamesInUsage]
        if(length(argNamesInArgListMissingInUsage) > 0) {
            usageText <- dbUsageTexts[[docObj]]
            ## In the case of 'over-documented' arguments, try to be
            ## defensive and reduce to arguments that do not match the
            ## \usage text (modulo word boundaries).
            bad <- sapply(argNamesInArgListMissingInUsage,
                          function(x)
                          regexpr(paste("\\b", x, "\\b", sep = ""),
                                  usageText) == -1)
            argNamesInArgListMissingInUsage <-
                argNamesInArgListMissingInUsage[bad]
            ## Note that the fact that we can parse the raw \usage does
            ## not imply that over-documented arguments are a problem:
            ## this works for Rd files documenting e.g. shell utilities
            ## but fails for files with special syntax (Extract.Rd).
        }

        ## Also test whether the objects we found from the \usage all
        ## have aliases, provided that there is no alias which ends in
        ## '-deprecated' (see Deprecated.Rd).
        if(!any(grep("-deprecated$", aliases))) {
            ## Currently, there is no useful markup for S3 Ops group
            ## methods and S3 methods for subscripting and subassigning,
            ## so the corresponding generics and methods need to be
            ## excluded from this test (e.g., the usage for '+' in
            ## 'DateTimeClasses.Rd' ...).
            functions <- functions[!functions %in%
                                   .functionsWithNoUsefulS3methodMarkup]
            ## Argh.  There are good reasons for keeping \S4method{}{}
            ## as is, but of course this is not what the aliases use ...
            ## <FIXME>
            ## Should maybe use topicName(), but in any case, we should
            ## have functions for converting between the two forms, see
            ## also the code for undoc().
            aliases <- sub("([^,]+),(.+)-method$",
                           "\\\\S4method{\\1}{\\2}",
                           aliases)
            ## </FIXME>
            aliases <- gsub("\\\\%", "%", aliases)
            functionsNotInAliases <- functions[! functions %in% aliases]
        }
        else
            functionsNotInAliases <- character()

        if((length(argNamesInUsageMissingInArgList) > 0)
           || any(duplicated(argNamesInArgList))
           || (length(argNamesInArgListMissingInUsage) > 0)
           || (length(functionsNotInAliases) > 0))
            badDocObjs[[docObj]] <-
                list(missing = argNamesInUsageMissingInArgList,
                     duplicated =
                     argNamesInArgList[duplicated(argNamesInArgList)],
                     overdoc = argNamesInArgListMissingInUsage,
                     unaliased = functionsNotInAliases)

    }

    class(badDocObjs) <- "checkDocFiles"
    attr(badDocObjs, "badLines") <- badLines
    badDocObjs
}

print.checkDocFiles <-
function(x, ...)
{
    for(docObj in names(x)) {
        argNamesInUsageMissingInArgList <- x[[docObj]][["missing"]]
        if(length(argNamesInUsageMissingInArgList) > 0) {
            writeLines(paste("Undocumented arguments",
                             " in documentation object ",
                             sQuote(docObj), ":", sep = ""))
            .prettyPrint(unique(argNamesInUsageMissingInArgList))
        }
        duplicatedArgsInArgList <- x[[docObj]][["duplicated"]]
        if(length(duplicatedArgsInArgList) > 0) {
            writeLines(paste("Duplicated \\argument entries",
                             " in documentation object ",
                             sQuote(docObj), ":", sep = ""))
            .prettyPrint(duplicatedArgsInArgList)
        }
        argNamesInArgListMissingInUsage <- x[[docObj]][["overdoc"]]
        if(length(argNamesInArgListMissingInUsage) > 0) {
            writeLines(paste("Documented arguments not in \\usage",
                             " in documentation object ",
                             sQuote(docObj), ":", sep = ""))
            .prettyPrint(unique(argNamesInArgListMissingInUsage))
        }
        functionsNotInAliases <- x[[docObj]][["unaliased"]]
        if(length(functionsNotInAliases) > 0) {
            writeLines(paste("Objects in \\usage without \\alias",
                             " in documentation object ",
                             sQuote(docObj), ":", sep = ""))
            .prettyPrint(unique(functionsNotInAliases))
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

        objectsInCode <- objects(envir = codeEnv, all.names = TRUE)

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

        objectsInCode <- objects(envir = codeEnv, all.names = TRUE)

        ## Does the package have a NAMESPACE file?  Note that when
        ## working on the sources we (currently?) cannot deal with the
        ## (experimental) alternative way of specifying the namespace.
        if(file.exists(file.path(dir, "NAMESPACE"))) {
            hasNamespace <- TRUE
            nsInfo <- parseNamespaceFile(basename(dir), dirname(dir))
            ## Determine exported objects.
            OK <- objectsInCode[objectsInCode %in% nsInfo$exports]
            for(p in nsInfo$exportPatterns)
                OK <- c(OK, grep(p, objectsInCode, value = TRUE))
            objectsInCode <- unique(OK)
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
    functionsInCode <-
        objectsInCode[sapply(objectsInCode,
                             function(f)
                             is.function(get(f, envir = codeEnv)))
                      == TRUE]

    ## Find all generic functions in the given package and (the current)
    ## base package.
    allGenerics <- character()
    envList <- list(codeEnv)
    if(!isBase) envList <- c(envList, list(as.environment(NULL)))
    for(env in envList) {
        ## Find all available S3 generics.
        objectsInEnv <- if(identical(env, codeEnv)) {
            ## We only want the exported ones anyway ...
            functionsInCode
        }
        else
            objects(envir = env, all.names = TRUE)
        allGenerics <-
            c(allGenerics,
              objectsInEnv[sapply(objectsInEnv,
                                  .isS3Generic,
                                  env)
                           == TRUE])
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
        methods <-
            functionsInCode[substr(functionsInCode, 1, nchar(name))
                            == name]
        ## </FIXME>
        methods <- methods[! methods %in% methodsStopList]
        if(hasNamespace) {
            ## Find registered methods for generic g.
            methods <- c(methods, nsS3methods[nsS3generics == g])
        }
        methods
    })
    allMethodsInPackage <- unlist(methodsInPackage)

    db <- if(!missing(package))
        Rddb(package, lib.loc = dirname(dir))
    else
        Rddb(dir = dir)

    db <- lapply(db,
                 function(f) paste(Rdpp(f), collapse = "\n"))
    names(db) <- dbNames <- sapply(db, getRdSection, "name")

    dbUsageTexts <- lapply(db, getRdSection, "usage")
    dbUsages <-
        lapply(dbUsageTexts,
               function(txt) {
                   methodRE <-
                       paste("(\\\\(S[34])?method",
                             "{([.[:alnum:]]*)}",
                             "{([.[:alnum:]]*)})",
                             sep = "")
                   txt <- gsub("\\\\l?dots", "...", txt)
                   txt <- gsub("\\\\%", "%", txt)
                   txt <- gsub(methodRE, "\"\\\\\\1\"", txt)
                   .parseTextAsMuchAsPossible(txt)
               })
    ind <- sapply(dbUsages,
                  function(x) !is.null(attr(x, "badLines")))
    badLines <- sapply(dbUsages[ind], attr, "badLines")

    badDocObjects <- list()

    for(docObj in dbNames) {

        ## Determine function names in the \usage.
        exprs <- dbUsages[[docObj]]
        exprs <- exprs[sapply(exprs, length) > 1]
        ## Ordinary functions.
        functions <-
            as.character(sapply(exprs,
                                function(e) as.character(e[[1]])))
        ## (Note that as.character(sapply(exprs, "[[", 1)) does not do
        ## what we want due to backquotifying.)
        ## Replacement functions.
        ind <- as.logical(sapply(exprs,
                                 .isCallFromReplacementFunctionUsage))
        if(any(ind)) {
            replaceFuns <-
                paste(sapply(exprs[ind],
                             function(e) as.character(e[[2]][[1]])),
                      "<-",
                      sep = "")
            functions <- c(functions, replaceFuns)
        }

        methodsWithFullName <-
            functions[functions %in% allMethodsInPackage]

        functions <- .transformS3methodMarkup(functions)

        methodsWithGeneric <-
            sapply(functions[functions %in% allGenerics],
                   function(g)
                   functions[functions %in% methodsInPackage[[g]]],
                   simplify = FALSE)

        if((length(methodsWithGeneric) > 0) ||
           (length(methodsWithFullName > 0)))
            badDocObjects[[docObj]] <-
                list(withGeneric  = methodsWithGeneric,
                     withFullName = methodsWithFullName)

    }

    attr(badDocObjects, "badLines") <- badLines
    class(badDocObjects) <- "checkDocStyle"
    badDocObjects
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
            writeLines(strwrap(paste(methodsWithFullName,
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
        .tryQuietly(load(file, envir = codeEnv))
        exprs <- lapply(ls(envir = codeEnv, all.names = TRUE),
                        function(f) {
                            f <- get(f, envir = codeEnv)
                            if(typeof(f) == "closure")
                                body(f)
                            else
                                NULL
                        })
        if(.isMethodsDispatchOn()) {
            ## Also check the code in S4 methods.
            ## This may find things twice if a setMethod() with a bad FF
            ## call is from inside a function (e.g., InitMethods()).
            for(f in methods::getGenerics(codeEnv)) {
                meths <-
                    methods::linearizeMlist(methods::getMethodsMetaData(f, codeEnv))
                exprs <-
                    c(exprs,
                      lapply(methods::slot(meths, "methods"), body))
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

### * checkS3methods

checkS3methods <-
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

        objectsInCode <- objects(envir = codeEnv, all.names = TRUE)

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
            S3reg <- nsS3methods[! nsS3methods %in% objectsInCode]
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

        objectsInCode <- objects(envir = codeEnv, all.names = TRUE)

        ## Does the package have a NAMESPACE file?  Note that when
        ## working on the sources we (currently?) cannot deal with the
        ## (experimental) alternative way of specifying the namespace.
        if(file.exists(file.path(dir, "NAMESPACE"))) {
            hasNamespace <- TRUE
            nsInfo <- parseNamespaceFile(basename(dir), dirname(dir))
            ## Determine exported objects.
            OK <- objectsInCode[objectsInCode %in% nsInfo$exports]
            for(p in nsInfo$exportPatterns)
                OK <- c(OK, grep(p, objectsInCode, value = TRUE))
            objectsInCode <- unique(OK)
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
    functionsInCode <-
        objectsInCode[sapply(objectsInCode,
                             function(f)
                             is.function(get(f, envir = codeEnv)))
                      == TRUE]

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
        objectsInEnv <- if(identical(env, codeEnv)) {
            ## We only want the exported ones anyway ...
            functionsInCode
        }
        else
            objects(envir = env, all.names = TRUE)
        S3generics <-
            objectsInEnv[sapply(objectsInEnv, .isS3Generic, env)
                         == TRUE]

        ## For base, also add the internal S3 generics which are not
        ## .Primitive (as checkArgs() does not deal with these).
        if(identical(env, as.environment(NULL))) {
            internalS3generics <- .getInternalS3generics()
            internalS3generics <-
                internalS3generics[sapply(internalS3generics,
                                          .isPrimitive, NULL)
                                   == FALSE]
            S3generics <- c(S3generics, internalS3generics)
        }

        for(g in S3generics) {
            ## Find all methods in functionsInCode for S3 generic g.
            ## <FIXME>
            ## We should really determine the name g dispatches for, see
            ## a current version of methods() [2003-07-07].  (Care is
            ## needed for internal generics and group generics.)
            ## Matching via grep() is tricky with e.g. a '$' in the name
            ## of the generic function ... hence substr().
            name <- paste(g, ".", sep = "")
            methods <-
                functionsInCode[substr(functionsInCode, 1, nchar(name))
                                == name]
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

    class(badMethods) <- "checkS3methods"
    badMethods
}

print.checkS3methods <-
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

    objectsInCode <- objects(envir = codeEnv, all.names = TRUE)
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
        objectsInCode <-
            objectsInCode[! objectsInCode %in% nsS3methods]
    }

    replaceFuns <-
        c(replaceFuns, grep("<-", objectsInCode, value = TRUE))

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

    if(.isMethodsDispatchOn()) { 
        S4generics <- methods::getGenerics(codeEnv)
        ## Assume that the ones with names ending in '<-' are always
        ## replacement functions.
        S4generics <- grep("<-$", S4generics, value = TRUE)
        badS4ReplaceMethods <-
            sapply(S4generics,
                   function(f) {
                       meths <- methods::linearizeMlist(methods::getMethodsMetaData(f, codeEnv))
                       ind <- which(sapply(methods::slot(meths,
                                                         "methods"),
                                           .checkLastFormalArg)
                                    == FALSE)
                       if(!length(ind))
                           character()
                       else {
                           sigs <-
                               sapply(methods::slot(meths,
                                                    "classes")[ind],
                                      paste, collapse = ",")
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
    if(length(x) > 0) .prettyPrint(unclass(x))
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

    findTnFInCode <- function(file, txt) {
        ## If 'txt' is given, it contains the extracted examples from
        ## the R documentation file 'file'.  Otherwise, 'file' gives a
        ## file with (just) R code.
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
        if(missing(txt)) {
            exprs <- try(parse(file = file, n = -1))
            if(inherits(exprs, "try-error"))
                stop(paste("parse error in file", sQuote(file)))
        }
        else {
            exprs <- try(parse(text = txt))
            if(inherits(exprs, "try-error"))
                stop(paste("parse error in examples from file",
                           sQuote(file)))
        }
        for(i in seq(along = exprs))
            findBadExprs(exprs[[i]], NULL)
        matches
    }

    badExprs <- list()
    for(file in codeFiles) {
        exprs <- findTnFInCode(file)
        if(length(exprs) > 0) {
            exprs <- list(exprs)
            names(exprs) <- file
            badExprs <- c(badExprs, exprs)
        }
    }
    for(file in docsFiles) {
        txt <- paste(Rdpp(readLines(file)), collapse = "\n")
        txt <- .getRdExampleCode(txt)
        exprs <- findTnFInCode(file, txt)
        if(length(exprs) > 0) {
            exprs <- list(exprs)
            names(exprs) <- file
            badExprs <- c(badExprs, exprs)
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

### * as.alist.call

as.alist.call <-
function(x)
{
    y <- as.list(x)
    ind <- if(is.null(names(y)))
        seq(along = y)
    else
        which(names(y) == "")
    if(any(ind)) {
        names(y)[ind] <- as.character(y[ind])
        y[ind] <- rep.int(list(alist(irrelevant = )[[1]]), length(ind))
    }
    y
}

### * as.alist.symbol

as.alist.symbol <-
function(x)
{
    as.alist.call(call(as.character(x)))
}

### * .argNamesFromCall

.argNamesFromCall <-
function(x)
{
    y <- as.character(x)
    if(!is.null(nx <- names(x))) {
        ind <- which(nx != "")
        y[ind] <- nx[ind]
    }
    y
}

### * .functionsToBeIgnoredFromUsage

.functionsToBeIgnoredFromUsage <-
function(packageName)
{
    c("<-", "=",
      if(packageName == "base")
      c("(", "{", "function", "if", "for", "while", "repeat", "?"),
      if(packageName == "methods") "@")
}

### * .functionsWithNoUsefulS3methodMarkup

.functionsWithNoUsefulS3methodMarkup <-
    ## Currently there is no useful markup for S3 Ops group methods and
    ## S3 methods for subscripting and subassigning.
    c("+", "-", "*", "/", "^", "<", ">", "<=", ">=", "!=",
      "==", "%%", "%/%", "&", "|", "!",
      "[", "[[", "$", "[<-", "[[<-", "$<-")


### * .isCallFromReplacementFunctionUsage

.isCallFromReplacementFunctionUsage <-
function(x)
{
    ((length(x) == 3)
     && (identical(x[[1]], as.symbol("<-")))
     && (length(x[[2]]) > 1)
     && is.symbol(x[[3]]))
}

### * .parseTextAsMuchAsPossible

.parseTextAsMuchAsPossible <-
function(txt)
{
    exprs <- try(parse(text = txt), silent = TRUE)
    if(!inherits(exprs, "try-error")) return(exprs)
    exprs <- expression()
    lines <- unlist(strsplit(txt, "\n"))
    badLines <- character()
    while((n <- length(lines)) > 0) {
        i <- 1; txt <- lines[1]
        while(inherits(yy <- try(parse(text = txt), silent = TRUE),
                       "try-error")
              && (i < n)) {
            i <- i + 1; txt <- paste(txt, lines[i], collapse = "\n")
        }
        if(inherits(yy, "try-error")) {
            badLines <- c(badLines, lines[1])
            lines <- lines[-1]
        }
        else {
            exprs <- c(exprs, yy)
            lines <- lines[-seq(length = i)]
        }
    }
    attr(exprs, "badLines") <- badLines
    exprs
}

### * .prettyPrint

.prettyPrint <-
function(x)
{
    writeLines(strwrap(paste(x, collapse = " "),
                       indent = 2, exdent = 2))
}

### * .transformS3methodMarkup

.transformS3methodMarkup <-
function(x)
{
    ## Note how we deal with S3 replacement methods found.
    ## These come out named "\method{GENERIC}{CLASS}<-" which we
    ## need to turn into 'GENERIC<-.CLASS'.
    sub("\\\\(S3)?method{([.[:alnum:]]*)}{([.[:alnum:]]*)}(<-)?",
        "\\2\\4.\\3",
        x)
}

### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "### [*]+" ***
### End: ***
