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
    isBase <- basename(dir) == "base"

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
