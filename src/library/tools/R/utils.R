### * File utilities.

### ** filePathAsAbsolute

filePathAsAbsolute <-
function(x)
{
    ## Turn a possibly relative file path absolute, performing tilde
    ## expansion if necessary.
    ## Seems the only way we can do this is 'temporarily' change the
    ## working dir and see where this takes us.
    if(!file.exists(epath <- path.expand(x)))
        stop(paste("file", sQuote(x), "does not exist"))
    cwd <- getwd()
    on.exit(setwd(cwd))
    if(fileTest("-d", epath)) {
        ## Combining dirname and basename does not work for e.g. '.' or
        ## '..' on Unix ...
        setwd(epath)
        getwd()
    }
    else {
        setwd(dirname(epath))
        file.path(getwd(), basename(epath))
    }
}

### ** filePathSansExt

filePathSansExt <-
function(x)
{
    ## Return the file paths without extensions.
    ## (Only purely alphanumeric extensions are recognized.)
    sub("\\.[[:alpha:]]+$", "", x)
}

### ** fileTest

fileTest <-
function(op, x, y)
{
    ## Provide shell-style '-f', '-d', '-nt' and '-ot' tests.
    ## Note that file.exists() only tests existence ('test -e') but not
    ## for being a regular file ('test -f').  We cannot really do this,
    ## so our '-f' tests for existence and not being a directory.
    ## Note: vectorized in x and y.
    switch(op,
           "-f" = !is.na(isdir <- file.info(x)$isdir) & !isdir,
           "-d" = !is.na(isdir <- file.info(x)$isdir) & isdir,
           "-nt" = (!is.na(mt.x <- file.info(x)$mtime)
                    & !is.na(mt.y <- file.info(y)$mtime)
                    & (mt.x > mt.y)),
           "-ot" = (!is.na(mt.x <- file.info(x)$mtime)
                    & !is.na(mt.y <- file.info(y)$mtime)
                    & (mt.x < mt.y)),
           stop(paste("test", sQuote(op), "is not available")))
}

### ** listFilesWithExts

listFilesWithExts <-
function(dir, exts, full.names = TRUE)
{
    ## Return the paths or names of the files in @code{dir} with
    ## extension in @code{exts}.
    files <- list.files(dir)
    files <- files[sub(".*\\.", "", files) %in% exts]
    if(full.names)
        files <- if(length(files) > 0)
            file.path(dir, files)
        else
            character(0)
    files
}

### ** listFilesWithType

listFilesWithType <-
function(dir, type)
{
    ## Return a character vector with the paths of the files in
    ## @code{dir} of type @code{type} (as in .makeFileExts()).
    ## When listing R code and documentation files, files in OS-specific
    ## subdirectories are included if present.
    exts <- .makeFileExts(type)
    files <- listFilesWithExts(dir, exts)
    if(type %in% c("code", "docs")) {
        OSdir <- file.path(dir, .Platform$OS)
        if(fileTest("-d", OSdir))
            files <- c(files, listFilesWithExts(OSdir, exts))
    }
    files
}

### * Text utilities.

### ** delimMatch

delimMatch <-
function(x, delim = c("\{", "\}"), syntax = "Rd")
{
    if(!is.character(x))
        stop("argument x must be a character vector")
    if((length(delim) != 2) || any(nchar(delim) != 1))
        stop("incorrect value for delim")
    if(syntax != "Rd")
        stop("only Rd syntax is currently supported")

    .Call("delim_match", x, delim, PACKAGE = "tools")
}

### * Internal utility functions.

### ** .getNamespaceS3methodNames

.getNamespaceS3methodNames <-
function(ns, dir, nsInfo)
{
    ## Get the names of the registered S3 methods for a namespace 'ns',
    ## or a @file{NAMESPACE} file in directory 'dir', or an 'nsInfo'
    ## object returned by parseNamespaceFile().  (Note that when doing
    ## QC on non-installed packages with a @file{NAMESPACE} file, we
    ## parse the @file{NAMESPACE} file anyway to determine the exports.)
    S3methods <- if(!missing(ns))
        sapply(getNamespaceInfo(ns, "S3methods"), "[[", 3)
    else {
        if(!missing(dir))
            nsInfo <- parseNamespaceFile(basename(dir), dirname(dir))
        sapply(nsInfo$S3methods,
               function(x) {
                   if(length(x) > 2)
                       x[3]
                   else
                       paste(x, collapse = ".")
               })
    }
    if(!length(S3methods)) S3methods <- character(0) # list() if empty
    S3methods
}

### ** .isPrimitive

.isPrimitive <-
function(fname, envir)
{
    ## Determine whether object named 'fname' found in environment
    ## 'envir' is a primitive function.
    f <- get(fname, envir = envir, inherits = FALSE)
    is.function(f) && any(grep("^\\.Primitive", deparse(f)))
}

### ** .isS3Generic

.isS3Generic <-
function(fname, envir, mustMatch = TRUE)
{
    ## Determine whether object named 'fname' found in environment
    ## 'envir' is (to be considered) an S3 generic function.  Note,
    ## found *in* not found *from*, so envir does not have a default.
    ##
    ## If it is, does it despatch methods of fname?  We need that to
    ## look for possible methods as functions named fname.* ....
    ##
    ## Provided by LT with the following comments:
    ##
    ## This is tricky.  Figuring out what could possibly dispatch
    ## successfully some of the time is pretty much impossible given R's
    ## semantics.  Something containing a literal call to UseMethod is
    ## too broad in the sense that a UseMethod call in a local function
    ## doesn't produce a dispatch on the outer function ...
    ##
    ## If we use something like: a generic has to be
    ##      function(e) <UME>  # UME = UseMethod Expression
    ## with
    ##	    <UME> = UseMethod(...) |
    ##             if (...) <UME> [else ...] |
    ##             if (...) ... else <UME>
    ##             { ... <UME> ... }
    ## then a recognizer for UME might be as follows.

    f <- get(fname, envir = envir, inherits = FALSE)
    if(!is.function(f)) return(FALSE)
    isUMEbrace <- function(e) {
        for (ee in as.list(e[-1])) if (nchar(res <- isUME(ee))) return(res)
        ""
    }
    isUMEif <- function(e) {
        if (length(e) == 3) isUME(e[[3]])
        else {
            if (nchar(res <- isUME(e[[3]]))) res
            else if (nchar(res <- isUME(e[[4]]))) res
            else ""
        }

    }
    isUME <- function(e) {
        if (is.call(e) && (is.name(e[[1]]) || is.character(e[[1]]))) {
            switch(as.character(e[[1]]),
                   UseMethod = as.character(e[[2]]),
                   "{" = isUMEbrace(e),
                   "if" = isUMEif(e),
                   "")
        } else ""
    }
    res <- isUME(body(f))
    if(mustMatch) res == fname else nchar(res) > 0
}

### ** .loadPackageQuietly

.loadPackageQuietly <-
function(package, lib.loc)
{
    ## Load (reload if already loaded) @code{package} from
    ## @code{lib.loc}, capturing all output and messages.  All QC
    ## functions use this for loading packages because R CMD check
    ## interprets all output as indicating a problem.
    outConn <- file(open = "w")         # anonymous tempfile
    sink(outConn, type = "output")
    sink(outConn, type = "message")
    yy <- try({
        pos <- match(paste("package", package, sep = ":"), search())
        if(!is.na(pos))
            detach(pos = pos)
        library(package, lib.loc = lib.loc, character.only = TRUE,
                verbose = FALSE)
    })
    sink(type = "message")
    sink(type = "output")
    close(outConn)
    if(inherits(yy, "try-error"))
        stop(yy)
}

### ** .makeFileExts

.makeFileExts <-
function(type = c("code", "data", "demo", "docs", "vignette"))
{
    ## Return a character vector with the possible/recognized file
    ## extensions for a given file type.
    switch(type,
           code = c("R", "r", "S", "s", "q"),
           ## Keep in sync with the order given in base's data.Rd.
           data = c("R", "r",
                    "RData", "rdata", "rda",
                    "tab", "txt", "TXT", "csv", "CSV"),
           demo = "R",
           docs = c("Rd", "rd"),
           vignette = c(outer(c("R", "r", "S", "s"), c("nw", "tex"),
                              paste, sep = "")))
}

### ** .makeS3MethodsStopList

.makeS3MethodsStopList <-
function(package)
{
    ## Return a character vector with the names of the functions in
    ## @code{package} which 'look' like S3 methods, but are not.
    ## using package=NULL returns all known examples
    stopList <- list(base = c("boxplot.stats",
	   "close.screen", "close.socket",
	   "flush.console",
	   "format.char", "format.info", "format.pval",
	   "influence.measures",
	   "plot.new", "plot.window", "plot.xy", "print.coefmat",
           "rep.int",
	   "split.screen",
	   "update.packages"),
           XML = "text.SAX",
	   quadprog = c("solve.QP", "solve.QP.compact"),
	   sm = "print.graph",
	   ts = "lag.plot")
    if(is.null(package)) return(unlist(stopList))
    thisPkg <- stopList[[package]]
    if(!length(thisPkg)) character(0) else thisPkg
}

### ** .sourceAssignments

.sourceAssignments <-
function(file, envir)
{
    ## Read and parse expressions from @code{file}, and then
    ## successively evaluate the top-level assignments in @code{envir}.
    ## Apart from only dealing with assignments, basically does the same
    ## as @code{sys.source(file, envir, keep.source = FALSE)}.
    oop <- options(keep.source = FALSE)
    on.exit(options(oop))
    assignmentSymbol <- as.name("<-")
    exprs <- try(parse(n = -1, file = file))
    if(length(exprs) == 0)
        return(invisible())
    for(e in exprs) {
        if(e[[1]] == assignmentSymbol)
            yy <- eval(e, envir)
    }
    invisible()
}

### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "### [*]+" ***
### End: ***
