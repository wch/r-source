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
    ## Note that file.exists() only tests existence ('test -e' on some
    ## systems), and that our '-f' tests for existence and not being a
    ## directory (the GNU variant tests for being a regular file).
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
function(dir, exts, all.files = FALSE, full.names = TRUE)
{
    ## Return the paths or names of the files in @code{dir} with
    ## extension in @code{exts}.
    files <- list.files(dir, all.files = all.files)
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
function(dir, type, all.files = FALSE, full.names = TRUE)
{
    ## Return a character vector with the paths of the files in
    ## @code{dir} of type @code{type} (as in .makeFileExts()).
    ## When listing R code and documentation files, files in OS-specific
    ## subdirectories are included if present.
    exts <- .makeFileExts(type)
    files <-
        listFilesWithExts(dir, exts, all.files = all.files,
                          full.names = full.names)
    
    if(type %in% c("code", "docs")) {
        OSdir <- file.path(dir, .Platform$OS)
        if(fileTest("-d", OSdir)) {
            OSfiles <-
                listFilesWithExts(OSdir, exts, all.files = all.files,
                                  full.names = FALSE)
            OSfiles <-
                file.path(if(full.names) OSdir else .Platform$OS,
                          OSfiles)
            files <- c(files, OSfiles)
        }
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


### * LaTeX utilities

### ** texi2dvi

texi2dvi <- function(file, pdf = FALSE, clean = TRUE,
                     quiet = TRUE, texi2dvi = getOption("texi2dvi")) 
{
    ## run texi2dvi on a file
    
    if(pdf) pdf <- "--pdf" else pdf <- ""
    if(clean) clean <- "--clean" else clean <- ""
    if(quiet) quiet <- "--quiet" else quiet <- ""
    if(is.null(texi2dvi)) {
        if(file.exists(file.path(R.home(), "bin", "texi2dvi")))
            texi2dvi <- file.path(R.home(), "bin", "texi2dvi")
        else
            texi2dvi <- "texi2dvi"
    }
    
    yy <- system(paste(texi2dvi, quiet, pdf, clean, file))
    if(yy > 0) stop(paste("running texi2dvi on", file, "failed"))
}



### * Internal utility functions.

### ** .getInternalS3generics

.getInternalS3generics <-
function()
{
    ## Get the list of R internal S3 generics (via DispatchOrEval(),
    ## cf. zMethods.Rd).
    c("[", "[[", "$", "[<-", "[[<-", "$<-", "length", "dimnames<-",
      "dimnames", "dim<-", "dim", "c", "unlist", "as.character",
      "as.vector", "is.array", "is.atomic", "is.call", "is.character",
      "is.complex", "is.double", "is.environment", "is.function",
      "is.integer", "is.language", "is.logical", "is.list", "is.matrix",
      "is.na", "is.nan", "is.null", "is.numeric", "is.object",
      "is.pairlist", "is.recursive", "is.single", "is.symbol")
}

### ** .getNamespaceS3methodsList

.getNamespaceS3methodsList <-
function(nsInfo)
{
    ## Get the list of the registered S3 methods for an 'nsInfo' object
    ## returned by parseNamespaceFile().  Each element of the list is a
    ## character vector of length 3 with the names of the generic, class
    ## and method (as a function).
    lapply(nsInfo$S3methods,
           function(spec) {
               if(length(spec) == 2)
                   spec <-
                       c(spec, paste(spec, collapse = "."))
               spec
           })
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
    .tryQuietly({
        pos <- match(paste("package", package, sep = ":"), search())
        if(!is.na(pos))
            detach(pos = pos)
        library(package, lib.loc = lib.loc, character.only = TRUE,
                verbose = FALSE)
    })
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
           demo = c("R", "r"),
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
    stopList <-
        list(base = c("anova.lmlist",
             "boxplot.stats",
             "close.screen", "close.socket",
             "fitted.values",
             "flush.console",
             "format.char", "format.info", "format.pval",
             "influence.measures",
             "kappa.tri",
             "plot.design", "plot.new", "plot.window", "plot.xy",
             "print.atomic", "print.coefmat",
             "rep.int",
             "split.screen",
             "update.packages"),
             Hmisc = "t.test.cluster",
             MASS = c("frequency.polygon", "hist.FD", "hist.scott"),
             XML = "text.SAX",
             ctest = "t.test",
             quadprog = c("solve.QP", "solve.QP.compact"),
             reposTools = "update.packages2",
             sm = "print.graph",
             ts = "lag.plot")
    if(is.null(package)) return(unlist(stopList))
    thisPkg <- stopList[[package]]
    if(!length(thisPkg)) character(0) else thisPkg
}

### ** .packageApply

.packageApply <-
function(packages = NULL, FUN, ...)
{
    ## Apply FUN and extra '...' args to all given packages.
    ## The default corresponds to all installed packages with high
    ## priority.
    if(is.null(packages))
        packages <- unique(installed.packages(priority = "high")[ , 1])
    out <- lapply(packages, FUN, ...)
    names(out) <- packages
    out
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
    assignmentSymbolLM <- as.symbol("<-")
    assignmentSymbolEq <- as.symbol("=")
    exprs <- try(parse(n = -1, file = file))
    if(length(exprs) == 0)
        return(invisible())
    for(e in exprs) {
        if(e[[1]] == assignmentSymbolLM || e[[1]] == assignmentSymbolEq)
            eval(e, envir)
    }
    invisible()
}

### ** .tryQuietly

.tryQuietly <-
function(expr)
{
    ## Try to run an expression, suppressing all 'output'.  In case of
    ## failure, stop with the error message.
    outConn <- file(open = "w")         # anonymous tempfile
    sink(outConn, type = "output")
    sink(outConn, type = "message")
    yy <- try(expr, silent = TRUE)
    sink(type = "message")
    sink(type = "output")
    close(outConn)
    if(inherits(yy, "try-error"))
        stop(yy)
    yy
}

### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "### [*]+" ***
### End: ***
