### Internal utility functions.

### * sQuote

sQuote <- function(s) paste("'", s, "'", sep = "")

### * .convertFilePathToAbsolute

.convertFilePathToAbsolute <-
function(path)
{
    ## Turn a possibly relative file path absolute, performing tilde
    ## expansion if necessary.
    ## Seems the only way we can do this is 'temporarily' change the
    ## working dir and see where this takes us.
    if(!file.exists(epath <- path.expand(path)))
        stop(paste("file", sQuote(path), "does not exist"))
    cwd <- getwd()
    on.exit(setwd(cwd))
    if(.fileTest("-d", epath)) {
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

### * .delimMatch

.delimMatch <-
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

### * .fileTest

.fileTest <-
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

### * .filePathSansExt

.filePathSansExt <-
function(x)
{
    ## Return the file paths without extensions.
    ## (Only purely alphanumeric extensions are recognized.)
    sub("\\.[[:alpha:]]+$", "", x)
}

### * .isS3Generic

.isS3Generic <-
function(fname, envir = NULL)
{
    ## Determine whether object named 'fname' in environment 'envir' is
    ## (to be considered) an S3 generic function.  In most cases, these
    ## just call UseMethod() in their body, so we test for this after
    ## possibly stripping braces.  This fails when e.g. it is attempted
    ## to dispatch on data.class, hence we need to hard-code a few known
    ## exceptions.
    ## <FIXME>
    ## This is not good enough for generics which dispatch in C code
    ## (base only?).
    ## We should also add group methods.
    ## </FIXME>
    f <- get(fname, envir = envir)
    if(!is.function(f)) return(FALSE)
    if(fname %in% c("as.data.frame", "plot")) return(TRUE)
    e <- body(f)
    while(is.call(e) && (length(e) > 1) && (e[[1]] == as.name("{")))
        e <- e[[2]]
    is.call(e) && (e[[1]] == as.name("UseMethod"))
}

### * .listFilesWithExts

.listFilesWithExts <-
function(dir, exts, path = TRUE)
{
    ## Return the paths or names of the files in @code{dir} with
    ## extension in @code{exts}.
    files <- list.files(dir)
    files <- files[sub(".*\\.", "", files) %in% exts]
    if(path)
        files <- if(length(files) > 0)
            file.path(dir, files)
        else
            character(0)
    files
}

### * .listFilesWithType

.listFilesWithType <-
function(dir, type)
{
    ## Return a character vector with the paths of the files in
    ## @code{dir} of type @code{type} (as in .makeFileExts()).
    ## When listing R code and documentation files, files in OS-specific
    ## subdirectories are included if present.
    exts <- .makeFileExts(type)
    files <- .listFilesWithExts(dir, exts)
    if(type %in% c("code", "docs")) {
        OSdir <- file.path(dir, .Platform$OS)
        if(.fileTest("-d", OSdir))
            files <- c(files, .listFilesWithExts(OSdir, exts))
    }
    files
}

### * .loadPackageQuietly

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

### * .makeFileExts

.makeFileExts <-
function(type = c("code", "data", "demo", "docs", "vignette"))
{
    ## Return a character vector with the possible/recognized file
    ## extensions for a given file type.
    switch(type,
           code = c("R", "r", "S", "s", "q"),
           data = c("R", "r",
                    "RData", "rdata", "rda",
                    "TXT", "txt", "tab", "CSV", "csv"),
           demo = "R",
           docs = c("Rd", "rd"),
           vignette = c(outer(c("R", "r", "S", "s"), c("nw", "tex"),
                              paste, sep = "")))
}

### * .makeS3MethodsStopList

.makeS3MethodsStopList <-
function(package)
{
    ## Return a character vector with the names of the functions in
    ## @code{package} which 'look' like S3 methods, but are not.
    switch(package,
	   base = c("boxplot.stats",
	   "close.screen", "close.socket",
	   "flush.console",
	   "format.char", "format.info", "format.pval",
	   "influence.measures",
	   "plot.new", "plot.window", "plot.xy", "print.coefmat",
	   "split.screen",
	   "update.packages"),
	   quadprog = c("solve.QP", "solve.QP.compact"),
	   sm = "print.graph",
	   ts = "lag.plot",
	   character(0))
}

### * .sourceAssignments

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
    exprs <- parse(n = -1, file = file)
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
