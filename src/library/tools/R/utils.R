### * File utilities.

### ** file_path_as_absolute

file_path_as_absolute <-
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
    if(file_test("-d", epath)) {
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

### ** file_path_sans_ext

file_path_sans_ext <-
function(x)
{
    ## Return the file paths without extensions.
    ## (Only purely alphanumeric extensions are recognized.)
    sub("\\.[[:alpha:]]+$", "", x)
}

### ** file_test

file_test <-
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

### ** list_files_with_exts

list_files_with_exts <-
function(dir, exts, all.files = FALSE, full.names = TRUE)
{
    ## Return the paths or names of the files in @code{dir} with
    ## extension in @code{exts}.
    ## Might be in a zipped dir on Windows
    if(file.exists(file.path(dir, "filelist")) &&
       any(file.exists(file.path(dir, c("Rdata.zip", "Rex.zip", "Rhelp.zip")))))
    {
        files <- readLines(file.path(dir, "filelist"))
        if(!all.files)
            files <- grep("^[^.]", files, value = TRUE)
    } else {
        files <- list.files(dir, all.files = all.files)
    }
    ## does not cope with exts with '.' in.
    ## files <- files[sub(".*\\.", "", files) %in% exts]
    patt <- paste("\\.(", paste(exts, collapse="|"), ")$", sep = "")
    files <- grep(patt, files, value = TRUE)
    if(full.names)
        files <- if(length(files) > 0)
            file.path(dir, files)
        else
            character(0)
    files
}

### ** list_files_with_type

list_files_with_type <-
function(dir, type, all.files = FALSE, full.names = TRUE)
{
    ## Return a character vector with the paths of the files in
    ## @code{dir} of type @code{type} (as in .make_file_exts()).
    ## When listing R code and documentation files, files in OS-specific
    ## subdirectories are included if present.
    exts <- .make_file_exts(type)
    files <-
        list_files_with_exts(dir, exts, all.files = all.files,
                             full.names = full.names)

    if(type %in% c("code", "docs")) {
        OSdir <- file.path(dir, .OStype())
        if(file_test("-d", OSdir)) {
            OSfiles <-
                list_files_with_exts(OSdir, exts, all.files = all.files,
                                     full.names = FALSE)
            OSfiles <-
                file.path(if(full.names) OSdir else .OStype(),
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
        stop(.wrong_args("x", "must be a character vector"))
    if((length(delim) != 2) || any(nchar(delim) != 1))
        stop(.wrong_args("delim", "must specify two single characters"))
    if(syntax != "Rd")
        stop("only Rd syntax is currently supported")

    .Call("delim_match", x, delim, PACKAGE = "tools")
}


### * LaTeX utilities

### ** texi2dvi

texi2dvi <-
function(file, pdf = FALSE, clean = FALSE,
         quiet = TRUE, texi2dvi = getOption("texi2dvi"))
{
    ## Run texi2dvi on a file.

    if(pdf) pdf <- "--pdf" else pdf <- ""
    if(clean) clean <- "--clean" else clean <- ""
    if(quiet) quiet <- "--quiet" else quiet <- ""
    if(is.null(texi2dvi)) {
        if(file.exists(file.path(R.home(), "bin", "texi2dvi")))
            texi2dvi <- file.path(R.home(), "bin", "texi2dvi")
        else
            texi2dvi <- "texi2dvi"
    }

    yy <- system(paste(shQuote(texi2dvi),
                       quiet, pdf, clean,
                       shQuote(file)))
    if(yy > 0) stop(paste("running texi2dvi on", file, "failed"))
}


### * Internal utility functions.

### ** %w/o%

"%w/o%" <-
function(x, y)
{
    ## x without y, as in the examples of ?match.
    x[!x %in% y]
}

### ** .OStype

.OStype <-
function()
{
    OS <- Sys.getenv("R_OSTYPE")
    if(nchar(OS)) OS else .Platform$OS.type
}

### ** .capture_output_from_print

.capture_output_from_print <-
function(x, ...)
{
    ## Better to provide a simple variant of utils::capture.output()
    ## ourselves (so that bootstrapping R only needs base and tools).
    file <- textConnection("out", "w", local = TRUE)
    sink(file)
    on.exit({ sink(); close(file) })
    print(x, ...)
    out
}

### ** .get_internal_S3_generics

.get_internal_S3_generics <-
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
      "is.pairlist", "is.recursive", "is.single", "is.symbol",
      ## and also the members of the group generics from groupGeneric.Rd
      "abs", "sign", "sqrt", "floor", "ceiling", "trunc", "round", "signif",
      "exp", "log", "cos", "sin", "tan", "acos", "asin", "atan",
      "cosh", "sinh", "tanh", "acosh", "asinh", "atanh",
      "lgamma", "gamma", "gammaCody", "digamma", "trigamma",
      "tetragamma", "pentagamma", "cumsum", "cumprod", "cummax", "cummin",
      "+", "-", "*", "/", "^", "%%", "%/%", "&", "|", "!", "==", "!=",
      "<", "<=", ">=", ">",
      "all", "any", "sum", "prod", "max", "min", "range",
      "Arg", "Conj", "Im", "Mod", "Re"
      )
}

### ** .get_namespace_package_depends

.get_namespace_package_depends <-
function(dir)
{
    nsInfo <- parseNamespaceFile(basename(dir), dirname(dir))
    depends <- c(sapply(nsInfo$imports, "[[", 1),
                 sapply(nsInfo$importClasses, "[[", 1),
                 sapply(nsInfo$importMethods, "[[", 1))
    unique(sort(as.character(depends)))
}

### ** .get_namespace_S3_methods_db

.get_namespace_S3_methods_db <-
function(nsInfo)
{
    ## Get the registered S3 methods for an 'nsInfo' object returned by
    ## parseNamespaceFile(), as a 3-column character matrix with the
    ## names of the generic, class and method (as a function).
    S3_methods_list <- nsInfo$S3methods
    if(!length(S3_methods_list)) return(matrix(character(), nc = 3))
    idx <- is.na(S3_methods_list[, 3])
    S3_methods_list[idx, 3] <-
        paste(S3_methods_list[idx, 1],
              S3_methods_list[idx, 2],
              sep = ".")
    S3_methods_list
}

### ** .get_S3_group_generics

.get_S3_group_generics <-
function()
    c("Ops", "Math", "Summary", "Complex")

### ** .get_standard_Rd_keywords

.get_standard_Rd_keywords <-
function()
{
    lines <- readLines(file.path(R.home(), "doc", "KEYWORDS.db"))
    lines <- grep("^.*\\\|([^:]*):.*", lines, value = TRUE)
    lines <- sub("^.*\\\|([^:]*):.*", "\\1", lines)
    lines
}

### ** .get_standard_package_names

.get_standard_package_names <-
function()
{
    lines <- readLines(file.path(R.home(), "share", "make", "vars.mk"))
    lines <- grep("^R_PKGS_[[:upper:]]+ *=", lines, value = TRUE)
    out <- strsplit(sub("^R_PKGS_[[:upper:]]+ *= *", "", lines), " +")
    names(out) <-
        tolower(sub("^R_PKGS_([[:upper:]]+) *=.*", "\\1", lines))
    out
}

### ** .is_primitive

.is_primitive <-
function(fname, envir)
{
    ## Determine whether object named 'fname' found in environment
    ## 'envir' is a primitive function.
    f <- get(fname, envir = envir, inherits = FALSE)
    is.function(f) && any(grep("^\\.Primitive", deparse(f)))
}

### ** .is_S3_generic

.is_S3_generic <-
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

### ** .load_package_quietly

.load_package_quietly <-
function(package, lib.loc)
{
    ## Load (reload if already loaded) @code{package} from
    ## @code{lib.loc}, capturing all output and messages.  All QC
    ## functions use this for loading packages because R CMD check
    ## interprets all output as indicating a problem.
    .try_quietly({
        pos <- match(paste("package", package, sep = ":"), search())
        if(!is.na(pos))
            detach(pos = pos)
        library(package, lib.loc = lib.loc, character.only = TRUE,
                verbose = FALSE)
    })
}

### ** .make_file_exts

.make_file_exts <-
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
           docs = c("Rd", "rd", "Rd.gz", "rd.gz"),
           vignette = c(outer(c("R", "r", "S", "s"), c("nw", "tex"),
                              paste, sep = "")))
}

### ** .make_S3_methods_stop_list

.make_S3_methods_stop_list <-
function(package)
{
    ## Return a character vector with the names of the functions in
    ## @code{package} which 'look' like S3 methods, but are not.
    ## Using package=NULL returns all known examples

    ## round.POSIXt is a method for S3 and S4 group generics with
    ## deliberately different arg names.
    stopList <-
        list(base = c("all.equal", "all.names", "all.vars",
             "format.char", "format.info", "format.pval",
             "kappa.tri",
             "max.col",
             "print.atomic", "print.coefmat",
             "rep.int", "round.POSIXt"),
             Hmisc = "t.test.cluster",
             HyperbolicDist = "log.hist",
             MASS = c("frequency.polygon",
             "gamma.dispersion", "gamma.shape",
             "hist.FD", "hist.scott"),
             XML = "text.SAX",
             car = "scatterplot.matrix",
             graphics = c("boxplot.stats", "close.screen",
             "plot.design", "plot.new", "plot.window", "plot.xy",
             "split.screen"),
             hier.part = "all.regs",
             quadprog = c("solve.QP", "solve.QP.compact"),
             reposTools = "update.packages2",
             sm = "print.graph",
             stats = c("anova.lmlist", "fitted.values", "lag.plot",
             "influence.measures", "t.test"),
             utils = c("close.socket", "flush.console",
             "update.packages")
             )
    if(is.null(package)) return(unlist(stopList))
    thisPkg <- stopList[[package]]
    if(!length(thisPkg)) character(0) else thisPkg
}

### ** .package_apply

.package_apply <-
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

### ** .read_Rd_lines_quietly

.read_Rd_lines_quietly <-
function(con)
{
    ## Read lines from a connection to an Rd file, trying to suppress
    ## "incomplete final line found by readLines" warnings.
    if(is.character(con)) {
        con <- if(length(grep("\\.gz$", con))) gzfile(con, "r") else file(con, "r")
        on.exit(close(con))
    }
    .try_quietly(readLines(con))
}

### ** .read_description

.read_description <-
function(dfile)
{
    ## Try reading in package metadata from a DESCRIPTION file.
    ## (Never clear whether this should work on the path of the file
    ## itself, or on that of the directory containing it.)
    ## <NOTE>
    ## As we do not have character "frames", we return a named character
    ## vector.
    ## </NOTE>
    if(!file_test("-f", dfile))
        stop(paste("file", sQuote(dfile), "does not exist"))
    db <- try(read.dcf(dfile)[1, ], silent = TRUE)
    if(inherits(db, "try-error"))
        stop(paste("file", sQuote(dfile), "is not in valid DCF format"))
    db
}

### ** .source_assignments

.source_assignments <-
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

### .source_assignments_in_code_dir

.source_assignments_in_code_dir <-
function(dir, env)
{
    ## Combine all code files in @code{dir}, read and parse expressions,
    ## and successively evaluated the top-level assignments in
    ## @code{env}.
    con <- tempfile("Rcode")
    on.exit(unlink(con))
    if(!file.create(con))
        stop(paste("unable to create", con))
    if(!all(file.append(con, list_files_with_type(dir, "code"))))
        stop("unable to write code files")
    .source_assignments(con, env)
}

### * .split_dependencies

.split_dependencies <- function(x)
{
    ## given one or more Depends: or Suggests: fields from DESCRIPTION
    ## return a named list of list (name, [op, version])
    if(!length(x)) return(list())
    x <- unlist(strsplit(x, ","))
    x <- unique(sub("^[[:space:]]*(.*)[[:space:]]*$", "\\1" , x))
    names(x) <- sub("^([[:alnum:].]+).*$", "\\1" , x)
    lapply(x, .split_op_version)
}

### * .split_op_version

.split_op_version <- function(x)
{
    ## given a single piece of dependency
    ## return a list of components (name, [op, version])
    pat <- "^([^\\([:space:]]+)[[:space:]]*\\(([^\\)]+)\\).*"
    x1 <- sub(pat, "\\1", x)
    x2 <- sub(pat, "\\2", x)
    if(x2 != x1) {
        pat <- "[[:space:]]*([[<>=]+)[[:space:]]+(.*)"
        list(name = x1, op = sub(pat, "\\1", x2),
             version = package_version(sub(pat, "\\2", x2)))
    } else list(name=x1)
}

### ** .strip_whitespace

.strip_whitespace <-
function(x)
{
    ## Strip leading and trailing whitespace.
    x <- sub("^[[:space:]]*", "", x)
    x <- sub("[[:space:]]*$", "", x)
    x
}

### ** .try_quietly

.try_quietly <-
function(expr)
{
    ## Try to run an expression, suppressing all 'output'.  In case of
    ## failure, stop with the error message.
    oop <- options(warn = 1)
    on.exit(options(oop))
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

### ** .wrong_args

.wrong_args <-
function(args, msg)
{
    len <- length(args)
    if(len == 0)
        character()
    else if(len == 1)
        paste("argument", sQuote(args), msg)
    else
        paste("arguments",
              paste(c(rep.int("", len - 1), "and "),
                    sQuote(args),
                    c(rep.int(", ", len - 1), ""),
                    sep = "", collapse = ""),
              msg)
}


### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "### [*]+" ***
### End: ***
