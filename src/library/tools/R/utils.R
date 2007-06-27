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
        stop(gettextf("file '%s' does not exist", x),
             domain = NA)
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
           stop(gettextf("test '%s' is not available", op),
                domain = NA))
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
function(dir, type, all.files = FALSE, full.names = TRUE,
         OS_subdirs = .OStype())
{
    ## Return a character vector with the paths of the files in
    ## @code{dir} of type @code{type} (as in .make_file_exts()).
    ## When listing R code and documentation files, files in OS-specific
    ## subdirectories are included (if present) according to the value
    ## of @code{OS_subdirs}.
    exts <- .make_file_exts(type)
    files <-
        list_files_with_exts(dir, exts, all.files = all.files,
                             full.names = full.names)

    if(type %in% c("code", "docs")) {
        for(os in OS_subdirs) {
            os_dir <- file.path(dir, os)
            if(file_test("-d", os_dir)) {
                os_files <- list_files_with_exts(os_dir, exts,
                                                 all.files = all.files,
                                                 full.names = FALSE)
                os_files <- file.path(if(full.names) os_dir else os,
                                      os_files)
                files <- c(files, os_files)
            }
        }
    }
    if(type %in% c("code", "docs")) { # only certain filenames are valid.
        files <- files[grep("^[A-Za-z0-9]", basename(files))]
    }
    if(type %in% "demo") {           # only certain filenames are valid.
        files <- files[grep("^[A-Za-z]", basename(files))]
    }
    files
}

### ** extract_Rd_file

extract_Rd_file <-
function(file, topic)
{
    ## extract an Rd file from the man dir.
    if(is.character(file)) {
        file <- if(length(grep("\\.gz$", file))) gzfile(file, "r")
        else file(file, "r")
        on.exit(close(file))
    }
    valid_lines <- lines <- readLines(file, warn = FALSE)
    valid_lines[is.na(nchar(lines, "c", TRUE))] <- ""
    patt <- paste("^% --- Source file:.*/", topic, ".Rd ---$", sep="")
    if(length(top <- grep(patt, valid_lines)) != 1)
        stop("no or more than one match")
    eofs <- grep("^\\\\eof$", valid_lines)
    end <- min(eofs[eofs > top]) - 1
    lines[top:end]
}

### * Text utilities.

### ** delimMatch

delimMatch <-
function(x, delim = c("{", "}"), syntax = "Rd")
{
    if(!is.character(x))
        stop("argument 'x' must be a character vector")
    ## FIXME: bytes or chars?
    if((length(delim) != 2) || any(nchar(delim) != 1))
        stop("argument 'delim' must specify two characters")
    if(syntax != "Rd")
        stop("only Rd syntax is currently supported")

    .Call(delim_match, x, delim)
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
        if(file.exists(file.path(R.home("bin"), "texi2dvi")))
            texi2dvi <- file.path(R.home("bin"), "texi2dvi")
        else
            texi2dvi <- "texi2dvi"
    }

    yy <- system(paste(shQuote(texi2dvi),
                       quiet, pdf, clean,
                       shQuote(file)))
    if(yy > 0)
      stop(gettextf("running texi2dvi on '%s' failed", file),
           domain = NA)
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
    if(nzchar(OS)) OS else .Platform$OS.type
}

### ** .capture_output_from_print

.capture_output_from_print <-
function(x, ...)
{
    ## Better to provide a simple variant of utils::capture.output()
    ## ourselves (so that bootstrapping R only needs base and tools).
    out <- NULL # Prevent codetools warning about "no visible binding
                # for global variable out".  Maybe there will eventually
                # be a better design for output text connections ...
    file <- textConnection("out", "w", local = TRUE)
    sink(file)
    on.exit({ sink(); close(file) })
    print(x, ...)
    out
}

### ** .file_append_ensuring_LFs

.file_append_ensuring_LFs <-
function(file1, file2)
{
    ## Use a fast version of file.append() that ensures LF between
    ## files.
    .Internal(codeFiles.append(file1, file2))
}

### ** .filter

.filter <-
function(x, f, ...)
{
    ## Higher-order function for filtering elements for which predicate
    ## function f (with additional arguments in ...) is true.
    x[as.logical(sapply(x, f, ...))]
}

### ** .find_owner_env

.find_owner_env <-
function(v, env, last = NA, default = NA) {
    while(!identical(env, last))
        if(exists(v, envir = env, inherits = FALSE))
            return(env)
        else
            env <- parent.env(env)
    default
}

### ** .get_contains_from_package_db

.get_contains_from_package_db <-
function(db)
{
    if("Contains" %in% names(db))
        unlist(strsplit(db["Contains"], " +"))
    else
        character()
}

### ** .get_internal_S3_generics

.get_internal_S3_generics <-
function(primitive = TRUE) # primitive means 'include primitives'
{
    out <-
        ## Get the names of R internal S3 generics (via DispatchOrEval(),
        ## cf. zMethods.Rd).
        c("[", "[[", "$", "[<-", "[[<-", "$<-",
          "as.vector", "unlist",
          .get_S3_primitive_generics()
          ## ^^^^^^^ now contains the members of the group generics from groupGeneric.Rd
          )
    if(!primitive)
        out <- out[!sapply(out, .is_primitive, baseenv())]
    out
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
    if(!length(S3_methods_list)) return(matrix(character(), ncol = 3))
    idx <- is.na(S3_methods_list[, 3])
    S3_methods_list[idx, 3] <-
        paste(S3_methods_list[idx, 1],
              S3_methods_list[idx, 2],
              sep = ".")
    S3_methods_list
}

### ** .get_requires_from_package_db

.get_requires_from_package_db <-
function(db, category = c("Depends", "Imports", "Suggests", "Enhances"))
{
    category <- match.arg(category)
    if(category %in% names(db)) {
        requires <- unlist(strsplit(db[category], ","))
        requires <-
            sub("^[[:space:]]*([[:alnum:].]+).*$", "\\1", requires)
        if(category == "Depends")
            requires <- requires[requires != "R"]
    }
    else
        requires <- character()
    requires
}

### ** .get_S3_generics_as_seen_from_package

.get_S3_generics_as_seen_from_package <-
function(dir, installed = TRUE, primitive = FALSE)
{
    ## Get the S3 generics "as seen from a package" rooted at
    ## @code{dir}.  Tricky ...
    if(basename(dir) == "base")
        env_list <- list()
    else {
        ## Always look for generics in the whole of the former base.
        ## (Not right, but we do not perform run time analyses when
        ## working off package sources.)  Maybe change this eventually,
        ## but we still cannot rely on packages to fully declare their
        ## dependencies on base packages.
        env_list <-
            list(baseenv(),
                 as.environment("package:graphics"),
                 as.environment("package:stats"),
                 as.environment("package:utils"))
        if(installed) {
            ## Also use the loaded namespaces and attached packages
            ## listed in the DESCRIPTION Depends and Imports fields.
            ## Not sure if this is the best approach: we could also try
            ## to determine which namespaces/packages were made
            ## available by loading the package (which should work at
            ## least when run from R CMD check), or we could simply
            ## attach every package listed as a dependency ... or
            ## perhaps do both.
            db <- .read_description(file.path(dir, "DESCRIPTION"))
            depends <- .get_requires_from_package_db(db, "Depends")
            imports <- .get_requires_from_package_db(db, "Imports")
            reqs <- intersect(c(depends, imports), loadedNamespaces())
            if(length(reqs))
                env_list <- c(env_list, lapply(reqs, getNamespace))
            reqs <- intersect(depends %w/o% loadedNamespaces(),
                              .packages())
            if(length(reqs))
                env_list <- c(env_list, lapply(reqs, .package_env))
            env_list <- unique(env_list)
        }
    }
    unique(c(.get_internal_S3_generics(primitive),
             unlist(lapply(env_list,
                           function(env) {
                               nms <- objects(envir = env,
                                              all.names = TRUE)
                               if(".no_S3_generics" %in% nms)
                                   character()
                               else .filter(nms, .is_S3_generic, env)
                           }))))
}

### ** .get_S3_group_generics

.get_S3_group_generics <-
function()
    c("Ops", "Math", "Summary", "Complex")

### ** .get_S3_primitive_generics

.get_S3_primitive_generics <-
function(include_group_generics = TRUE)
{
    if(include_group_generics)
        c(base::.S3PrimitiveGenerics,
          "abs", "sign", "sqrt", "floor", "ceiling", "trunc", "round",
          "signif", "exp", "log", "expm1", "log1p", "cos", "sin", "tan", "acos", "asin",
          "atan", "cosh", "sinh", "tanh", "acosh", "asinh", "atanh",
          "lgamma", "gamma", "gammaCody", "digamma", "trigamma",
          "tetragamma", "pentagamma", "cumsum", "cumprod", "cummax",
          "cummin",
          "+", "-", "*", "/", "^", "%%", "%/%", "&", "|", "!", "==",
          "!=", "<", "<=", ">=", ">",
          "all", "any", "sum", "prod", "max", "min", "range",
          "Arg", "Conj", "Im", "Mod", "Re")
    else
        base::.S3PrimitiveGenerics
}

### ** .get_standard_Rd_keywords

.get_standard_Rd_keywords <-
function()
{
    lines <- readLines(file.path(R.home("doc"), "KEYWORDS.db"))
    lines <- grep("^.*\\|([^:]*):.*", lines, value = TRUE)
    lines <- sub( "^.*\\|([^:]*):.*", "\\1", lines)
    lines
}

### ** .get_standard_package_names

## we cannot assume that file.path(R.home(), "share", "make", "vars.mk")
## is installed, as it is not on Windows
.get_standard_package_names <-
local({
    lines <- readLines(file.path(R.home("share"), "make", "vars.mk"))
    lines <- grep("^R_PKGS_[[:upper:]]+ *=", lines, value = TRUE)
    out <- strsplit(sub("^R_PKGS_[[:upper:]]+ *= *", "", lines), " +")
    names(out) <-
        tolower(sub("^R_PKGS_([[:upper:]]+) *=.*", "\\1", lines))
    eval(substitute(function() {out}, list(out=out)), envir=NULL)
})

### ** .get_standard_repository_db_fields

.get_standard_repository_db_fields <-
function()
    c("Package", "Version", "Priority", "Bundle",
      "Contains", "Depends", "Imports", "Suggests")

### ** .identity

.identity <-
function(x)
    x

### ** .is_ASCII

.is_ASCII <-
function(x)
{
    ## Determine whether the strings in a character vector are ASCII or
    ## not.
    as.logical(sapply(as.character(x),
                      function(txt)
                      all(charToRaw(txt) <= as.raw(127))))
}

### ** .is_ISO_8859

.is_ISO_8859 <-
function(x)
{
    ## Determine whether the strings in a character vector could be in
    ## some ISO 8859 character set or not.
    raw_ub <- charToRaw("\x7f")
    raw_lb <- charToRaw("\xa0")
    as.logical(sapply(as.character(x),
                      function(txt) {
                          raw <- charToRaw(txt)
                          all(raw <= raw_ub | raw >= raw_lb)
                      }))
}

### ** .is_primitive

.is_primitive <-
function(fname, envir)
{
    ## Determine whether object named 'fname' found in environment
    ## 'envir' is a primitive function.
    is.primitive(get(fname, envir = envir, inherits = FALSE))
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
        for (ee in as.list(e[-1])) if (nzchar(res <- isUME(ee))) return(res)
        ""
    }
    isUMEif <- function(e) {
        if (length(e) == 3) isUME(e[[3]])
        else {
            if (nzchar(res <- isUME(e[[3]]))) res
            else if (nzchar(res <- isUME(e[[4]]))) res
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
    if(mustMatch) res == fname else nzchar(res)
}

### ** .load_package_quietly

.load_package_quietly <-
function(package, lib.loc)
{
    ## Load (reload if already loaded) @code{package} from
    ## @code{lib.loc}, capturing all output and messages.  Don't do
    ## anything for base, and don't attempt reloading methods, as this
    ## does not work (most likely a bug).
    ##
    ## All QC functions use this for loading packages because R CMD
    ## check interprets all output as indicating a problem.
    if(package != "base")
        .try_quietly({
            pos <- match(paste("package", package, sep = ":"), search())
            if(!is.na(pos)) {
                if(package == "methods") return()
                detach(pos = pos, unload = TRUE)
            }
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

### ** .make_S3_group_generic_env

.make_S3_group_generic_env <-
function(parent = parent.frame())
{
    ## Create an environment with pseudo-definitions for the S3 group
    ## methods.
    env <- new.env(parent = parent)
    assign("Math", function(x, ...) UseMethod("Math"),
           envir = env)
    assign("Ops", function(e1, e2) UseMethod("Ops"),
           envir = env)
    assign("Summary", function(..., na.rm = FALSE) UseMethod("Summary"),
           envir = env)
    assign("Complex", function(z) UseMethod("Complex"),
           envir = env)
    env
}

### ** .make_S3_primitive_generic_env

.make_S3_primitive_generic_env <-
function(parent = parent.frame(), fixup = FALSE)
{
    ## Create an environment with pseudo-definitions for the S3 primitive
    ## generics
    env <- new.env(parent = parent)
    for(f in ls(base::.GenericArgsEnv))
        assign(f, get(f, envir=base::.GenericArgsEnv), envir = env)
    if(fixup) {
        ## now fixup the operators
        for(f in c('+', '-', '*', '/', '^', '%%', '%/%', '&', '|',
                   '==', '!=', '<', '<=', '>=', '>')) {
            fx <- get(f, envir = env)
            formals(fx) <- alist(x=, y=)
            assign(f, fx, envir = env)
        }
    }
    env
}

### ** .make_S3_primitive_nongeneric_env

.make_S3_primitive_nongeneric_env <-
function(parent = parent.frame())
{
    ## Create an environment with pseudo-definitions
    ## for the S3 primitive non-generics
    env <- new.env(parent = parent)
    for(f in ls(base::.ArgsEnv))
        assign(f, get(f, envir=base::.ArgsEnv), envir = env)
    env
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
             "rep.int", "round.POSIXt",
             "seq.int", "sort.int", "sort.list"),
             BSDA = "sign.test",
             Hmisc = c("abs.error.pred", "t.test.cluster"),
             HyperbolicDist = "log.hist",
             MASS = c("frequency.polygon",
             "gamma.dispersion", "gamma.shape",
             "hist.FD", "hist.scott"),
             SMPracticals = "exp.gibbs",
             XML = "text.SAX",
             ape = "sort.index",
             boot = "exp.tilt",
             car = "scatterplot.matrix",
	     calibrator = "t.fun",
             equivalence = "sign.boot",
             grDevices = "boxplot.stats",
             graphics = c("close.screen",
             "plot.design", "plot.new", "plot.window", "plot.xy",
             "split.screen"),
             hier.part = "all.regs",
             mratios = c("t.test.ratio.default", "t.test.ratio.formula"),
             quadprog = c("solve.QP", "solve.QP.compact"),
             reposTools = "update.packages2",
             sac = "cumsum.test",
             sm = "print.graph",
             stats = c("anova.lmlist", "fitted.values", "lag.plot",
             "influence.measures", "t.test"),
             supclust = c("sign.change", "sign.flip"),
             utils = c("close.socket", "flush.console", "update.packages")
             )
    if(is.null(package)) return(unlist(stopList))
    thisPkg <- stopList[[package, exact = TRUE]] # 'st' matched 'stats'
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
        packages <-
            unique(utils::installed.packages(priority = "high")[ , 1])
    out <- lapply(packages, function(p)
                  tryCatch(FUN(p, ...),
                           error = function(e)
                           noquote(paste("Error:",
                                         conditionMessage(e)))))
    ## (Just don't throw the error ...)
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
    .try_quietly(readLines(con, warn=FALSE))
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
        stop(gettextf("file '%s' does not exist", dfile),
             domain = NA)
    db <- try(read.dcf(dfile)[1, ], silent = TRUE)
    if(inherits(db, "try-error"))
        stop(gettextf("file '%s' is not in valid DCF format", dfile),
             domain = NA)
    db
}

### ** .source_assignments

.source_assignments <-
function(file, envir, enc = NA)
{
    ## Read and parse expressions from @code{file}, and then
    ## successively evaluate the top-level assignments in @code{envir}.
    ## Apart from only dealing with assignments, basically does the same
    ## as @code{sys.source(file, envir, keep.source = FALSE)}.
    oop <- options(keep.source = FALSE)
    on.exit(options(oop))
    assignmentSymbolLM <- as.symbol("<-")
    assignmentSymbolEq <- as.symbol("=")
    if(!is.na(enc) &&
       !(Sys.getlocale("LC_CTYPE") %in% c("C", "POSIX"))) {
        con <- file(file, encoding = enc)
        on.exit(close(con))
    } else con <- file
    exprs <- parse(n = -1, file = con)
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
function(dir, env, enc = NA)
{
    ## Combine all code files in @code{dir}, read and parse expressions,
    ## and successively evaluated the top-level assignments in
    ## @code{env}.
    con <- tempfile("Rcode")
    on.exit(unlink(con))
    if(!file.create(con))
        stop("unable to create ", con)
    if(!all(.file_append_ensuring_LFs(con,
                                      list_files_with_type(dir,
                                                           "code"))))
        stop("unable to write code files")
    tryCatch(.source_assignments(con, env, enc = enc),
             error =
             function(e)
             stop("cannot source package code\n",
                  conditionMessage(e),
                  call. = FALSE))
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
    x <- sub("^[[:space:]]+", "", x)
    x <- sub("[[:space:]]+$", "", x)
    x
}

### ** .try_quietly

.try_quietly <-
function(expr)
{
    ## Try to run an expression, suppressing all 'output'.  In case of
    ## failure, stop with the error message and a "traceback" ...

    oop <- options(warn = 1)
    on.exit(options(oop))
    outConn <- file(open = "w+")         # anonymous tempfile
    sink(outConn, type = "output")
    sink(outConn, type = "message")
    yy <- tryCatch(withRestarts(withCallingHandlers(expr, error = {
        function(e) invokeRestart("grmbl", e, sys.calls())
    }),
                                grmbl = function(e, calls) {
                                    n <- length(sys.calls())
                                    ## Chop things off as needed ...
                                    calls <- calls[-seq.int(length.out = n - 1)]
                                    calls <- rev(calls)[-c(1, 2)]
                                    tb <- lapply(calls, deparse)
                                    stop(conditionMessage(e),
                                         "\nCall sequence:\n",
                                         paste(utils::capture.output(traceback(tb)),
                                               collapse = "\n"),
                                         call. = FALSE)
                                }),
                   error = .identity,
                   finally = {
                       sink(type = "message")
                       sink(type = "output")
                       close(outConn)
                   })
    if(inherits(yy, "error"))
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
