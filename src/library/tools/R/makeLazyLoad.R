code2LazyLoadDB <-
    function(package, lib.loc = NULL,
             keep.source = getOption("keep.source.pkgs"),
             compress = TRUE)
{
    pkgpath <- .find.package(package, lib.loc, quiet = TRUE)
    if(length(pkgpath) == 0)
        stop(paste("There is no package called", sQuote(package)))
    loadenv <-new.env(hash=TRUE)
    codeFile <- file.path(pkgpath, "R", package)
    dbbase <- file.path(pkgpath, "R", package)
    if (packageHasNamespace(package, dirname(pkgpath))) {
        if (! is.null(.Internal(getRegisteredNamespace(as.name(package)))))
            stop("name space must not be loaded.")
        ns <- loadNamespace(package, lib.loc, keep.source, TRUE, TRUE)
        makeLazyLoadDB(ns, dbbase)
    }
    else {
        loadenv <- new.env(hash = TRUE, parent = .GlobalEnv)
        if(file.exists(codeFile))
            sys.source(codeFile, loadenv, keep.source = keep.source)
        ## now transfer contents of loadenv to a new env to mimic library
        ## the actual copy has to be done by C code to avoid forcing
        ## promises that might have been created using delay().
        env <- new.env(hash=TRUE)
        .Internal(lib.fixup(loadenv, env))
        ## save the package name in the environment
        assign(".packageName", package, envir = env)
        makeLazyLoadDB(env, dbbase, compress = compress)
    }
}

rda2LazyLoadDB <- function(package, lib.loc = NULL, compress = TRUE)
{
    pkgpath <- .find.package(package, lib.loc, quiet = TRUE)
    if(length(pkgpath) == 0)
        stop(paste("There is no package called", sQuote(package)))
    rdafile <- file.path(pkgpath, "R", "all.rda")
    if (! file.exists(rdafile))
        stop(paste("Package", sQuote(package), "has no .rda file"))
    dbbase <- file.path(pkgpath, "R", package)
    e <- new.env(hash=TRUE)
    load(rdafile, e)
    makeLazyLoadDB(e, dbbase, compress = compress)
}

makeLazyLoadDB <- function(from, filebase, compress = TRUE, ascii = FALSE,
                           variables) {

    envlist <- function(e) {
        names <- ls(e, all=TRUE)
        list <- .Call("R_getVarsFromFrame", names, e, FALSE, PACKAGE="base")
        names(list) <- names
        list
    }

    envtable <- function() {
        idx <- 0
        envs <- NULL
        enames <- character(0)
        find <- function(v, keys, vals)
            for (i in seq(along=keys))
                if (identical(v, keys[[i]]))
                    return(vals[i])
        getname <- function(e) find(e, envs, enames)
        getenv <- function(n) find(n, enames, envs)
        insert <- function(e) {
            idx <<- idx + 1
            name <- paste("env", idx, sep="::")
            envs <<- c(e, envs)
            enames <<- c(name, enames)
            name
        }
        list(insert = insert, getenv = getenv, getname = getname)
    }

    lazyLoadDBinsertValue <- function(value, file, ascii, compress, hook)
        .Call("R_lazyLoadDBinsertValue", value, file, ascii, compress, hook,
              PACKAGE = "base")

    lazyLoadDBinsertListElement <- function(x, i, file, ascii, compress, hook)
        .Call("R_lazyLoadDBinsertValue", x[[i]], file, ascii, compress, hook,
              PACKAGE = "base")

    lazyLoadDBinsertVariable <- function(n, e, file, ascii, compress, hook) {
        x <- .Call("R_getVarsFromFrame", n, e, FALSE, PACKAGE="base")
       .Call("R_lazyLoadDBinsertValue", x[[1]], file, ascii, compress, hook,
              PACKAGE = "base")
    }

    mapfile <- paste(filebase, "rdx", sep = ".")
    datafile <- paste(filebase, "rdb", sep = ".")
    close(file(datafile, "w")) # truncate to zero
    table <- envtable()
    varenv <- new.env(hash = TRUE)
    envenv <- new.env(hash = TRUE)

    envhook <- function(e) {
        if (is.environment(e)) {
            name <- table$getname(e)
            if (is.null(name)) {
                name <- table$insert(e)
                data <- list(bindings = envlist(e),
                             enclos = parent.env(e))
                key <- lazyLoadDBinsertValue(data, datafile, ascii,
                                             compress, envhook)
                assign(name, key, env = envenv)
            }
            name
        }
    }

    if (is.null(from) || is.environment(from)) {
        if (! missing(variables))
            vars <- variables
        else vars <- ls(from, all = TRUE)
    }
    else if (is.list(from)) {
        vars <- names(from)
        if (length(vars) != length(from) || any(nchar(vars) == 0))
            stop("source list must have names for all elements")
    }
    else stop("source must be an environment or a list");

    for (i in seq(along = vars)) {
        if (is.null(from) || is.environment(from))
            key <- lazyLoadDBinsertVariable(vars[i], from, datafile,
                                            ascii, compress,  envhook)
        else key <- lazyLoadDBinsertListElement(from, i, datafile, ascii,
                                                compress, envhook)
        assign(vars[i], key, env = varenv)
    }

    vals <- lapply(vars, get, env = varenv, inherits = FALSE)
    names(vals) <- vars

    rvars <- ls(envenv, all = TRUE)
    rvals <- lapply(rvars, get, env = envenv, inherits = FALSE)
    names(rvals) <- rvars

    val <- list(variables = vals, references = rvals,
                compressed = compress)
   .saveRDS(val, mapfile)
}

makeLazyLoading <-
    function(package, lib.loc = NULL, compress = TRUE,
             keep.source = getOption("keep.source.pkgs"))
{
    findpack <- function(package, lib.loc) {
        pkgpath <- .find.package(package, lib.loc, quiet = TRUE)
        if(length(pkgpath) == 0)
            stop(paste("There is no package called", sQuote(package)))
        pkgpath
    }

    pkgpath <- findpack(package, lib.loc)

    if (package == "base")
        loaderFile <- system.file("baseloader.R", package="base",
                                  lib.loc=.Library)
    else if (packageHasNamespace(package, dirname(pkgpath)))
        loaderFile <- file.path(R.home(), "share", "R", "nspackloader.R")
    else
        loaderFile <- file.path(R.home(), "share", "R", "packloader.R")
    codeFile <- file.path(pkgpath, "R", package)

    if (file.info(codeFile)["size"] == file.info(loaderFile)["size"])
        warning("package seems to be using lazy loading already")
    else if (package == "base") {
        dbFile <- file.path(pkgpath, "R", "base.rdx")
        if (! file.exists(dbFile))
            stop("you need to first build the base DB with `makebasedb.R'")
        if (file.info(codeFile)["mtime"] > file.info(dbFile)["mtime"])
            stop("code file newer than base DB; rebuild with `makebasedb.R'")
        file.copy(loaderFile, codeFile, TRUE)
    }
    else {
        rdaFile <- file.path(pkgpath, "R", "all.rda")
        if (file.exists(rdaFile))
            rda2LazyLoadDB(package, lib.loc, compress = compress)
        else
            code2LazyLoadDB(package, lib.loc = lib.loc,
                            keep.source = keep.source, compress = compress)
        file.copy(loaderFile, codeFile, TRUE)
    }
    invisible()
}
