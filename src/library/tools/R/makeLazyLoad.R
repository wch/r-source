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

sysdata2LazyLoadDB <- function(srcFile, destDir, compress = TRUE)
{
    e <- new.env(hash=TRUE)
    load(srcFile, e)
    makeLazyLoadDB(e, file.path(destDir, "sysdata"), compress = compress)
}

list_data_in_pkg <- function(package, lib.loc = NULL, dataDir = NULL)
{
    if(is.null(dataDir)) {
        pkgpath <- .find.package(package, lib.loc, quiet = TRUE)
        if(length(pkgpath) == 0)
            stop(paste("There is no package called", sQuote(package)))
        dataDir <- file.path(pkgpath, "data")
    } else {
        pkgpath <- sub("/data$", "", dataDir)
        package <- basename(pkgpath)
	# avoid builddir != srcdir problems -- assume package has been installed
        lib.loc <- c(dirname(pkgpath), .libPaths())
    }
    if(file_test("-d", dataDir)) {
        if(file.exists(sv <- file.path(dataDir, "Rdata.rds"))) {
            ans <- .readRDS(sv)
        } else if(file.exists(sv <- file.path(dataDir, "datalist"))) {
            ans <- strsplit(readLines(sv), ":")
            nms <- lapply(ans, function(x) x[1])
            ans <- lapply(ans, function(x)
                          if(length(x)==1) x[1] else
                          strsplit(x[2], " +")[[1]][-1])
            names(ans) <- nms
        } else {
            files <- list_files_with_type(dataDir, "data")
            files <- unique(basename(file_path_sans_ext(files)))
            ans <- vector("list", length(files))
            dataEnv <- new.env(hash=TRUE)
            names(ans) <- files
            for(f in files) {
                utils::data(list = f, package = package, lib.loc = lib.loc,
                            envir = dataEnv)
                ans[[f]] <- ls(envir = dataEnv, all = TRUE)
                rm(list = ans[[f]], envir = dataEnv)
            }
        }
        ans
    } else NULL
}

data2LazyLoadDB <- function(package, lib.loc = NULL, compress = TRUE)
{
    pkgpath <- .find.package(package, lib.loc, quiet = TRUE)
    if(length(pkgpath) == 0)
        stop(paste("There is no package called", sQuote(package)))
    dataDir <- file.path(pkgpath, "data")
    if(tools:::file_test("-d", dataDir)) {
        if(file.exists(file.path(dataDir, "Rdata.rds")))
            warning("package seems to be using lazy loading for data already")
        dataEnv <- new.env(hash=TRUE)
        tmpEnv <- new.env()
        f0 <- files <- tools:::list_files_with_type(dataDir, "data")
        files <- unique(basename(tools:::file_path_sans_ext(files)))
        dlist <- vector("list", length(files))
        names(dlist) <- files
        loaded <- character(0)
        for(f in files) {
            utils::data(list = f, package = package, lib.loc = lib.loc,
                        envir = dataEnv)
            utils::data(list = f, package = package, lib.loc = lib.loc,
                        envir = tmpEnv)
            tmp <- ls(envir = tmpEnv, all.names = TRUE)
            rm(list = tmp, envir = tmpEnv)
            dlist[[f]] <- tmp
            loaded <- c(loaded, tmp)
        }
        dup<- duplicated(loaded)
        if(any(dup))
            warning("object(s) ", paste(sQuote(loaded[dup]), collapse=", "),
                    " are created by more than one data call")

        if(length(loaded)) {
            dbbase <- file.path(dataDir, "Rdata")
            makeLazyLoadDB(dataEnv, dbbase, compress = compress)
            .saveRDS(dlist, file.path(dataDir, "Rdata.rds"))
            print(f0)
            unlink(f0)
            if(file.exists(file.path(dataDir, "filelist")))
                unlink(file.path(dataDir, c("filelist", "Rdata.zip")))
        }
    }
}

makeLazyLoadDB <- function(from, filebase, compress = TRUE, ascii = FALSE,
                           variables)
{
    envlist <- function(e) {
        names <- ls(e, all=TRUE)
        .Call("R_getVarsFromFrame", names, e, FALSE, PACKAGE="base")
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
    close(file(datafile, "wb")) # truncate to zero
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
                # key[1] <- pos; pos <<- pos + key[2]
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

    pos <- as.integer(0)
    for (i in seq(along = vars)) {
        key <- if (is.null(from) || is.environment(from))
            lazyLoadDBinsertVariable(vars[i], from, datafile,
                                     ascii, compress,  envhook)
        else lazyLoadDBinsertListElement(from, i, datafile, ascii,
                                         compress, envhook)
        # key[1] <- pos; pos <- pos + key[2]
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
        loaderFile <- file.path(R.home(), "share", "R", "baseloader.R")
    else if (packageHasNamespace(package, dirname(pkgpath)))
        loaderFile <- file.path(R.home(), "share", "R", "nspackloader.R")
    else
        loaderFile <- file.path(R.home(), "share", "R", "packloader.R")
    codeFile <- file.path(pkgpath, "R", package)

    if (!file.exists(codeFile)) {
        warning("package contains no R code")
        return(invisible())
    }
    if (file.info(codeFile)["size"] == file.info(loaderFile)["size"])
        warning("package seems to be using lazy loading already")
    else if (package == "base" && is.null(lib.loc)) {# allow for cross-building
        dbFile <- file.path(pkgpath, "R", "base.rdx")
        if (! file.exists(dbFile))
            stop("you need to first build the base DB with 'makebasedb.R'")
        if (file.info(codeFile)["mtime"] > file.info(dbFile)["mtime"])
            stop("code file newer than base DB; rebuild with 'makebasedb.R'")
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
    ## <NOTE> This test needs to be independent of tools, so
    ## package tools can be prepared for lazy loading.
    ## </NOTE>
    ## if(file.exists(file.path(pkgpath, "data")))
    ##    data2LazyLoadDB(package, lib.loc, compress = compress)

    invisible()
}
