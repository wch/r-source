#  File src/library/tools/R/makeLazyLoad.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2019 The R Core Team
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  https://www.R-project.org/Licenses/

code2LazyLoadDB <-
    function(package, lib.loc = NULL,
             keep.source = getOption("keep.source.pkgs"),
             keep.parse.data = getOption("keep.parse.data.pkgs"),
             compress = TRUE, set.install.dir = NULL)
{
    pkgpath <- find.package(package, lib.loc, quiet = TRUE)
    if(!length(pkgpath))
        stop(packageNotFoundError(package, lib.loc, sys.call()))
    dbbase <- file.path(pkgpath, "R", package)
    if (packageHasNamespace(package, dirname(pkgpath))) {
        if (! is.null(.getNamespace(as.name(package))))
            stop("namespace must not be already loaded")
        ns <- suppressPackageStartupMessages(loadNamespace(
                  package = package, lib.loc = lib.loc,
                  keep.source = keep.source, keep.parse.data = keep.parse.data,
                  partial = TRUE))
        makeLazyLoadDB(ns, dbbase, compress = compress,
                       set.install.dir = set.install.dir)
    }
    else
        stop("all packages should have a NAMESPACE")
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
        pkgpath <- find.package(package, lib.loc, quiet = TRUE)
        if(!length(pkgpath))
            stop(packageNotFoundError(package, lib.loc, sys.call()))
        dataDir <- file.path(pkgpath, "data")
    } else {
	if(has.pkg <- !missing(package)) ## try with default lib.loc
	    pkgpath <- find.package(package, lib.loc, quiet = TRUE)
	if(!has.pkg || !length(pkgpath)) {
	   ## <FIXME> making assumptions about dataDir (e.g., pkgpath *NOT* from R-forge symlink)
	    pkgpath <- sub("/data$", "", dataDir)
	    ## avoid builddir != srcdir problems -- assume package has been installed
	    ## making use of the fact that utils::data() works with *source* package:
	    lib.loc <- c(dirname(pkgpath), .libPaths())
	    if(!has.pkg)
		package <- basename(pkgpath)
	}
    }
    if(dir.exists(dataDir)) {
        if(file.exists(sv <- file.path(dataDir, "Rdata.rds"))) {
            ans <- readRDS(sv)
        } else if(file.exists(sv <- file.path(dataDir, "datalist")) &&
                  !file.info(sv)$isdir) { # package cp4p had a directory
            ## BioC mess this file up, of course!
            ans <- strsplit(readLines(sv, warn = FALSE), ":")
            nms <- lapply(ans, function(x) x[1L])
            ans <- lapply(ans, function(x)
                          if(length(x) == 1L) x[1L] else
                          strsplit(x[2L], " +")[[1L]][-1L])
            names(ans) <- nms
        } else {
            files <- list_files_with_type(dataDir, "data")
            ## omit compression extensions
            files <- unique(basename(file_path_sans_ext(files, TRUE)))
            ans <- vector("list", length(files))
            dataEnv <- new.env(hash=TRUE)
            names(ans) <- files
            for(f in files) {
                ## This occasionally fails on uninstalled sources,
                ## hence the tryCatch().  And e.g. CHNOSZ gave
                ## messages and cricketr gave warnings.
                tryCatch(suppressMessages(suppressWarnings(utils::data(list = f, package = package, lib.loc = lib.loc, envir = dataEnv))), error = identity)
                ans[[f]] <- ls(envir = dataEnv, all.names = TRUE)
                rm(list = ans[[f]], envir = dataEnv)
            }
        }
        ans
    } else NULL
}

data2LazyLoadDB <- function(package, lib.loc = NULL, compress = TRUE)
{
    options(warn=1)
    pkgpath <- find.package(package, lib.loc, quiet = TRUE)
    if(!length(pkgpath))
        stop(packageNotFoundError(package, lib.loc, sys.call()))
    dataDir <- file.path(pkgpath, "data")
    ## set the encoding for text files to be read, if specified
    enc <- .read_description(file.path(pkgpath, "DESCRIPTION"))["Encoding"]
    if(!is.na(enc)) {
        op <- options(encoding=enc)
        on.exit(options(encoding=op[[1L]]))
    }
    if(dir.exists(dataDir)) {
        if(file.exists(file.path(dataDir, "Rdata.rds")) &&
	    file.exists(file.path(dataDir, paste0(package, ".rdx"))) &&
	    file.exists(file.path(dataDir, paste0(package, ".rdb"))) ){
            warning("package seems to be using lazy loading for data already")
        }
	else {
            dataEnv <- new.env(hash=TRUE)
            tmpEnv <- new.env()
            f0 <- files <- list_files_with_type(dataDir, "data")
            ## omit compression extensions
            files <- unique(basename(file_path_sans_ext(files, TRUE)))
            dlist <- vector("list", length(files))
            names(dlist) <- files
            loaded <- character(0L)
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
            dup <- duplicated(loaded)
            if(any(dup))
                warning(sprintf(ngettext(sum(dup),
                                         "object %s is created by more than one data call",
                                         "objects %s are created by more than one data call"),
                                paste(sQuote(loaded[dup]), collapse=", ")),
                        call. = FALSE, domain = NA)

            if(length(loaded)) {
                dbbase <- file.path(dataDir, "Rdata")
                makeLazyLoadDB(dataEnv, dbbase, compress = compress)
                saveRDS(dlist, file.path(dataDir, "Rdata.rds"),
                         compress = compress)
                unlink(f0)
                if(file.exists(file.path(dataDir, "filelist")))
                    unlink(file.path(dataDir, c("filelist", "Rdata.zip")))
            }
        }
    }
}

makeLazyLoadDB <- function(from, filebase, compress = TRUE, ascii = FALSE,
                           variables, set.install.dir = NULL)
{
    ## pre-empt any problems with interpretation of 'ascii'
    ascii <- as.logical(ascii)
    if (is.na(ascii)) stop("'ascii' must be TRUE or FALSE", domain = NA)
    ascii <- as.integer(ascii)

    envlist <- function(e)
        .Internal(getVarsFromFrame(ls(e, all.names = TRUE), e, FALSE))

    envtable <- function() {
        idx <- 0
        envs <- NULL
        enames <- character(0L)
        find <- function(v, keys, vals) {
            for (i in seq_along(keys))
                if (identical(v, keys[[i]]))
                    return(vals[i])
	    NULL
	}
        getname <- function(e) find(e, envs, enames)
        getenv <- function(n) find(n, enames, envs)
        insert <- function(e) {
            idx <<- idx + 1
            name <- paste0("env::", idx)
            envs <<- c(e, envs)
            enames <<- c(name, enames)
            name
        }
        list(insert = insert, getenv = getenv, getname = getname)
    }

    lazyLoadDBinsertValue <- function(value, file, ascii, compress, hook)
        .Internal(lazyLoadDBinsertValue(value, file, ascii, compress, hook))

    lazyLoadDBinsertListElement <- function(x, i, file, ascii, compress, hook)
        .Internal(lazyLoadDBinsertValue(x[[i]], file, ascii, compress, hook))

    lazyLoadDBinsertVariable <- function(n, e, file, ascii, compress, hook) {
        x <- .Internal(getVarsFromFrame(n, e, FALSE))
        .Internal(lazyLoadDBinsertValue(x[[1L]], file, ascii, compress, hook))
    }

    mapfile <- paste0(filebase, ".rdx")
    datafile <- paste0(filebase, ".rdb")
    close(file(datafile, "wb")) # truncate to zero
    table <- envtable()
    varenv <- new.env(hash = TRUE)
    envenv <- new.env(hash = TRUE)

    # bindings of names from "lazy" will be serialized independently so that
    # they can be loaded lazily, after the other bindings have already been
    # eagerly loaded

    lazyenvhook <- function(e, bindings, lazy) {
        bnames <- names(bindings)
        lnames <- intersect(bnames, lazy)
        if (length(lnames)) {
            enames <- setdiff(bnames, lazy)
            edata <- list(bindings = bindings[enames],
                          enclos = parent.env(e),
                          attributes = attributes(e),
                          isS4 = isS4(e),
                          locked = environmentIsLocked(e))
            ekey <- lazyLoadDBinsertValue(edata, datafile, ascii,
                          compress, envhook)
            lkeys <- lapply(lnames, function(varname) {
                lazyLoadDBinsertValue(bindings[[varname]], datafile,
                                      ascii, compress, envhook)
            })
            names(lkeys) <- lnames
            list(eagerKey = ekey, lazyKeys = lkeys)
        }
    }

    envhook <- function(e) {
        if (is.environment(e)) {
            name <- table$getname(e)
            if (is.null(name)) {
                name <- table$insert(e)
                bindings <- envlist(e)
                key <- NULL

                if (!is.null(set.install.dir)) {
                    if (inherits(e, "srcfilecopy") &&
                            "filename" %in% names(bindings))
                        bindings[["filename"]] <- set.install.dir

                    if (identical(e, nsinfo) && "path" %in% names(bindings))
                        bindings[["path"]] <- set.install.dir
                }

                if (inherits(e, "srcfile"))
                    key <- lazyenvhook(e, bindings, c("lines", "parseData"))

                if (is.null(key)) {
                    data <- list(bindings = bindings,
                                 enclos = parent.env(e),
                                 attributes = attributes(e),
                                 isS4 = isS4(e),
                                 locked = environmentIsLocked(e))
                    key <- lazyLoadDBinsertValue(data, datafile, ascii,
                                                 compress, envhook)
                }
                assign(name, key, envir = envenv)
            }
            name
        }
    }

    if (is.null(from) || is.environment(from)) {
        if (! missing(variables))
            vars <- variables
        else vars <- ls(from, all.names = TRUE)
    }
    else if (is.list(from)) {
        vars <- names(from)
        if (length(vars) != length(from) || any(!nzchar(vars)))
            stop("source list must have names for all elements")
    }
    else stop("source must be an environment or a list")

    if (!is.null(set.install.dir) && is.environment(from)
            && ".__NAMESPACE__." %in% vars) {
        x <- .Internal(getVarsFromFrame(".__NAMESPACE__.", from, FALSE))
        nsinfo <- x[[1L]]
    } else
        nsinfo <- NULL

    for (i in seq_along(vars)) {
        key <- if (is.null(from) || is.environment(from))
            lazyLoadDBinsertVariable(vars[i], from, datafile,
                                     ascii, compress,  envhook)
        else lazyLoadDBinsertListElement(from, i, datafile, ascii,
                                         compress, envhook)
        assign(vars[i], key, envir = varenv)
    }

    vals <- lapply(vars, get, envir = varenv, inherits = FALSE)
    names(vals) <- vars

    rvars <- ls(envenv, all.names = TRUE)
    rvals <- lapply(rvars, get, envir = envenv, inherits = FALSE)
    names(rvals) <- rvars

    val <- list(variables = vals, references = rvals,
                compressed = compress)
    saveRDS(val, mapfile)
}

makeLazyLoading <-
    function(package, lib.loc = NULL, compress = TRUE,
             keep.source = getOption("keep.source.pkgs"),
             keep.parse.data = getOption("keep.parse.data.pkgs"),
             set.install.dir = NULL)
{
    if(!is.logical(compress) && compress %notin% c(2,3))
	stop(gettextf("invalid value for '%s' : %s", "compress",
		      "should be FALSE, TRUE, 2 or 3"), domain = NA)
    options(warn = 1L)
    findpack <- function(package, lib.loc) {
        pkgpath <- find.package(package, lib.loc, quiet = TRUE)
        if(!length(pkgpath))
            stop(packageNotFoundError(package, lib.loc, sys.call()))
        pkgpath
    }

    if (package == "base")
        stop("this cannot be used for package 'base'")

    loaderFile <- file.path(R.home("share"), "R", "nspackloader.R")
    pkgpath <- findpack(package, lib.loc)
    codeFile <- file.path(pkgpath, "R", package)

    if (!file.exists(codeFile)) {
        warning("package contains no R code")
        return(invisible())
    }
    if (file.size(codeFile) == file.size(loaderFile))
        warning("package seems to be using lazy loading already")
    else {
        code2LazyLoadDB(package, lib.loc = lib.loc,
                        keep.source = keep.source,
                        keep.parse.data = keep.parse.data,
                        compress = compress,
                        set.install.dir = set.install.dir)
        file.copy(loaderFile, codeFile, TRUE)
    }

    invisible()
}
