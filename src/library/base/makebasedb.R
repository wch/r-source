#  File src/library/base/makebasedb.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1995-2012 The R Core Team
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
#  http://www.r-project.org/Licenses/

local({
    makeLazyLoadDB <- function(from, filebase, compress = TRUE, ascii = FALSE,
                               variables) {

        envlist <- function(e)
            .Internal(getVarsFromFrame(ls(e, all = TRUE), e, FALSE))

        envtable <- function() {
            idx <- 0
            envs <- NULL
            enames <- character(0)
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
                name <- paste("env", idx, sep="::")
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
                    assign(name, key, envir = envenv)
                }
                name
            }
        }

        if (is.environment(from)) {
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

        for (i in seq_along(vars)) {
            if (is.environment(from))
                key <- lazyLoadDBinsertVariable(vars[i], from, datafile,
                                                ascii, compress,  envhook)
            else key <- lazyLoadDBinsertListElement(from, i, datafile, ascii,
                                                    compress, envhook)
            assign(vars[i], key, envir = varenv)
        }

        vals <- lapply(vars, get, envir = varenv, inherits = FALSE)
        names(vals) <- vars

        rvars <- ls(envenv, all = TRUE)
        rvals <- lapply(rvars, get, envir = envenv, inherits = FALSE)
        names(rvals) <- rvars

        val <- list(variables = vals, references = rvals,
                    compressed = compress)
       saveRDS(val, mapfile)
    }

    omit <- c(".Last.value", ".AutoloadEnv", ".BaseNamespaceEnv",
              ".Device", ".Devices", ".Machine", ".Options", ".Platform")

    if (length(search()[search()!="Autoloads"]) != 2)
        stop("start R with NO packages loaded to create the data base")

    baseFileBase <- file.path(.Library,"base","R","base")

    if (file.info(baseFileBase)["size"] < 20000) # crude heuristic
        stop("may already be using lazy loading on base");

    basevars <- ls(baseenv(), all.names=TRUE)
    prims <- basevars[sapply(basevars, function(n) is.primitive(get(n, baseenv())))]
    basevars <- basevars[! basevars %in% c(omit, prims)]

    makeLazyLoadDB(baseenv(), baseFileBase, variables = basevars)
})
