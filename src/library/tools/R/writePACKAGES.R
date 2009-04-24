#  File src/library/tools/R/writePACKAGES.R
#  Part of the R package, http://www.R-project.org
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

write_PACKAGES <-
function(dir = ".", fields = NULL,
         type = c("source", "mac.binary", "win.binary"),
         verbose = FALSE, unpacked = FALSE, subdirs = FALSE)
{
    if(missing(type) && .Platform$OS.type == "windows")
        type <- "win.binary"
    type <- match.arg(type)
    nfields <- 0
    out <- file(file.path(dir, "PACKAGES"), "wt")
    outgz <- gzfile(file.path(dir, "PACKAGES.gz"), "wt")

    paths <- ""
    if(is.logical(subdirs) && subdirs) {
        owd <- setwd(dir)
        paths <- unique(dirname(list.files(".", recursive=TRUE)))
        setwd(owd)
        paths <- c("", paths[paths != "."])
    } else if(is.character(subdirs)) paths <- c("", subdirs)

    for(path in paths) {
        this <- if(nzchar(path)) file.path(dir, path) else dir
        desc <- .build_repository_package_db(this, fields, type, verbose,
                                             unpacked)

        if(length(desc)) {
            fields <- names(desc[[1L]])
            desc <- matrix(unlist(desc), ncol = length(fields), byrow = TRUE)
            colnames(desc) <- fields
            bundle <- !is.na(desc[,"Bundle"])
            desc[bundle, "Package"] <- desc[bundle, "Bundle"]

            ## Writing PACKAGES file from matrix desc linewise in order to
            ## omit NA entries appropriately:
            for(i in seq_len(nrow(desc))){
                desci <- desc[i, !(is.na(desc[i, ]) | (desc[i, ] == "")),
                              drop = FALSE]
                write.dcf(desci, file = out)
                if(nzchar(path)) cat("Path: ", path, "\n", sep="", file = out)
                cat("\n", file = out)
                write.dcf(desci, file = outgz)
                if(nzchar(path)) cat("Path: ", path, "\n", sep="", file = outgz)
                cat("\n", file = outgz)
            }
            nfields <- nfields + nrow(desc)
        }
    }

    close(out)
    close(outgz)
    invisible(nfields)
}

.build_repository_package_db <-
function(dir, fields = NULL,
         type = c("source", "mac.binary", "win.binary"),
         verbose = getOption("verbose"),
         unpacked = FALSE)
{
    if(unpacked)
        return(.build_repository_package_db_from_source_dirs(dir,
                                                             fields,
                                                             verbose))

    type <- match.arg(type)

    package_pattern <- switch(type,
                              "source" = "_.*\\.tar\\.gz$",
                              "mac.binary" = "_.*\\.tgz$",
                              "win.binary" = "_.*\\.zip$")
    files <- list.files(dir, pattern = package_pattern)

    if(!length(files))
        return(list())

    ## Add the standard set of fields required to build a repository's
    ## PACKAGES file:
    fields <- unique(c(.get_standard_repository_db_fields(), fields))
    packages <- sapply(strsplit(files, "_", fixed = TRUE), "[", 1L)
    db <- vector(length(files), mode = "list")
    ## Many (roughly length(files)) warnings are *expected*, hence
    ## suppressed.
    op <- options(warn = -1)
    on.exit(options(op))
    if(verbose) message("Processing packages:")
    if(type == "win.binary") {
        files <- file.path(dir, files)
        for(i in seq_along(files)) {
            if(verbose) message(paste(" ", files[i]))
            ## package zips have <name>/DESCRIPTION, rarer bundle zips do not.
            ## So try package case first.
            con <- unz(files[i], file.path(packages[i], "DESCRIPTION"))
            temp <- try(read.dcf(con, fields = fields)[1L, ], silent = TRUE)
            if(inherits(temp, "try-error")) {
                close(con)
                ## prior to 2.9.0 bundle zips had a top-level DESCRIPTION file
                con <- unz(files[i], "DESCRIPTION")
                temp <- try(read.dcf(con, fields = fields)[1L, ], silent = TRUE)
                if(inherits(temp, "try-error")) {
                    close(con)
                    ## otherwise look for the DESCRIPTION file of first package.
                    inzip <- as.character(unzip(files[i], list = TRUE)$Name)
                    d <- grepl("DESCRIPTION$", inzip)
                    if(any(d)) {
                        con <- unz(files[i], (inzip[d])[1])
                        temp <- try(read.dcf(con, fields = fields)[1L, ],
                                    silent = TRUE)
                    }
                    if(inherits(temp, "try-error")) {
                        close(con)
                        next
                    }
                }
            }
            db[[i]] <- temp
            close(con)
        }
    } else {
        dir <- file_path_as_absolute(dir)
        files <- file.path(dir, files)
        cwd <- getwd()
        td <- tempfile("PACKAGES")
        if(!dir.create(td)) stop("unable to create ", td)
        on.exit(unlink(td, recursive = TRUE), add = TRUE)
        setwd(td)
        for(i in seq_along(files)) {
            if(verbose) message(paste(" ", files[i]))
            p <- file.path(packages[i], "DESCRIPTION")
            temp <- try(system(paste("tar zxf", files[i], p)))
            if(!inherits(temp, "try-error")) {
                temp <- try(read.dcf(p, fields = fields)[1L, ],
                            silent = TRUE)
                if(!inherits(temp, "try-error"))
                    db[[i]] <- temp
            }
            unlink(packages[i], recursive = TRUE)
        }
        setwd(cwd)
    }
    if(verbose) message("done")

    db
}

.build_repository_package_db_from_source_dirs <-
function(dir, fields = NULL, verbose = getOption("verbose"))
{
    dir <- file_path_as_absolute(dir)
    fields <- unique(c(.get_standard_repository_db_fields(), fields))
    paths <- list.files(dir, full.names = TRUE)
    paths <- paths[file_test("-d", paths) &
                   file_test("-f", file.path(paths, "DESCRIPTION"))]
    db <- vector(length(paths), mode = "list")
    if(verbose) message("Processing packages:")
    for(i in seq_along(paths)) {
        if(verbose) message(paste(" ", basename(paths[i])))
        temp <- try(read.dcf(file.path(paths[i], "DESCRIPTION"),
                             fields = fields)[1L, ],
                    silent = TRUE)
        if(!inherits(temp, "try-error"))
            db[[i]] <- temp
    }
    if(verbose) message("done")
    names(db) <- basename(paths)
    db
}

dependsOnPkgs <-
function(pkgs,
         dependencies = c("Depends", "Imports"),
         recursive = TRUE,
         lib.loc = NULL,
         installed = installed.packages(lib.loc, fields = "Enhances"))
{
    need <- apply(installed[, dependencies, drop = FALSE], 1L,
                  function(x)
                  any(pkgs %in% utils:::.clean_up_dependencies(x)) )
    uses <- rownames(installed)[need]
    if(recursive) {
        p <- pkgs
        repeat {
            p <- unique(c(p, uses))
            need <- apply(installed[, dependencies, drop = FALSE], 1L,
                          function(x)
                          any(p %in% utils:::.clean_up_dependencies(x)) )
            uses <- unique(c(p, rownames(installed)[need]))
            if(length(uses) <= length(p)) break
        }
    }
    setdiff(uses, pkgs)
}
