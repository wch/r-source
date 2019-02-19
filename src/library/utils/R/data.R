#  File src/library/utils/R/data.R
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

data <-
function(..., list = character(), package = NULL, lib.loc = NULL,
         verbose = getOption("verbose"), envir = .GlobalEnv,
         overwrite = TRUE)
{
    fileExt <- function(x) {
        db <- grepl("\\.[^.]+\\.(gz|bz2|xz)$", x)
        ans <- sub(".*\\.", "", x)
        ans[db] <-  sub(".*\\.([^.]+\\.)(gz|bz2|xz)$", "\\1\\2", x[db])
        ans
    }

    names <- c(as.character(substitute(list(...))[-1L]), list)

    ## Find the directories of the given packages and maybe the working
    ## directory.
    if (!is.null(package)) {
        if (!is.character(package))
            stop("'package' must be a character string or NULL")
        if (any(package %in% "base"))
            warning("datasets have been moved from package 'base' to package 'datasets'")
        if (any(package %in% "stats"))
           warning("datasets have been moved from package 'stats' to package 'datasets'")
        package[package %in% c("base", "stats")] <- "datasets"
    }
    paths <- find.package(package, lib.loc, verbose = verbose)
    if (is.null(lib.loc))
        paths <- c(path.package(package, TRUE),
                   if (!length(package)) getwd(), # ignored if NULL
                   paths)
    paths <- unique(normalizePath(paths[file.exists(paths)]))

    ## Find the directories with a 'data' subdirectory.
    paths <- paths[dir.exists(file.path(paths, "data"))]

    dataExts <- tools:::.make_file_exts("data")

    if (length(names) == 0L) {
        ## List all possible data sets.

        ## Build the data db.
        db <- matrix(character(), nrow = 0L, ncol = 4L)
        for(path in paths) {
            entries <- NULL
            ## Use "." as the 'package name' of the working directory.
            packageName <-
                if (file_test("-f", file.path(path, "DESCRIPTION")))
                    basename(path)
                else
                    "."
            ## Check for new-style 'Meta/data.rds'
            if (file_test("-f", INDEX <- file.path(path, "Meta", "data.rds"))) {
                entries <- readRDS(INDEX)
            } else {
                ## No index: should only be true for ./data >= 2.0.0
                dataDir <- file.path(path, "data")
                entries <- tools::list_files_with_type(dataDir, "data")
                if (length(entries)) {
                    entries <-
                        unique(tools::file_path_sans_ext(basename(entries)))
                    entries <- cbind(entries, "")
                }
            }
            if (NROW(entries)) {
                if (is.matrix(entries) && ncol(entries) == 2L)
                    db <- rbind(db, cbind(packageName, dirname(path), entries))
                else
                    warning(gettextf("data index for package %s is invalid and will be ignored",
                                     sQuote(packageName)),
                            domain=NA, call.=FALSE)
            }
        }
        colnames(db) <- c("Package", "LibPath", "Item", "Title")

        footer <- if (missing(package))
            paste0("Use ",
                   sQuote(paste("data(package =",
                                ".packages(all.available = TRUE))")),
                   "\n",
                   "to list the data sets in all *available* packages.")
        else
            NULL
        y <- list(title = "Data sets", header = NULL, results = db,
                  footer = footer)
        class(y) <- "packageIQR"
        return(y)
    }

    tmp_env <- if (overwrite) envir else new.env()
    paths <- file.path(paths, "data")
    for(name in names) {
        if (!overwrite && exists(name, envir = envir, inherits = FALSE)) {
            warning(gettextf("an object named %s already exists and will not be overwritten", sQuote(name)))
            next
        }

        found <- FALSE
        for(p in paths) {
            ## does this package have "Rdata" databases?
            if (file_test("-f", file.path(p, "Rdata.rds"))) {
                rds <- readRDS(file.path(p, "Rdata.rds"))
                if (name %in% names(rds)) {
                    ## found it, so copy objects from database
                    found <- TRUE
                    if (verbose)
                        message(sprintf("name=%s:\t found in Rdata.rds", name),
                                domain=NA)
                    thispkg <- sub(".*/([^/]*)/data$", "\\1", p)
                    thispkg <- sub("_.*$", "", thispkg) # versioned installs.
                    thispkg <- paste0("package:", thispkg)
                    objs <- rds[[name]] # guaranteed an exact match
                    lazyLoad(file.path(p, "Rdata"), envir = tmp_env,
                             filter = function(x) x %in% objs)
                    break
		} else if (verbose)
		    message(sprintf("name=%s:\t NOT found in names() of Rdata.rds, i.e.,\n\t%s\n",
				    name, paste(names(rds), collapse=",")),
				domain=NA)
            }
            ## check for zipped data dir
            if (file_test("-f", file.path(p, "Rdata.zip"))) {
                warning("zipped data found for package ",
                        sQuote(basename(dirname(p))),
                        ".\nThat is defunct, so please re-install the package.",
                        domain = NA)
                if (file_test("-f", fp <- file.path(p, "filelist")))
                    files <- file.path(p, scan(fp, what = "", quiet = TRUE))
                else {
                    warning(gettextf("file 'filelist' is missing for directory %s", sQuote(p)), domain = NA)
                    next
                }
            } else {
                files <- list.files(p, full.names = TRUE)
            }
            files <- files[grep(name, files, fixed = TRUE)]
            if (length(files) > 1L) {
                ## more than one candidate
                o <- match(fileExt(files), dataExts, nomatch = 100L)
                paths0 <- dirname(files)
		## Next line seems unnecessary to MM (FIXME?)
		paths0 <- factor(paths0, levels = unique(paths0))
                files <- files[order(paths0, o)]
            }
            if (length(files)) {
                ## have a plausible candidate (or more)
                for(file in files) {
                    if (verbose)
                        message("name=", name, ":\t file= ...",
                                .Platform$file.sep, basename(file), "::\t",
                                appendLF = FALSE, domain = NA)
                    ext <- fileExt(file)
                    ## make sure the match is really for 'name.ext'
                    if (basename(file) != paste0(name, ".", ext))
                        found <- FALSE
                    else {
                        found <- TRUE
                        zfile <- file
                        zipname <- file.path(dirname(file), "Rdata.zip")
                        if (file.exists(zipname)) {
                            Rdatadir <- tempfile("Rdata")
                            dir.create(Rdatadir, showWarnings=FALSE)
                            topic <- basename(file)
                            rc <- .External(C_unzip, zipname, topic, Rdatadir, FALSE, TRUE, FALSE, FALSE)
                            if (rc == 0L) zfile <- file.path(Rdatadir, topic)
                        }
                        if (zfile != file) on.exit(unlink(zfile))
                        switch(ext,
                               R = , r = {
                                   ## ensure utils is visible
                                   library("utils")
                                   sys.source(zfile, chdir = TRUE,
                                              envir = tmp_env)
                               },
                               RData = , rdata = , rda =
                               load(zfile, envir = tmp_env),
                               TXT = , txt = , tab = ,
                               tab.gz = , tab.bz2 = , tab.xz = ,
                               txt.gz = , txt.bz2 = , txt.xz =
                               assign(name,
                                      ## ensure default for as.is has not been
                                      ## overridden by options(stringsAsFactor)
                                      read.table(zfile, header = TRUE, as.is = FALSE),
                                      envir = tmp_env),
                               CSV = , csv = ,
                               csv.gz = , csv.bz2 = , csv.xz =
                               assign(name,
                                      read.table(zfile, header = TRUE,
                                                 sep = ";", as.is = FALSE),
                                      envir = tmp_env),
                               found <- FALSE)
                    }
                    if (found) break # from files
                }
                if (verbose) message(if (!found) "*NOT* ", "found", domain = NA)
            }
            if (found) break # from paths
        }

        if (!found)
            warning(gettextf("data set %s not found", sQuote(name)),
                    domain = NA)
        else if (!overwrite)
            assign(name, get(name, envir = tmp_env, inherits = FALSE),
                   envir = envir)
    }
    invisible(names)
}
