#  File src/library/tools/R/packages.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2022 The R Core Team
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

write_PACKAGES <-
function(dir = ".", fields = NULL,
         type = c("source", "mac.binary", "win.binary"),
         verbose = FALSE, unpacked = FALSE, subdirs = FALSE,
         latestOnly = TRUE, addFiles = FALSE, rds_compress = "xz",
         validate = FALSE)
{
    if(missing(type) && .Platform$OS.type == "windows")
        type <- "win.binary"
    type <- match.arg(type)

    paths <- ""
    if(is.logical(subdirs) && subdirs) {
        owd <- setwd(dir)
        paths <- list.dirs(".")
        setwd(owd)
        paths <- c("", paths[paths != "."])
        ## now strip leading ./
        paths <- sub("^[.]/", "", paths)
    } else if(is.character(subdirs)) paths <- c("", subdirs)

    ## Older versions created only plain text and gzipped DCF files with
    ## the (non-missing and non-empty) package db entries, and hence did
    ## so one path at a time.  We now also serialize the db directly,
    ## and hence first build the whole db, and then create the files in
    ## case some packages were found.

    db <- NULL
    addPaths <- !identical(paths, "")

    for(path in paths) {
        this <- if(nzchar(path)) file.path(dir, path) else dir
        desc <- .build_repository_package_db(this, fields, type, verbose,
                                             unpacked, validate)
        desc <- .process_repository_package_db_to_matrix(desc,
                                                         path,
                                                         addFiles,
                                                         addPaths,
                                                         latestOnly)
        if(NROW(desc))
            db <- rbind(db, desc)

    }

    np <- .write_repository_package_db(db, dir, rds_compress)

    invisible(np)
}

.write_repository_package_db <-
function(db, dir, rds_compress)
{
   np <- NROW(db)
   if(np > 0L) {
       ## To save space, empty entries are not written to the DCF, so
       ## that read.dcf() on these will have the entries as missing.
       ## Hence, change empty to missing in the db.
       db[!is.na(db) & (db == "")] <- NA_character_
       con <- file(file.path(dir, "PACKAGES"), "wt")
       write.dcf(db, con)
       close(con)
       con <- gzfile(file.path(dir, "PACKAGES.gz"), "wt")
       write.dcf(db, con)
       close(con)
       rownames(db) <- db[, "Package"]
       saveRDS(db, file.path(dir, "PACKAGES.rds"), compress = rds_compress)
   }

   invisible(np)
}

.process_repository_package_db_to_matrix <-
function(desc, path, addFiles, addPaths, latestOnly)
{
    desc <- Filter(length, desc)

    if(length(desc)) {
        Files <- names(desc)
        fields <- names(desc[[1L]])
        desc <- matrix(unlist(desc), ncol = length(fields), byrow = TRUE)
        colnames(desc) <- fields
        if(addFiles) desc <- cbind(desc, File = Files)
        if(addPaths) desc <- cbind(desc, Path = path)
        if(latestOnly) desc <- .remove_stale_dups(desc)

        ## Standardize licenses or replace by NA.
        license_info <- analyze_licenses(desc[, "License"])
            desc[, "License"] <-
                ifelse(license_info$is_standardizable,
                       license_info$standardization,
                       NA)
        }
    desc
}

## factored out so it can be used in multiple
## places without threat of divergence
.get_pkg_file_pattern = function(type = c("source", "mac.binary", "win.binary"),
                                 ext.only = FALSE)
{

    type <- match.arg(type)
    ## FIXME: might the source pattern be more general?
    ## was .tar.gz prior to 2.10.0

    ret = switch(type,
                 "source" = "_.*\\.tar\\.[^_]*$",
                 "mac.binary" = "_.*\\.tgz$",
                 "win.binary" = "_.*\\.zip$")
    if(ext.only)
        ret = gsub("_.*", "", fixed = TRUE, ret)
    ret
}
## this is OK provided all the 'fields' are ASCII -- so be careful
## what you add.
.build_repository_package_db <-
function(dir, fields = NULL,
         type = c("source", "mac.binary", "win.binary"),
         verbose = getOption("verbose"),
         unpacked = FALSE, validate = FALSE)
{
    if(unpacked)
        return(.build_repository_package_db_from_source_dirs(dir,
                                                             fields,
                                                             verbose,
                                                             validate))

       package_pattern <- .get_pkg_file_pattern(type)
    files <- list.files(dir, pattern = package_pattern, full.names = TRUE)

    if(!length(files))
        return(list())
    type <- match.arg(type)
    db <- .process_package_files_for_repository_db(files,
                                                   type,
                                                   fields,
                                                   verbose,
                                                   validate)
    db
}

.process_package_files_for_repository_db <-
function(files, type, fields, verbose, validate = FALSE)
{

    files <- normalizePath(files, mustWork=TRUE) # files comes from list.files, mustWork ok
    ## Add the standard set of fields required to build a repository's
    ## PACKAGES file:
    fields <- unique(c(.get_standard_repository_db_fields(type), fields))
    ## files was without path at this point in original code,
    ## use filetbs instead to compute pkg names and set db names
    filetbs <- basename(files)
    packages <- sapply(strsplit(filetbs, "_", fixed = TRUE), `[`, 1L)
    db <- vector(length(files), mode = "list")
    names(db) <- filetbs #files was not full paths before
    ## Many (roughly length(files)) warnings are *expected*, hence
    ## suppressed.
    op <- options(warn = -1)
    on.exit(options(op))
    if(verbose) message("Processing packages:")
    if(type == "win.binary") {
        for(i in seq_along(files)) {
            if(verbose) message(paste0("  ", files[i]))
            con <- unz(files[i], file.path(packages[i], "DESCRIPTION"))
            temp <- tryCatch(read.dcf(con, fields = fields)[1L, ],
                             error = identity)
            if(inherits(temp, "error")) {
                close(con)
                next
            }
            db[[i]] <- temp
            close(con)
        }
    } else {
        cwd <- getwd()
        if (is.null(cwd))
            stop("current working directory cannot be ascertained")
        td <- tempfile("PACKAGES")
        if(!dir.create(td)) stop("unable to create ", td)
        on.exit(unlink(td, recursive = TRUE), add = TRUE)
        setwd(td)
        for(i in seq_along(files)) {
            if(verbose) message(paste0("  ", files[i]))
            p <- file.path(packages[i], "DESCRIPTION")
            ## temp <- try(system(paste("tar zxf", files[i], p)))
            temp <- try(utils::untar(files[i], files = p))
            if(!inherits(temp, "try-error")) {
                temp <- tryCatch(read.dcf(p, fields = fields)[1L, ],
                                 error = identity)
                if(!inherits(temp, "error")) {
                    if(validate) {
                        ## .check_package_description() by default goes via
                        ## .read_description() which re-encodes and insists on a
                        ## single entry unlike the above read.dcf() call.
                        ok <- .check_package_description(db = temp[!is.na(temp)])
                        ## FIXME: no format.check_package_description yet.
                        if(any(as.integer(lengths(ok)) > 0L)) {
                            message(paste(gettextf("Invalid DESCRIPTION file for package %s",
                                                   sQuote(basename(dirname(p)))),
                                          paste(format(ok), collapse = "\n\n"),
                                          sep = "\n\n"),
                                    domain = NA)
                            next
                        }
                    }
                    if("NeedsCompilation" %in% fields &&
                       is.na(temp["NeedsCompilation"])) {
                        l <- utils::untar(files[i], list = TRUE)
                        temp["NeedsCompilation"] <-
                            if(any(l == file.path(packages[i], "src/"))) "yes" else "no"
                    }
                    temp["MD5sum"] <- md5sum(files[i])
                    db[[i]] <- temp
                } else {
                    message(gettextf("reading DESCRIPTION for package %s failed with message:\n  %s",
                                     sQuote(basename(dirname(p))),
                                     conditionMessage(temp)),
                            domain = NA)
                }
            }
            unlink(packages[i], recursive = TRUE)
        }
        setwd(cwd)
    }
    if(verbose) message("done")

    db
}

.build_repository_package_db_from_source_dirs <-
function(dir, fields = NULL, verbose = getOption("verbose"),
         validate = FALSE)
{
    dir <- file_path_as_absolute(dir)
    fields <- unique(c(.get_standard_repository_db_fields(), fields))
    paths <- list.files(dir, full.names = TRUE)
    paths <- paths[dir.exists(paths) &
                   file_test("-f", file.path(paths, "DESCRIPTION"))]
    db <- vector(length(paths), mode = "list")
    if(verbose) message("Processing packages:")
    for(i in seq_along(paths)) {
        if(verbose) message(paste0("  ", basename(paths[i])))
        temp <- tryCatch(read.dcf(file.path(paths[i], "DESCRIPTION"),
                                  fields = fields)[1L, ],
                         error = identity)
        if(!inherits(temp, "error")) {
            if(validate) {
                ## .check_package_description() by default goes via
                ## .read_description() which re-encodes and insists on a
                ## single entry unlike the above read.dcf() call.
                ok <- .check_package_description(db = temp[!is.na(temp)])
                ## FIXME: no format.check_package_description yet.
                if(any(as.integer(lengths(ok)) > 0L)) {
                    warning(paste(gettextf("Invalid DESCRIPTION file for package %s",
                                           sQuote(basename(paths[i]))),
                                  paste(format(ok), collapse = "\n\n"),
                                  sep = "\n\n"),
                            domain = NA,
                            call. = FALSE)
                    next
                }
            }
            if(is.na(temp["NeedsCompilation"])) {
                temp["NeedsCompilation"] <-
                    if(dir.exists(file.path(paths[i], "src"))) "yes" else "no"
            }
            ## Cannot compute MD5 sum of the source tar.gz when working
            ## on the unpacked sources ...
            db[[i]] <- temp
        } else {
            warning(gettextf("reading DESCRIPTION for package %s failed with message:\n  %s",
                             sQuote(basename(paths[i])),
                             conditionMessage(temp)),
                    domain = NA)
        }
    }
    if(verbose) message("done")
    names(db) <- basename(paths)
    db
}

dependsOnPkgs <-
function(pkgs, dependencies = "strong",
         recursive = TRUE, lib.loc = NULL,
         installed = utils::installed.packages(lib.loc, fields = "Enhances"))
{
    dependencies <- .expand_dependency_type_spec(dependencies)

    av <- installed[, dependencies, drop = FALSE]
    rn <- as.character(installed[, "Package"])
    need <- apply(av, 1L, function(x)
                  any(pkgs %in% utils:::.clean_up_dependencies(x)) )
    uses <- rn[need]
    if(recursive) {
        p <- pkgs
        repeat {
            p <- unique(c(p, uses))
            need <- apply(av, 1L, function(x)
                          any(p %in% utils:::.clean_up_dependencies(x)) )
            uses <- unique(c(p, rn[need]))
            if(length(uses) <= length(p)) break
        }
    }
    setdiff(uses, pkgs)
}

.remove_stale_dups <-
function(ap)
{
    ## Given a matrix from available.packages, return a copy
    ## with no duplicate packages, being sure to keep the packages
    ## with highest version number.
    ## (Also works for data frame package repository dbs.)
    pkgs <- ap[ , "Package"]
    dup_pkgs <- pkgs[duplicated(pkgs)]
    if (length(dup_pkgs) > 100) {
        ## Some packages may be in multiple repositories in the same
        ## version. Handle those specially for performance reasons.
        ap <- ap[!duplicated(ap[, c("Package", "Version")]), , drop = FALSE]
        pkgs <- ap[ , "Package"]
        dup_pkgs <- pkgs[duplicated(pkgs)]
    }
    stale_dups <- integer(length(dup_pkgs))
    i <- 1L
    for (dp in dup_pkgs) {
        wh <- which(dp == pkgs)
        vers <- package_version(ap[wh, "Version"])
        keep_ver <- max(vers)
	keep_idx <- which.max(vers == keep_ver) # they might all be max
        wh <- wh[-keep_idx]
        end_i <- i + length(wh) - 1L
        stale_dups[i:end_i] <- wh
        i <- end_i + 1L
    }
    ## Possible to have only one package in a repository
    if(length(stale_dups)) ap[-stale_dups, , drop = FALSE] else ap
}

package_dependencies <-
function(packages = NULL, db = NULL, which = "strong",
         recursive = FALSE, reverse = FALSE,
         verbose = getOption("verbose"))
{
    packages1 <- unique(packages)

    if(is.null(db)) db <- utils::available.packages()

    fields <- which <- .expand_dependency_type_spec(which)
    if(is.character(recursive)) {
        recursive <- .expand_dependency_type_spec(recursive)
        if(identical(which, recursive))
            recursive <- TRUE
        else
            fields <- unique(c(fields, recursive))
    }

    ind <- if(!is.character(recursive) && !recursive && !reverse &&
              !is.null(packages)) {
               ## For forward non-recursive depends, we can simplify
               ## matters by subscripting the db right away---modulo
               ## boundary cases.
               match(packages1, db[, "Package"], nomatch = 0L)
           } else !duplicated(db[, "Package"])

    db <- as.data.frame(db[ind, c("Package", fields), drop = FALSE])

    ## Avoid recomputing package dependency names in recursive
    ## invocations.
    for(f in fields) {
        if(!is.list(d <- db[[f]]))
            db[[f]] <- lapply(d, .extract_dependency_package_names)
    }

    if(is.character(recursive)) {
        ## Direct dependencies:
        d_d <- Recall(packages, db, which, FALSE,
                      reverse, verbose)
        ## Recursive dependencies of all these:
        d_r <- Recall(unique(unlist(d_d)), db, recursive, TRUE,
                      reverse, verbose)
        ## Now glue together:
        return(lapply(d_d,
                      function(p) {
                          sort(unique(c(p, unlist(d_r[p],
                                                  use.names = FALSE))))
                      }))
    }

    depends <-
        do.call(Map,
                c(list("c"),
                  db[which],
                  list(USE.NAMES = FALSE)))

    depends <- lapply(depends, unique)

    if(!recursive && !reverse) {
        names(depends) <- db$Package
        if(!is.null(packages)) {
            depends <- depends[match(packages, names(depends))]
            names(depends) <- packages
        }
        return(depends)
    }

    all_packages <- sort(unique(c(db$Package, unlist(depends))))

    if(!recursive) {
        ## Need to invert.
        depends <-
            split(rep.int(db$Package, lengths(depends)),
                  factor(unlist(depends), levels = all_packages))
        if(!is.null(packages)) {
            depends <- depends[match(packages, names(depends))]
            names(depends) <- packages
        }
        return(depends)
    }

    ## Recursive dependencies.
    ## We need to compute the transitive closure of the dependency
    ## relation, but e.g. Warshall's algorithm (O(n^3)) is
    ## computationally infeasible.
    ## Hence, in principle, we do the following.
    ## Take the current list of pairs (i,j) in the relation.
    ## Iterate over all j and whenever i R j and j R k add (i,k).
    ## Repeat this until no new pairs get added.
    ## To do this in R, we use a 2-column matrix of (i,j) rows.
    ## We then create two lists which for all j contain the i and k
    ## with i R j and j R k, respectively, and combine these.
    ## This works reasonably well, but of course more efficient
    ## implementations should be possible.
    matchP <- match(rep.int(db$Package, lengths(depends)),
		    all_packages)
    matchD <- match(unlist(depends), all_packages)
    tab <- if(reverse)
	split(matchP,
	      factor(matchD, levels = seq_along(all_packages)))
    else
	split(matchD,
	      factor(matchP, levels = seq_along(all_packages)))
    if(is.null(packages)) {
        if(reverse) {
            packages1 <- all_packages
            p_L <- seq_along(all_packages)
        } else {
            packages1 <- db$Package
            p_L <- match(packages1, all_packages)
        }
    } else {
        p_L <- match(packages1, all_packages, nomatch = 0L)
        if(any(ind <- (p_L == 0L))) {
            p_L <- p_L[!ind]
        }
    }
    p_R <- tab[p_L]
    pos <- cbind(rep.int(p_L, lengths(p_R)), unlist(p_R))
    ctr <- 0L

    ## posunique() speeds up computing "unique(pos)" in the following loop.
    ## When the number of packages is small enough, we can easily represent
    ## an edge from i to j by a single integer number. Finding duplicates in
    ## a vector of integers is much faster than in rows of a matrix.
    shift <- as.integer(2^15)  ## allows to fit two numbers to an integer
    if (length(pos) && max(pos) < shift)
        posunique <- function(p)
            p[!duplicated(p[,1L]*shift + p[,2L]), , drop = FALSE]
    else
        posunique <- function(p) unique(p)

    repeat {
        if(verbose) cat("Cycle:", (ctr <- ctr + 1L))
        p_L <- split(pos[, 1L], pos[, 2L])
        new <- do.call(rbind,
                       Map(function(i, k)
                           cbind(rep.int(i, length(k)),
                                 rep(k, each = length(i))),
                           p_L, tab[as.integer(names(p_L))]))

        ## could be just posunique(rbind(pos, new)), but computing this
        ## iteratively is faster
        npos <- posunique(rbind(pos, posunique(new)))
        nnew <- nrow(npos) - nrow(pos)
        if(verbose) cat(" NNew:", nnew, "\n")
        if(!nnew) break
        pos <- npos
    }
    depends <-
        split(all_packages[pos[, 2L]],
              factor(all_packages[pos[, 1L]], levels = packages1))
    if(!is.null(packages)) {
        depends <- depends[match(packages, names(depends))]
        names(depends) <- packages
    }
    depends
}

.expand_dependency_type_spec <-
function(x)
{
    if(identical(x, "strong"))
        c("Depends", "Imports", "LinkingTo")
    else if(identical(x, "most"))
        c("Depends", "Imports", "LinkingTo", "Suggests")
    else if(identical(x, "all"))
        c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances")
    else
        x
    ## (Could also intersect x with the possible types.)
}

## .extract_dependency_package_names <-
## function(x)
## {
##     ## Assume a character *string*.
##     if(is.na(x)) return(character())
##     x <- strsplit(x, ",", fixed = TRUE)[[1L]]
##     ## FIXME: The following is much faster on Linux but apparently not
##     ## on Windows:
##     ## x <- sub("(?s)[[:space:]]*([[:alnum:].]+).*", "\\1", x, perl = TRUE)
##     x <- sub("[[:space:]]*([[:alnum:].]+).*", "\\1", x)
##     x[nzchar(x) & (x != "R")]
## }

.extract_dependency_package_names <-
function(x)
    .Call(C_package_dependencies_scan, x)
