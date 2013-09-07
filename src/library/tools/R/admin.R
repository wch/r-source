#  File src/library/tools/R/admin.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1995-2013 The R Core Team
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


### * .install_package_description

## called from basepkg.mk and .install_packages
.install_package_description <-
function(dir, outDir)
{
    ## Function for taking the DESCRIPTION package meta-information,
    ## checking/validating it, and installing it with the 'Built:'
    ## field added.  Note that from 1.7.0 on, packages without
    ## compiled code are not marked as being from any platform.

    ## Check first.  Note that this also calls .read_description(), but
    ## .check_package_description() currently really needs to know the
    ## path to the DESCRIPTION file, and returns an object with check
    ## results and not the package metadata ...
    ok <- .check_package_description(file.path(dir, "DESCRIPTION"))
    if(any(as.integer(sapply(ok, length)) > 0L)) {
        stop(paste(gettext("Invalid DESCRIPTION file") ,
                   paste(.eval_with_capture(print(ok))$output,
                         collapse = "\n"),
                   sep = "\n\n"),
             domain = NA,
             call. = FALSE)
    }

    ## This reads (in C locale) byte-by-byte, declares latin1 or UTF-8
    ## Maybe it would be better to re-encode others (there are none at
    ## present, at least in a UTF-8 locale?
    db <- .read_description(file.path(dir, "DESCRIPTION"))

    ## should not have a Built: field, so ignore it if it is there
    nm <- names(db)
    if("Built" %in% nm) {
        db <- db[-match("Built", nm)]
        warning(gettextf("*** someone has corrupted the Built field in package '%s' ***",
                         db["Package"]),
                domain = NA,
                call. = FALSE)
    }

    OStype <- R.version$platform
    if (length(grep("-apple-darwin", R.version$platform)) &&
        nzchar(Sys.getenv("R_ARCH")))
        OStype <- sub(".*-apple-darwin", "universal-apple-darwin", OStype)
    Built <-
	paste0("R ",
	       paste(R.version[c("major", "minor")], collapse = "."),
	       "; ",
	       if(file_test("-d", file.path(dir, "src"))) OStype else "",
	       "; ",
	       ## Prefer date in ISO 8601 format, UTC.
	       format(Sys.time(), tz = "UTC", usetz = TRUE),
	       ## Sys.time(),
	       "; ",
	       .OStype())

    ## At some point of time, we had:
    ##   We must not split the Built: field across lines.
    ## Not sure if this is still true.  If not, the following could be
    ## simplified to
    ##   db["Built"] <- Built
    ##   write.dcf(rbind(db), file.path(outDir, "DESCRIPTION"))
    ## But in any case, it is true for fields obtained from expanding R
    ## fields (Authors@R): these should not be reformatted.

    db <- c(db,
            .expand_package_description_db_R_fields(db),
            Built = Built)

    ## This cannot be done in a MBCS: write.dcf fails
    ctype <- Sys.getlocale("LC_CTYPE")
    Sys.setlocale("LC_CTYPE", "C")
    on.exit(Sys.setlocale("LC_CTYPE", ctype))
    .write_description(db, file.path(outDir, "DESCRIPTION"))

    outMetaDir <- file.path(outDir, "Meta")
    if(!file_test("-d", outMetaDir) && !dir.create(outMetaDir))
         stop(gettextf("cannot open directory '%s'",
                       outMetaDir),
              domain = NA)
    saveInfo <- .split_description(db)
    saveRDS(saveInfo, file.path(outMetaDir, "package.rds"))

    invisible()
}

### * .split_description

## also used in .getRequiredPackages
.split_description <-
function(db, verbose = FALSE)
{
    if(!is.na(Built <- db["Built"])) {
        Built <- as.list(strsplit(Built, "; ")[[1L]])
        if(length(Built) != 4L) {
            warning(gettextf("*** someone has corrupted the Built field in package '%s' ***",
                             db["Package"]),
                    domain = NA,
                    call. = FALSE)
            Built <- NULL
        } else {
            names(Built) <- c("R", "Platform", "Date", "OStype")
            Built[["R"]] <- R_system_version(sub("^R ([0-9.]+)", "\\1",
                                                 Built[["R"]]))
        }
    } else Built <- NULL
    ## might perhaps have multiple entries
    Depends <- .split_dependencies(db[names(db) %in% "Depends"])
    ## several packages 'Depends' on base!
    ind <- match("base", names(Depends), 0L)
    if(ind) Depends <- Depends[-ind]
    ## We only need Rdepends for R < 2.7.0, but we still need to be
    ## able to check that someone is not trying to load this into a
    ## very old version of R.
    if("R" %in% names(Depends)) {
        Rdeps2 <- Depends["R" == names(Depends)]
        names(Rdeps2) <- NULL
        Rdeps <- Depends[["R", exact = TRUE]] # the first one
        Depends <- Depends[names(Depends) != "R"]
        ## several packages have 'Depends: R', which is a noop.
        if(verbose && length(Rdeps) == 1L)
             message("WARNING: omitting pointless dependence on 'R' without a version requirement")
        if(length(Rdeps) <= 1L) Rdeps <- NULL
    } else Rdeps2 <- Rdeps <- NULL
    Rdeps <- as.vector(Rdeps)
    Suggests <- .split_dependencies(db[names(db) %in% "Suggests"])
    Imports <- .split_dependencies(db[names(db) %in% "Imports"])
    LinkingTo <- .split_dependencies(db[names(db) %in% "LinkingTo"])
    structure(list(DESCRIPTION = db, Built = Built,
                   Rdepends = Rdeps, Rdepends2 = Rdeps2,
                   Depends = Depends, Suggests = Suggests,
                   Imports = Imports, LinkingTo = LinkingTo),
              class = "packageDescription2")
}

### * .vinstall_package_descriptions_as_RDS

## called from src/library/Makefile
.vinstall_package_descriptions_as_RDS <-
function(dir, packages)
{
    ## For the given packages installed in @file{dir}, install their
    ## DESCRIPTION package metadata as R metadata.
    ## Really only useful for base packages under Unix.
    ## See @file{src/library/Makefile.in}.

    for(p in unlist(strsplit(packages, "[[:space:]]+"))) {
        meta_dir <- file.path(dir, p, "Meta")
        if(!file_test("-d", meta_dir) && !dir.create(meta_dir))
            stop(gettextf("cannot open directory '%s'", meta_dir))
        package_info_dcf_file <- file.path(dir, p, "DESCRIPTION")
        package_info_rds_file <- file.path(meta_dir, "package.rds")
        if(file_test("-nt",
                     package_info_rds_file,
                     package_info_dcf_file))
            next
        saveRDS(.split_description(.read_description(package_info_dcf_file)),
                 package_info_rds_file)
    }
    invisible()
}

### * .update_package_rds

## not used
.update_package_rds <-
function(lib.loc = NULL)
{
    ## rebuild the dumped package descriptions for all packages in lib.loc
    if (is.null(lib.loc)) lib.loc <- .libPaths()
    lib.loc <- lib.loc[file.exists(lib.loc)]
    for (lib in lib.loc) {
        a <- list.files(lib, all.files = FALSE, full.names = TRUE)
        for (nam in a) {
            dfile <- file.path(nam, "DESCRIPTION")
            if (file.exists(dfile)) {
                print(nam)
                .install_package_description(nam, nam)
            }
        }
    }
}

### * .install_package_code_files

.install_package_code_files <-
function(dir, outDir)
{
    if(!file_test("-d", dir))
        stop(gettextf("directory '%s' does not exist", dir),
             domain = NA)
    dir <- file_path_as_absolute(dir)

    ## Attempt to set the LC_COLLATE locale to 'C' to turn off locale
    ## specific sorting.
    curLocale <- Sys.getlocale("LC_COLLATE")
    on.exit(Sys.setlocale("LC_COLLATE", curLocale), add = TRUE)
    ## (Guaranteed to work as per the Sys.setlocale() docs.)
    lccollate <- "C"
    if(Sys.setlocale("LC_COLLATE", lccollate) != lccollate) {
        ## <NOTE>
        ## I don't think we can give an error here.
        ## It may be the case that Sys.setlocale() fails because the "OS
        ## reports request cannot be honored" (src/main/platform.c), in
        ## which case we should still proceed ...
        warning("cannot turn off locale-specific sorting via LC_COLLATE")
        ## </NOTE>
    }

    ## We definitely need a valid DESCRIPTION file.
    db <- .read_description(file.path(dir, "DESCRIPTION"))

    codeDir <- file.path(dir, "R")
    if(!file_test("-d", codeDir)) return(invisible())

    codeFiles <- list_files_with_type(codeDir, "code", full.names = FALSE)

    collationField <-
        c(paste("Collate", .OStype(), sep = "."), "Collate")
    if(any(i <- collationField %in% names(db))) {
        collationField <- collationField[i][1L]
        codeFilesInCspec <- .read_collate_field(db[collationField])
        ## Duplicated entries in the collation spec?
        badFiles <-
            unique(codeFilesInCspec[duplicated(codeFilesInCspec)])
        if(length(badFiles)) {
            out <- gettextf("\nduplicated files in '%s' field:",
                            collationField)
            out <- paste(out,
                         paste(" ", badFiles, collapse = "\n"),
                         sep = "\n")
            stop(out, domain = NA)
        }
        ## See which files are listed in the collation spec but don't
        ## exist.
        badFiles <- setdiff(codeFilesInCspec, codeFiles)
        if(length(badFiles)) {
            out <- gettextf("\nfiles in '%s' field missing from '%s':",
                            collationField,
                            codeDir)
            out <- paste(out,
                         paste(" ", badFiles, collapse = "\n"),
                         sep = "\n")
            stop(out, domain = NA)
        }
        ## See which files exist but are missing from the collation
        ## spec.  Note that we do not want the collation spec to use
        ## only a subset of the available code files.
        badFiles <- setdiff(codeFiles, codeFilesInCspec)
        if(length(badFiles)) {
            out <- gettextf("\nfiles in '%s' missing from '%s' field:",
                            codeDir,
                            collationField)
            out <- paste(out,
                         paste(" ", badFiles, collapse = "\n"),
                         sep = "\n")
            stop(out, domain = NA)
        }
        ## Everything's groovy ...
        codeFiles <- codeFilesInCspec
    }

    codeFiles <- file.path(codeDir, codeFiles)

    if(!file_test("-d", outDir) && !dir.create(outDir))
        stop(gettextf("cannot open directory '%s'", outDir),
             domain = NA)
    outCodeDir <- file.path(outDir, "R")
    if(!file_test("-d", outCodeDir) && !dir.create(outCodeDir))
        stop(gettextf("cannot open directory '%s'", outCodeDir),
             domain = NA)
    outFile <- file.path(outCodeDir, db["Package"])
    if(!file.create(outFile))
        stop(gettextf("unable to create '%s'", outFile), domain = NA)
    writeLines(paste0(".packageName <- \"", db["Package"], "\""),
               outFile)
    enc <- as.vector(db["Encoding"])
    need_enc <- !is.na(enc) # Encoding was specified
    ## assume that if locale is 'C' we can used 8-bit encodings unchanged.
    if(need_enc && !(Sys.getlocale("LC_CTYPE") %in% c("C", "POSIX"))) {
        con <- file(outFile, "a")
        on.exit(close(con))  # Windows does not like files left open
        for(f in codeFiles) {
            tmp <- iconv(readLines(f, warn = FALSE), from = enc, to = "")
            if(length(bad <- which(is.na(tmp)))) {
                warning(sprintf(ngettext(length(bad),
                                         "unable to re-encode %s line %s",
                                         "unable to re-encode %s lines %s"),
                                sQuote(basename(f)),
                                paste(bad, collapse = ", ")),
                        domain = NA, call. = FALSE)
                tmp <- iconv(readLines(f, warn = FALSE), from = enc, to = "",
                             sub = "byte")
            }
            writeLines(paste0("#line 1 \"", f, "\""), con)
            writeLines(tmp, con)
        }
	close(con); on.exit()
    } else {
        ## <NOTE>
        ## It may be safer to do
        ##   writeLines(sapply(codeFiles, readLines), outFile)
        ## instead, but this would be much slower ...
        ## use fast version of file.append that ensures LF between files
        if(!all(.file_append_ensuring_LFs(outFile, codeFiles)))
            stop("unable to write code files")
        ## </NOTE>
    }
    ## A syntax check here, so that we do not install a broken package.
    ## FIXME:  this is only needed if we don't lazy load, as the lazy loader
    ## would detect the error.
    op <- options(showErrorCalls=FALSE)
    on.exit(options(op))
    parse(outFile)
    invisible()
}


### * .install_package_indices
## called from R CMD INSTALL

.install_package_indices <-
function(dir, outDir)
{
    options(warn = 1)                   # to ensure warnings get seen
    if(!file_test("-d", dir))
        stop(gettextf("directory '%s' does not exist", dir),
             domain = NA)
    if(!file_test("-d", outDir))
        stop(gettextf("directory '%s' does not exist", outDir),
             domain = NA)

    ## If there is an @file{INDEX} file in the package sources, we
    ## install this, and do not build it.
    if(file_test("-f", file.path(dir, "INDEX")))
        if(!file.copy(file.path(dir, "INDEX"),
                      file.path(outDir, "INDEX"),
                      overwrite = TRUE))
            stop(gettextf("unable to copy INDEX to '%s'",
                          file.path(outDir, "INDEX")),
                 domain = NA)

    outMetaDir <- file.path(outDir, "Meta")
    if(!file_test("-d", outMetaDir) && !dir.create(outMetaDir))
         stop(gettextf("cannot open directory '%s'", outMetaDir),
              domain = NA)
    .install_package_Rd_indices(dir, outDir)
    .install_package_demo_index(dir, outDir)
    invisible()
}

### * .install_package_Rd_indices

.install_package_Rd_indices <-
function(dir, outDir)
{
    dir <- file_path_as_absolute(dir)
    docsDir <- file.path(dir, "man")
    dataDir <- file.path(outDir, "data")
    outDir <- file_path_as_absolute(outDir)

    ## <FIXME>
    ## Not clear whether we should use the basename of the directory we
    ## install to, or the package name as obtained from the DESCRIPTION
    ## file in the directory we install from (different for versioned
    ## installs).  We definitely do not want the basename of the dir we
    ## install from.
    packageName <- basename(outDir)
    ## </FIXME>

    allRd <- if(file_test("-d", docsDir))
        list_files_with_type(docsDir, "docs") else character()
    ## some people have man dirs without any valid .Rd files
    if(length(allRd)) {
        ## we want the date of the newest .Rd file we will install
        newestRd <- max(file.info(allRd)$mtime)
        ## these files need not exist, which gives NA.
        indices <- c(file.path("Meta", "Rd.rds"),
                     file.path("Meta", "hsearch.rds"),
                     file.path("Meta", "links.rds"),
                     "INDEX")
        upToDate <- file.info(file.path(outDir, indices))$mtime >= newestRd
        if(file_test("-d", dataDir)
           && length(dataFiles <- list.files(dataDir))) {
            ## Note that the data index is computed from both the package's
            ## Rd files and the data sets actually available.
            newestData <- max(file.info(dataFiles)$mtime)
            upToDate <- c(upToDate,
                          file.info(file.path(outDir, "Meta", "data.rds"))$mtime >=
                          max(newestRd, newestData))
        }
        ## Note that this is not quite good enough: an Rd file or data file
        ## might have been removed since the indices were made.
        RdsFile <- file.path("Meta", "Rd.rds")
        if(file.exists(RdsFile)) { ## for Rd files
            ## this has file names without path
            files <- readRDS(RdsFile)$File
            if(!identical(basename(allRd), files)) upToDate <- FALSE
        }
        ## we want to proceed if any is NA.
        if(all(upToDate %in% TRUE)) return(invisible())

        ## Rd objects should already have been installed.
        db <- tryCatch(Rd_db(basename(outDir), lib.loc = dirname(outDir)),
                       error = function(e) NULL)
        ## If not, we build the Rd db from the sources:
        if(is.null(db)) db <- .build_Rd_db(dir, allRd)
        contents <- Rd_contents(db)

        .write_Rd_contents_as_RDS(contents,
                                  file.path(outDir, "Meta", "Rd.rds"))

        defaultEncoding <- as.vector(readRDS(file.path(outDir, "Meta", "package.rds"))$DESCRIPTION["Encoding"])
        if(is.na(defaultEncoding)) defaultEncoding <- NULL
        saveRDS(.build_hsearch_index(contents, packageName, defaultEncoding),
                 file.path(outDir, "Meta", "hsearch.rds"))

        saveRDS(.build_links_index(contents, packageName),
                 file.path(outDir, "Meta", "links.rds"))

        ## If there is no @file{INDEX} file in the package sources, we
        ## build one.
        ## <NOTE>
        ## We currently do not also save this in RDS format, as we can
        ## always do
        ##   .build_Rd_index(readRDS(file.path(outDir, "Meta", "Rd.rds"))
        if(!file_test("-f", file.path(dir, "INDEX")))
            writeLines(formatDL(.build_Rd_index(contents)),
                       file.path(outDir, "INDEX"))
        ## </NOTE>
    } else {
        contents <- NULL
        saveRDS(.build_hsearch_index(contents, packageName, defaultEncoding),
                 file.path(outDir, "Meta", "hsearch.rds"))

        saveRDS(.build_links_index(contents, packageName),
                 file.path(outDir, "Meta", "links.rds"))

    }
    if(file_test("-d", dataDir))
        saveRDS(.build_data_index(dataDir, contents),
                 file.path(outDir, "Meta", "data.rds"))
    invisible()
}

### * .install_package_vignettes2
## called from R CMD INSTALL for pre 3.0.2-built tarballs, and for base packages

.install_package_vignettes2 <-
function(dir, outDir, encoding = "")
{
    dir <- file_path_as_absolute(dir)
    subdirs <- c("vignettes", file.path("inst", "doc"))
    ok <- file_test("-d", file.path(dir, subdirs))
    ## Create a vignette index only if the vignette dir exists.
    if (!any(ok))
       return(invisible())

    subdir <- subdirs[ok][1L]
    vignetteDir <- file.path(dir, subdir)

    outDir <- file_path_as_absolute(outDir)
    packageName <- basename(outDir)
    outVignetteDir <- file.path(outDir, "doc")
    ## --fake  and --no-inst installs do not have a outVignetteDir.
    if(!file_test("-d", outVignetteDir)) return(invisible())

    ## If there is an HTML index in the @file{inst/doc} subdirectory of
    ## the package source directory (@code{dir}), we do not overwrite it
    ## (similar to top-level @file{INDEX} files).  Installation already
    ## copied this over.
    hasHtmlIndex <- file_test("-f", file.path(vignetteDir, "index.html"))
    htmlIndex <- file.path(outDir, "doc", "index.html")

    vigns <- pkgVignettes(dir=dir, subdirs=subdir)

    ## Write dummy HTML index if no vignettes are found and exit.
    if(length(vigns$docs) == 0L) {
        ## we don't want to write an index if the directory is in fact empty
        files <- list.files(vignetteDir, all.files = TRUE, no.. = TRUE)
        if((length(files) > 0L) && !hasHtmlIndex)
            .writeVignetteHtmlIndex(packageName, htmlIndex)
        return(invisible())
    }

    if (subdir == "vignettes") {
        ## copy vignette sources over.
        file.copy(vigns$docs, outVignetteDir)
    }

    vigns <- tryCatch({
        pkgVignettes(dir=outDir, subdirs="doc", output=TRUE, source=TRUE)
    }, error = function(ex) {
        pkgVignettes(dir=outDir, subdirs="doc")
    })

    vignetteIndex <- .build_vignette_index(vigns)
    if(NROW(vignetteIndex) > 0L) {
        cwd <- getwd()
        if (is.null(cwd))
            stop("current working directory cannot be ascertained")
        setwd(outVignetteDir)

	loadVignetteBuilder(dir, mustwork = FALSE)

        ## install tangled versions of Sweave vignettes.  FIXME:  Vignette
        ## *.R files should have been included when the package was built,
        ## but in the interim before they are all built with the new code,
        ## this is needed.
        for(i in seq_along(vigns$docs)) {
            file <- vigns$docs[i]
            if (!is.null(vigns$sources) && !is.null(vigns$sources[file][[1]]))
            	next
            file <- basename(file)
            enc <- getVignetteEncoding(file, TRUE)
            if(enc %in% c("non-ASCII", "unknown")) enc <- encoding

            cat("  ", sQuote(basename(file)),
                if(nzchar(enc)) paste("using", sQuote(enc)), "\n")

	    engine <- try(vignetteEngine(vigns$engines[i]), silent = TRUE)
	    if (!inherits(engine, "try-error"))
            	engine$tangle(file, quiet = TRUE, encoding = enc)
            setwd(outVignetteDir) # just in case some strange tangle function changed it
        }
        setwd(cwd)

        # Update - now from the output directory
        vigns <- pkgVignettes(dir=outDir, subdirs="doc", source=TRUE)

        ## remove any files with no R code (they will have header comments).
        ## if not correctly declared they might not be in the current encoding
        sources <- unlist(vigns$sources)
        for(i in seq_along(sources)) {
            file <- sources[i]
            if (!file_test("-f", file)) next
            bfr <- readLines(file, warn = FALSE)
            if(all(grepl("(^###|^[[:space:]]*$)", bfr, useBytes = TRUE)))
                unlink(file)
        }

        # Update
        vigns <- pkgVignettes(dir=outDir, subdirs="doc", source=TRUE)

        # Add tangle source files (*.R) to the vignette index
        # Only the "main" R file, because tangle may also split
        # output into multiple files
        sources <- character(length(vigns$docs))
        for (i in seq_along(vigns$docs)) {
           name <- vigns$names[i]
           source <- find_vignette_product(name, by = "tangle", main = TRUE, dir = vigns$dir, engine = engine)
           if (length(source) > 0L)
              sources[i] <- basename(source)
        }
        vignetteIndex$R <- sources
    }

    if(!hasHtmlIndex)
        .writeVignetteHtmlIndex(packageName, htmlIndex, vignetteIndex)

    saveRDS(vignetteIndex,
             file = file.path(outDir, "Meta", "vignette.rds"))

    invisible()
}

### * .install_package_vignettes3
## called from R CMD INSTALL for 3.0.2 or later tarballs

.install_package_vignettes3 <-
function(dir, outDir, encoding = "")
{
    packageName <- basename(outDir)
    dir <- file_path_as_absolute(dir)
    indexname <- file.path(dir, "build", "vignette.rds")
    ok <- file_test("-f", indexname)
    ## Create a vignette index only if the vignette dir exists.
    if (!ok)
       return(invisible())
       
    ## Copy the index to Meta
    file.copy(indexname, file.path(outDir, "Meta"))

    ## If there is an HTML index in the @file{inst/doc} subdirectory of
    ## the package source directory (@code{dir}), we do not overwrite it
    ## (similar to top-level @file{INDEX} files).  Installation already
    ## copied this over.
    vignetteDir <- file.path(outDir, "doc")
    hasHtmlIndex <- file_test("-f", file.path(vignetteDir, "index.html"))
    htmlIndex <- file.path(outDir, "doc", "index.html")

    vignetteIndex <- readRDS(indexname)

    if(!hasHtmlIndex)
        .writeVignetteHtmlIndex(packageName, htmlIndex, vignetteIndex)

    invisible()
}

### * .install_package_demo_index

.install_package_demo_index <-
function(dir, outDir)
{
    demoDir <- file.path(dir, "demo")
    if(!file_test("-d", demoDir)) return(invisible())
    demoIndex <- .build_demo_index(demoDir)
    saveRDS(demoIndex,
             file = file.path(outDir, "Meta", "demo.rds"))
    invisible()
}

### * .vinstall_package_indices

## called from src/library/Makefile
.vinstall_package_indices <-
function(src_dir, out_dir, packages)
{
    ## For the given packages with sources rooted at @file{src_dir} and
    ## installations rooted at @file{out_dir}, install the package
    ## indices.
    ## Really only useful for base packages under Unix.
    ## See @file{src/library/Makefile.in}.

    for(p in unlist(strsplit(packages, "[[:space:]]+")))
        .install_package_indices(file.path(src_dir, p), file.path(out_dir, p))
    utils::make.packages.html(.Library, verbose = FALSE)
    invisible()
}

### * .install_package_vignettes

## called from src/library/Makefile[.win]
## this is only used when building R
.install_package_vignettes <-
function(dir, outDir, keep.source = TRUE)
{
    dir <- file_path_as_absolute(dir)
    vigns <- pkgVignettes(dir = dir)
    if(is.null(vigns) || !length(vigns$docs)) return(invisible())

    outDir <- file_path_as_absolute(outDir)
    outVignetteDir <- file.path(outDir, "doc")
    if(!file_test("-d", outVignetteDir) && !dir.create(outVignetteDir))
        stop(gettextf("cannot open directory '%s'", outVignetteDir),
             domain = NA)

    ## We have to be careful to avoid repeated rebuilding.
    vignettePDFs <-
        file.path(outVignetteDir,
                  sub("$", ".pdf",
                      basename(file_path_sans_ext(vigns$docs))))
    upToDate <- file_test("-nt", vignettePDFs, vigns$docs)

    ## The primary use of this function is to build and install PDF
    ## vignettes in base packages.
    ## Hence, we build in a subdir of the current directory rather
    ## than a temp dir: this allows inspection of problems and
    ## automatic cleanup via Make.
    cwd <- getwd()
    if (is.null(cwd))
        stop("current working directory cannot be ascertained")
    buildDir <- file.path(cwd, ".vignettes")
    if(!file_test("-d", buildDir) && !dir.create(buildDir))
        stop(gettextf("cannot create directory '%s'", buildDir), domain = NA)
    on.exit(setwd(cwd))
    setwd(buildDir)

    loadVignetteBuilder(vigns$pkgdir)

    for(i in seq_along(vigns$docs)[!upToDate]) {
        file <- vigns$docs[i]
        name <- vigns$names[i]
        engine <- vignetteEngine(vigns$engines[i])

        message(gettextf("processing %s", sQuote(basename(file))),
                domain = NA)

        ## Note that contrary to all other weave/tangle calls, here
        ## 'file' is not a file in the current directory [hence no
        ## file <- basename(file) above]. However, weave should/must
        ## always create a file ('output') in the current directory.
        output <- tryCatch({
            engine$weave(file, pdf = TRUE, eps = FALSE, quiet = TRUE,
                        keep.source = keep.source, stylepath = FALSE)
            setwd(buildDir)
            find_vignette_product(name, by = "weave", engine = engine)
        }, error = function(e) {
            stop(gettextf("running %s on vignette '%s' failed with message:\n%s",
                 engine[["name"]], file, conditionMessage(e)),
                 domain = NA, call. = FALSE)
        })
        ## In case of an error, do not clean up: should we point to
        ## buildDir for possible inspection of results/problems?
        ## We need to ensure that vignetteDir is in TEXINPUTS and BIBINPUTS.
        if (vignette_is_tex(output)) {
	    ## <FIXME>
	    ## What if this fails?
            ## Now gives a more informative error texi2pdf fails
            ## or if it does not produce a <name>.pdf.
            tryCatch({
                texi2pdf(file = output, quiet = TRUE, texinputs = vigns$dir)
                output <- find_vignette_product(name, by = "texi2pdf", engine = engine)
            }, error = function(e) {
                stop(gettextf("compiling TeX file %s failed with message:\n%s",
                 sQuote(output), conditionMessage(e)),
                 domain = NA, call. = FALSE)
            })
	    ## </FIXME>
	}

        if(!file.copy(output, outVignetteDir, overwrite = TRUE))
            stop(gettextf("cannot copy '%s' to '%s'",
                          output,
                          outVignetteDir),
                 domain = NA)
    }
    ## Need to change out of this dir before we delete it,
    ## at least on Windows.
    setwd(cwd)
    unlink(buildDir, recursive = TRUE)
    ## Now you need to update the HTML index!
    ## This also creates the .R files
    .install_package_vignettes2(dir, outDir)    
    invisible()
}

### * .install_package_namespace_info

.install_package_namespace_info <-
function(dir, outDir)
{
    dir <- file_path_as_absolute(dir)
    nsFile <- file.path(dir, "NAMESPACE")
    if(!file_test("-f", nsFile)) return(invisible())
    nsInfoFilePath <- file.path(outDir, "Meta", "nsInfo.rds")
    if(file_test("-nt", nsInfoFilePath, nsFile)) return(invisible())
    nsInfo <- parseNamespaceFile(basename(dir), dirname(dir))
    outMetaDir <- file.path(outDir, "Meta")
    if(!file_test("-d", outMetaDir) && !dir.create(outMetaDir))
        stop(gettextf("cannot open directory '%s'", outMetaDir),
             domain = NA)
    saveRDS(nsInfo, nsInfoFilePath)
    invisible()
}

### * .vinstall_package_namespaces_as_RDS

## called from src/library/Makefile
.vinstall_package_namespaces_as_RDS <-
function(dir, packages)
{
    ## For the given packages installed in @file{dir} which have a
    ## NAMESPACE file, install the namespace info as R metadata.
    ## Really only useful for base packages under Unix.
    ## See @file{src/library/Makefile.in}.

    for(p in unlist(strsplit(packages, "[[:space:]]+")))
        .install_package_namespace_info(file.path(dir, p),
                                        file.path(dir, p))
    invisible()
}

### * .install_package_Rd_objects

## called from src/library/Makefile
.install_package_Rd_objects <-
function(dir, outDir, encoding = "unknown")
{
    dir <- file_path_as_absolute(dir)
    mandir <- file.path(dir, "man")
    manfiles <- if(!file_test("-d", mandir)) character()
    else list_files_with_type(mandir, "docs")
    manOutDir <- file.path(outDir, "help")
    dir.create(manOutDir, FALSE)
    db_file <- file.path(manOutDir,
                         paste0(basename(outDir), ".rdx"))
    built_file <- file.path(dir, "build", "partial.rdb")
    ## Avoid (costly) rebuilding if not needed.
    ## Actually, it seems no more costly than these tests, which it also does
    pathsFile <- file.path(manOutDir, "paths.rds")
    if(!file_test("-f", db_file) || !file.exists(pathsFile) ||
       !identical(sort(manfiles), sort(readRDS(pathsFile))) ||
       !all(file_test("-nt", db_file, manfiles))) {
        db <- .build_Rd_db(dir, manfiles, db_file = db_file,
                           encoding = encoding, built_file = built_file)
        nm <- as.character(names(db)) # Might be NULL
        saveRDS(nm, pathsFile)
        names(db) <- sub("\\.[Rr]d$", "", basename(nm))
        makeLazyLoadDB(db, file.path(manOutDir, basename(outDir)))
    }
    invisible()
}

### * .install_package_demos

## called from basepkg.mk and .install_packages
.install_package_demos <-
function(dir, outDir)
{
    ## NB: we no longer install 00Index
    demodir <- file.path(dir, "demo")
    if(!file_test("-d", demodir)) return()
    demofiles <- list_files_with_type(demodir, "demo", full.names = FALSE)
    if(!length(demofiles)) return()
    demoOutDir <- file.path(outDir, "demo")
    if(!file_test("-d", demoOutDir)) dir.create(demoOutDir)
    file.copy(file.path(demodir, demofiles), demoOutDir,
              overwrite = TRUE)
}


### * .find_cinclude_paths

.find_cinclude_paths <-
function(pkgs, lib.loc = NULL, file = NULL)
{
    ## given a character string of comma-separated package names,
    ## find where the packages are installed and generate
    ## -I"/path/to/package/include" ...

    if(!is.null(file)) {
        tmp <- read.dcf(file, "LinkingTo")[1L, 1L]
        if(is.na(tmp)) return(invisible())
        pkgs <- tmp
    }
    pkgs <- strsplit(pkgs[1L], ",[[:blank:]]*")[[1L]]
    paths <- find.package(pkgs, lib.loc, quiet=TRUE)
    if(length(paths))
	cat(paste(paste0('-I"', paths, '/include"'), collapse=" "))
    return(invisible())
}

### * .Rtest_package_depends_R_version

.Rtest_package_depends_R_version <-
function(dir)
{
    if(missing(dir)) dir <- "."
    meta <- .read_description(file.path(dir, "DESCRIPTION"))
    deps <- .split_description(meta, verbose = TRUE)$Rdepends2
    status <- 0
    current <- getRversion()
    for(depends in deps) {
        ## .split_description will have ensured that this is NULL or
        ## of length 3.
        if(length(depends) > 1L) {
            ## .check_package_description will insist on these operators
            if(!depends$op %in% c("<=", ">=", "<", ">", "==", "!="))
                message("WARNING: malformed 'Depends' field in 'DESCRIPTION'")
            else {
                status <- if(inherits(depends$version, "numeric_version"))
                    !do.call(depends$op, list(current, depends$version))
                else {
                    ver <- R.version
                    if (ver$status %in% c("", "Patched")) FALSE
                    else !do.call(depends$op,
                                 list(ver[["svn rev"]],
                                      as.numeric(sub("^r", "", depends$version))))
                }
            }
            if(status != 0) {
                package <- Sys.getenv("R_PACKAGE_NAME")
                if(!nzchar(package))
                    package <- meta["Package"]
                msg <- if(nzchar(package))
                    gettextf("ERROR: this R is version %s, package '%s' requires R %s %s",
                                    current, package,
                                    depends$op, depends$version)
                else
                    gettextf("ERROR: this R is version %s, required is R %s %s",
                                    current, depends$op, depends$version)
                message(strwrap(msg, exdent = 2L))
                break
            }
        }
    }
    status
}

## no longer used
.test_package_depends_R_version <-
function(dir)
    q(status = .Rtest_package_depends_R_version(dir))


### * .test_load_package

.test_load_package <- function(pkg_name, lib)
{
    options(warn = 1)
    res <- try(suppressPackageStartupMessages(library(pkg_name, lib.loc = lib, character.only = TRUE, logical.return = TRUE)))
    if (inherits(res, "try-error") || !res)
        stop("loading failed", call. = FALSE)
}


### * checkRdaFiles

checkRdaFiles <- function(paths)
{
    if(length(paths) == 1L && isTRUE(file.info(paths)$isdir)) {
        paths <- Sys.glob(c(file.path(paths, "*.rda"),
                            file.path(paths, "*.RData")))
        ## Exclude .RData, which this may or may not match
        paths <- grep("/[.]RData$", paths, value = TRUE, invert = TRUE)
    }
    res <- data.frame(size = NA_real_, ASCII = NA,
                      compress = NA_character_, version = NA_integer_,
                      stringsAsFactors = FALSE)
    res <- res[rep(1L, length(paths)), ]
    row.names(res) <- paths
    keep <- file.exists(paths)
    res$size[keep] <- file.info(paths)$size[keep]
    for(p in paths[keep]) {
        magic <- readBin(p, "raw", n = 5)
        res[p, "compress"] <- if(all(magic[1:2] == c(0x1f, 0x8b))) "gzip"
        else if(rawToChar(magic[1:3]) == "BZh") "bzip2"
        else if(magic[1L] == 0xFD && rawToChar(magic[2:5]) == "7zXZ") "xz"
        else if(grepl("RD[ABX][12]", rawToChar(magic), useBytes = TRUE)) "none"
        else "unknown"
        con <- gzfile(p)
        magic <- readChar(con, 5L, useBytes = TRUE)
        close(con)
        res[p, "ASCII"]  <- if (grepl("RD[ABX][12]", magic, useBytes = TRUE))
            substr(magic, 3, 3) == "A" else NA
        ver <- sub("(RD[ABX])([12]*)", "\\2", magic, useBytes = TRUE)
        res$version <- as.integer(ver)
    }
    res
}

### * resaveRdaFiles

resaveRdaFiles <- function(paths,
                           compress = c("auto", "gzip", "bzip2", "xz"),
                           compression_level)
{
    if(length(paths) == 1L && isTRUE(file.info(paths)$isdir))
        paths <- Sys.glob(c(file.path(paths, "*.rda"),
                            file.path(paths, "*.RData")))
    compress <- match.arg(compress)
    if (missing(compression_level))
        compression_level <- switch(compress, "gzip" = 6, 9)
    for(p in paths) {
        env <- new.env(hash = TRUE) # probably small, need not be
#        sink(tempfile()) ## suppress startup messages to stdout, for BARD
        suppressPackageStartupMessages(load(p, envir = env))
#        sink()
        if(compress == "auto") {
            f1 <- tempfile()
            save(file = f1, list = ls(env, all.names = TRUE), envir = env)
            f2 <- tempfile()
            save(file = f2, list = ls(env, all.names = TRUE), envir = env,
                 compress = "bzip2")
            ss <- file.info(c(f1, f2))$size * c(0.9, 1.0)
            names(ss) <- c(f1, f2)
            if(ss[1L] > 10240) {
                f3 <- tempfile()
                save(file = f3, list = ls(env, all.names = TRUE), envir = env,
                     compress = "xz")
                ss <- c(ss, file.info(f3)$size)
		names(ss) <- c(f1, f2, f3)
            }
            nm <- names(ss)
            ind <- which.min(ss)
            file.copy(nm[ind], p, overwrite = TRUE)
            unlink(nm)
        } else
            save(file = p, list = ls(env, all.names = TRUE), envir = env,
                 compress = compress, compression_level = compression_level)
    }
}

### * compactPDF

compactPDF <-
    function(paths, qpdf = Sys.which(Sys.getenv("R_QPDF", "qpdf")),
             gs_cmd = Sys.getenv("R_GSCMD", ""),
             gs_quality = Sys.getenv("GS_QUALITY", "none"),
             gs_extras = character())
{
    use_qpdf <- nzchar(qpdf)
    gs_quality <- match.arg(gs_quality, c("none", "printer", "ebook", "screen"))
    use_gs <- if(gs_quality != "none") nzchar(gs_cmd <- find_gs_cmd(gs_cmd)) else FALSE
    if (!use_gs && !use_qpdf) return()
    if(length(paths) == 1L && isTRUE(file.info(paths)$isdir))
        paths <- Sys.glob(file.path(paths, "*.pdf"))
    dummy <- rep.int(NA_real_, length(paths))
    ans <- data.frame(old = dummy, new = dummy, row.names = paths)
    tf <- tempfile("pdf"); tf2 <- tempfile("pdf")
    for (p in paths) {
        res <- 0
        if (use_gs) {
            res <- system2(gs_cmd,
                           c("-q -dNOPAUSE -dBATCH -sDEVICE=pdfwrite",
                             sprintf("-dPDFSETTINGS=/%s", gs_quality),
                             "-dCompatibilityLevel=1.5",
                             "-dAutoRotatePages=/None",
                             sprintf("-sOutputFile=%s", tf),
                             gs_extras, p), FALSE, FALSE)
            if(!res && use_qpdf) {
                unlink(tf2) # precaution
                file.rename(tf, tf2)
                res <- system2(qpdf, c("--stream-data=compress",
                                       "--object-streams=generate",
                                       tf2, tf), FALSE, FALSE)
                unlink(tf2)
            }
        } else if(use_qpdf) {
            res <- system2(qpdf, c("--stream-data=compress",
                                   "--object-streams=generate",
                                   p, tf), FALSE, FALSE)
        }
        if(!res && file.exists(tf)) {
            old <- file.info(p)$size; new <-  file.info(tf)$size
            if(new/old < 0.9 && new < old - 1e4) {
                file.copy(tf, p, overwrite = TRUE)
                ans[p, ] <- c(old, new)
            }
        }
        unlink(tf)
    }
    structure(na.omit(ans), class = c("compactPDF", "data.frame"))
}

find_gs_cmd <- function(gs_cmd)
{
    if(!nzchar(gs_cmd)) {
        if(.Platform$OS.type == "windows") {
            gs_cmd <- Sys.which("gswin64c")
            if (!nzchar(gs_cmd)) gs_cmd <- Sys.which("gswin32c")
            gs_cmd
        } else Sys.which("gs")
    } else Sys.which(gs_cmd)
}

format.compactPDF <- function(x, ratio = 0.9, diff = 1e4, ...)
{
    if(!nrow(x)) return(character())
    z <- y <- x[with(x, new/old < ratio & new < old - diff), ]
    if(!nrow(z)) return(character())
    z[] <- lapply(y, function(x) sprintf("%.0fKb", x/1024))
    large <- y$new >= 1024^2
    z[large, ] <- lapply(y[large, ], function(x) sprintf("%.1fMb", x/1024^2))
    paste('  compacted', sQuote(basename(row.names(y))),
          'from', z[, 1L], 'to', z[, 2L])
}

### * add_datalist

add_datalist <- function(pkgpath, force = FALSE)
{
    dlist <- file.path(pkgpath, "data", "datalist")
    if (!force && file.exists(dlist)) return()
    fi <- file.info(Sys.glob(file.path(pkgpath, "data", "*")))
    size <- sum(fi$size)
    if(size <= 1024^2) return()
    z <- suppressPackageStartupMessages(list_data_in_pkg(dataDir = file.path(pkgpath, "data"))) # for BARD
    if(!length(z)) return()
    con <- file(dlist, "w")
    for (nm in names(z)) {
        zz <- z[[nm]]
        if (length(zz) == 1L && zz == nm) writeLines(nm, con)
        else cat(nm, ": ", paste(zz, collapse = " "), "\n",
                 sep = "", file = con)
    }
    close(con)
    invisible()
}


### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "### [*]+" ***
### End: ***
