#  File src/library/tools/R/build.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1995-2014 The R Core Team
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

#### R based engine for R CMD build

## R developers can use this to debug the function by running it
## directly as tools:::.build_packages(args), where the args should
## be what commandArgs(TRUE) would return, that is a character vector
## of (space-delimited) terms that would be passed to R CMD build.

writeDefaultNamespace <-
function(filename, desc = file.path(dirname(filename), "DESCRIPTION"))
{
    pkgInfo <- .split_description(.read_description(desc))
    pkgs <- unique(c(names(pkgInfo$Imports), names(pkgInfo$Depends)))
    pkgs <- pkgs[pkgs != "base"]

    writeLines(c("# Default NAMESPACE created by R",
                 "# Remove the previous line if you edit this file",
    		 "",
    		 "# Export all names",
		 "exportPattern(\".\")",
		 if (length(pkgs))
		     c("",
		       "# Import all packages listed as Imports or Depends",
		       "import(",
		       paste(" ", pkgs, collapse = ",\n"),
		       ")")),
    	       filename)
}


### formerly Perl R::Utils::get_exclude_patterns

## Return list of file patterns excluded by R CMD build and check.
## Kept here so that we ensure that the lists are in sync, but not exported.
## Has Unix-style '/' path separators hard-coded, but that is what dir() uses.
get_exclude_patterns <- function()
    c("^\\.Rbuildignore$",
      "(^|/)\\.DS_Store$",
      "^\\.(RData|Rhistory)$",
      "~$", "\\.bak$", "\\.swp$",
      "(^|/)\\.#[^/]*$", "(^|/)#[^/]*#$",
      ## Outdated ...
      "^TITLE$", "^data/00Index$",
      "^inst/doc/00Index\\.dcf$",
      ## Autoconf
      "^config\\.(cache|log|status)$",
      "^autom4te\\.cache$",
      ## Windows dependency files
      "^src/.*\\.d$", "^src/Makedeps$",
      ## IRIX, of some vintage
      "^src/so_locations$",
      ## Sweave detrius
      "^inst/doc/Rplots\\.(ps|pdf)$"
      )


### based on Perl build script

.build_packages <- function(args = NULL)
{
    ## this requires on Windows sh make

    WINDOWS <- .Platform$OS.type == "windows"

    Sys.umask("022") # Perl version did not have this.

    writeLinesNL <- function(text, file)
    {
        ## a version that uses NL line endings everywhere
        con <- file(file, "wb")
        on.exit(close(con))
        writeLines(text, con)
    }

    ## This version of system_with_capture merges stdout and stderr
    ## Used to run R to install package and build vignettes.
    system_with_capture <- function (command, args) {
        outfile <- tempfile("xshell")
        on.exit(unlink(outfile))
        status <- system2(command, args, outfile, outfile)
        list(status = status, stdout = readLines(outfile, warn = FALSE))
    }
    ## Run silently
    Ssystem <- function(command, args = character(), ...)
        system2(command, args, stdout = NULL, stderr = NULL, ...)

    do_exit <- function(status = 1L) q("no", status = status, runLast = FALSE)

    env_path <- function(...) file.path(..., fsep = .Platform$path.sep)

    ## Used for BuildVignettes, BuildManual, BuildKeepEmpty,
    ## and (character not logical) BuildResaveData
    parse_description_field <-
        function(desc, field, default = TRUE, logical = TRUE)
    {
        tmp <- desc[field]
        if (is.na(tmp)) default
        else if(logical)
            switch(tmp,
                   "yes"=, "Yes" =, "true" =, "True" =, "TRUE" = TRUE,
                   "no" =, "No" =, "false" =, "False" =, "FALSE" = FALSE,
                   default)
        else tmp
    }

    Usage <- function() {
        cat("Usage: R CMD build [options] pkgdirs",
            "",
            "Build R packages from package sources in the directories specified by",
            sQuote("pkgdirs"),
            "",
            "Options:",
            "  -h, --help		print short help message and exit",
            "  -v, --version		print version info and exit",
            "",
            "  --force               force removal of INDEX file",
            "  --keep-empty-dirs     do not remove empty dirs",
            "  --no-build-vignettes  do not (re)build package vignettes",
            "  --no-manual           do not build the PDF manual even if \\Sexprs are present",
            "  --resave-data=        re-save data files as compactly as possible:",
            '                        "no", "best", "gzip" (default)',
            "  --resave-data         same as --resave-data=best",
            "  --no-resave-data      same as --resave-data=no",
            "  --compact-vignettes=  try to compact PDF files under inst/doc:",
            '                        "no" (default), "qpdf", "gs", "gs+qpdf", "both"',
            "  --compact-vignettes   same as --compact-vignettes=qpdf",
            "  --md5                 add MD5 sums",
           "",
            "Report bugs at bugs.r-project.org .", sep = "\n")
    }

    add_build_stamp_to_description_file <- function(ldpath) {
        db <- .read_description(ldpath)
        ## this is an optional function, so could fail
        user <- Sys.info()["user"]
        if(user == "unknown") user <- Sys.getenv("LOGNAME")
        db["Packaged"] <-
            sprintf("%s; %s",
                    format(Sys.time(), '', tz = 'UTC', usetz = TRUE),
                    user)
        .write_description(db, ldpath)
    }

    ## <FIXME>
    ## This should really be combined with
    ## add_build_stamp_to_description_file().
    ## Also, the build code reads DESCRIPTION files too often ...
    add_expanded_R_fields_to_description_file <- function(ldpath) {
        db <- .read_description(ldpath)
        fields <- .expand_package_description_db_R_fields(db)
        if(length(fields))
            .write_description(c(db, fields), ldpath)
    }
    ## </FIXME>

    temp_install_pkg <- function(pkgdir, libdir) {
	dir.create(libdir, mode = "0755", showWarnings = FALSE)
        ## assume vignettes only need one arch
        if (WINDOWS) {
            cmd <- file.path(R.home("bin"), "Rcmd.exe")
            args <- c("INSTALL -l", shQuote(libdir),
                      "--no-multiarch", shQuote(pkgdir))
        } else {
            cmd <- file.path(R.home("bin"), "R")
            args <- c("CMD", "INSTALL -l", shQuote(libdir),
                      "--no-multiarch", shQuote(pkgdir))
        }
	res <- system_with_capture(cmd, args)
	if (res$status) {
	    printLog(Log, "      -----------------------------------\n")
	    printLog0(Log, paste(c(res$stdout, ""),  collapse = "\n"))
	    printLog(Log, "      -----------------------------------\n")
	    unlink(libdir, recursive = TRUE)
	    printLog(Log, "ERROR: package installation failed\n")
	    do_exit(1)
	}
	Sys.setenv("R_BUILD_TEMPLIB" = libdir)
	TRUE
    }

    prepare_pkg <- function(pkgdir, desc, Log)
    {
        owd <- setwd(pkgdir); on.exit(setwd(owd))
        pkgname <- basename(pkgdir)
        checkingLog(Log, "DESCRIPTION meta-information")
        res <- try(.check_package_description("DESCRIPTION"))
        if (inherits(res, "try-error")) {
            resultLog(Log, "ERROR")
            messageLog(Log, "running '.check_package_description' failed")
        } else {
            if (any(sapply(res, length))) {
                resultLog(Log, "ERROR")
                print(res) # FIXME print to Log?
                do_exit(1L)
            } else resultLog(Log, "OK")
        }
        cleanup_pkg(pkgdir, Log)

        libdir <- tempfile("Rinst")

        ensure_installed <- function()
	    if (!pkgInstalled) {
		messageLog(Log,
			   "installing the package to build vignettes")
		pkgInstalled <<- temp_install_pkg(pkgdir, libdir)
	    }

        pkgInstalled <- build_Rd_db(pkgdir, libdir, desc)

        if (file.exists("INDEX")) update_Rd_index("INDEX", "man", Log)
        doc_dir <- file.path("inst", "doc")
        if ("makefile" %in% dir(doc_dir)) { # avoid case-insensitive match
            messageLog(Log, "renaming 'inst/doc/makefile' to 'inst/doc/Makefile'")
            file.rename(file.path(doc_dir, "makefile"),
                        file.path(doc_dir, "Makefile"))
        }
        if (vignettes &&
            parse_description_field(desc, "BuildVignettes", TRUE)) {
## this is not a logical field
##	    if (nchar(parse_description_field(desc, "VignetteBuilder", "")))
##		ensure_installed()
            ## PR#15775: check VignetteBuilder packages are installed
            ## This is a bit wasteful: we do not need them in this process
            loadVignetteBuilder(pkgdir, TRUE)

            ## Look for vignette sources
            vigns <- pkgVignettes(dir = '.', check = TRUE)
            if (!is.null(vigns) && length(vigns$docs)) {
                ensure_installed()
                ## Good to do this in a separate process: it might die
                creatingLog(Log, "vignettes")
                R_LIBS <- Sys.getenv("R_LIBS", NA_character_)
                if (!is.na(R_LIBS)) {
                    on.exit(Sys.setenv(R_LIBS = R_LIBS), add = TRUE)
                    Sys.setenv(R_LIBS = env_path(libdir, R_LIBS))
                } else {
                    on.exit(Sys.unsetenv("R_LIBS"), add = TRUE)
                    Sys.setenv(R_LIBS = libdir)
                }

                # Tangle all vignettes now.

                cmd <- file.path(R.home("bin"), "Rscript")
                args <- c("--vanilla",
                          "--default-packages=", # some vignettes assume methods
                          "-e", shQuote("tools::buildVignettes(dir = '.', tangle = TRUE)"))
                ## since so many people use 'R CMD' in Makefiles,
                oPATH <- Sys.getenv("PATH")
                Sys.setenv(PATH = paste(R.home("bin"), oPATH,
                           sep = .Platform$path.sep))
                res <- system_with_capture(cmd, args)
                Sys.setenv(PATH = oPATH)
                if (res$status) {
                    resultLog(Log, "ERROR")
                    printLog0(Log, paste(c(res$stdout, ""),  collapse = "\n"))
                    do_exit(1L)
                } else {
                    # Rescan for weave and tangle output files
                    vigns <- pkgVignettes(dir = '.', output = TRUE, source = TRUE)
                    stopifnot(!is.null(vigns))

                    resultLog(Log, "OK")
                }

                ## We may need to install them.
                if (basename(vigns$dir) == "vignettes") {
                    ## inst may not yet exist
                    dir.create(doc_dir, recursive = TRUE, showWarnings = FALSE)
                    file.copy(c(vigns$docs, vigns$outputs, unlist(vigns$sources)), doc_dir)
                    unlink(c(vigns$outputs, unlist(vigns$sources)))
                    extras_file <- file.path("vignettes", ".install_extras")
                    if (file.exists(extras_file)) {
                        extras <- readLines(extras_file, warn = FALSE)
                        if(length(extras)) {
                            allfiles <- dir("vignettes", all.files = TRUE,
                                            full.names = TRUE, recursive = TRUE,
                                            include.dirs = TRUE)
                            inst <- rep(FALSE, length(allfiles))
                            for (e in extras)
                                inst <- inst | grepl(e, allfiles, perl = TRUE,
                                                     ignore.case = TRUE)
                            file.copy(allfiles[inst], doc_dir, recursive = TRUE)
                        }
                    }
                }

		vignetteIndex <- .build_vignette_index(vigns)

		if(NROW(vignetteIndex) > 0L) {
		    ## remove any files with no R code (they will have header comments).
		    ## if not correctly declared they might not be in the current encoding
		    sources <- vignetteIndex$R
		    for(i in seq_along(sources)) {
			file <- file.path(doc_dir, sources[i])
			if (!file_test("-f", file)) next
			bfr <- readLines(file, warn = FALSE)
			if(all(grepl("(^###|^[[:space:]]*$)", bfr, useBytes = TRUE))) {
			    unlink(file)
			    vignetteIndex$R[i] <- ""
			}
		    }
		}

		## Save the list
		dir.create("build", showWarnings = FALSE)
		saveRDS(vignetteIndex,
			file = file.path("build", "vignette.rds"))
            }
        } else {
            fv <- file.path("build", "vignette.rds")
            if(file.exists(fv)) {
                checkingLog(Log, "vignette meta-information")
                db <- readRDS(fv)
                pdfs <- file.path("inst", "doc", db[nzchar(db$PDF), ]$PDF)
                missing <- !file.exists(pdfs)
                if(any(missing)) {
                    msg <- c("Output(s) listed in 'build/vignette.rds' but not in package:",
                             strwrap(sQuote(pdfs[missing]), indent = 2L, exdent = 2L),
                             "Run R CMD build without --no-build-vignettes to re-create")
                    errorLog(Log, paste(msg, collapse = "\n"))
                    do_exit(1L)
                } else resultLog(Log, "OK")
            }
        }
        if (compact_vignettes != "no" &&
            length(pdfs <- dir(doc_dir, pattern = "[.]pdf", recursive = TRUE,
                               full.names = TRUE))) {
            messageLog(Log, "compacting vignettes and other PDF files")
            if(compact_vignettes %in% c("gs", "gs+qpdf", "both")) {
                gs_cmd <- find_gs_cmd()
                gs_quality <- "ebook"
            } else {
                gs_cmd <- ""
                gs_quality <- "none"
            }
            qpdf <-
                ifelse(compact_vignettes %in% c("qpdf", "gs+qpdf", "both"),
                       Sys.which(Sys.getenv("R_QPDF", "qpdf")), "")
            res <- compactPDF(pdfs, qpdf = qpdf,
                              gs_cmd = gs_cmd, gs_quality = gs_quality)
            res <- format(res, diff = 1e5)
            if(length(res))
                printLog0(Log, paste(" ", format(res), collapse = "\n"), "\n")
        }
        if (pkgInstalled) {
            unlink(libdir, recursive = TRUE)

	    ## And finally, clean up again.
            cleanup_pkg(pkgdir, Log)
        }
    }

    cleanup_pkg <- function(pkgdir, Log)
    {
        owd <- setwd(pkgdir); on.exit(setwd(owd))
        pkgname <- basename(pkgdir)
        if (dir.exists("src")) {
            setwd("src")
            messageLog(Log, "cleaning src")
            if (WINDOWS) {
                have_make <- nzchar(Sys.which(Sys.getenv("MAKE", "make")))
                if (file.exists("Makefile.win")) {
                    if (have_make)
                        Ssystem(Sys.getenv("MAKE", "make"), "-f Makefile.win clean")
                    else warning("unable to run 'make clean' in 'src'",
                                 domain = NA)
                } else {
                    if (file.exists("Makevars.win")) {
                        if (have_make) {
                            makefiles <- paste()
                            makefiles <- paste("-f",
                                               shQuote(file.path(R.home("share"), "make", "clean.mk")),
                                           "-f Makevars.win")
                            Ssystem(Sys.getenv("MAKE", "make"),
                                    c(makefiles, "clean"))
                        } else warning("unable to run 'make clean' in 'src'",
                                       domain = NA)
                    }
                    ## Also cleanup possible Unix leftovers ...
                    unlink(c(Sys.glob(c("*.o", "*.sl", "*.so", "*.dylib")),
                             paste0(pkgname, c(".a", ".dll", ".def")),
                             "symbols.rds"))
                    if (dir.exists(".libs")) unlink(".libs", recursive = TRUE)
                    if (dir.exists("_libs")) unlink("_libs", recursive = TRUE)
                }
            } else {
                makefiles <- paste("-f",
                                   shQuote(file.path(R.home("etc"),
                                                     Sys.getenv("R_ARCH"),
                                                     "Makeconf")))
                if (file.exists("Makefile")) {
                    makefiles <- paste(makefiles, "-f", "Makefile")
                    Ssystem(Sys.getenv("MAKE", "make"), c(makefiles, "clean"))
                } else {
                    if (file.exists("Makevars")) {
                        ## ensure we do have a 'clean' target.
                        makefiles <- paste(makefiles, "-f",
                                       shQuote(file.path(R.home("share"), "make", "clean.mk")),
                                           "-f Makevars")
                        Ssystem(Sys.getenv("MAKE", "make"),
                                c(makefiles, "clean"))
                    }
                    ## Also cleanup possible Windows leftovers ...
                    unlink(c(Sys.glob(c("*.o", "*.sl", "*.so", "*.dylib")),
                             paste0(pkgname, c(".a", ".dll", ".def")),
                             "symbols.rds"))
                    if (dir.exists(".libs")) unlink(".libs", recursive = TRUE)
                    if (dir.exists("_libs")) unlink("_libs", recursive = TRUE)
                }
            }
        }
        setwd(owd)
        ## It is not clear that we want to do this: INSTALL should do so.
        ## Also, certain environment variables should be set according
        ## to 'Writing R Extensions', but were not in Perl version (nor
        ## was cleanup.win used).
        if (WINDOWS) {
            if (file.exists("cleanup.win")) {
                ## check we have sh.exe first
                if (nzchar(Sys.which("sh.exe"))) {
                    Sys.setenv(R_PACKAGE_NAME = pkgname)
                    Sys.setenv(R_PACKAGE_DIR = pkgdir)
                    Sys.setenv(R_LIBRARY_DIR = dirname(pkgdir))
                    messageLog(Log, "running 'cleanup.win'")
                    Ssystem("sh", "./cleanup.win")
                }
            }
        } else if (file_test("-x", "cleanup")) {
            Sys.setenv(R_PACKAGE_NAME = pkgname)
            Sys.setenv(R_PACKAGE_DIR = pkgdir)
            Sys.setenv(R_LIBRARY_DIR = dirname(pkgdir))
            messageLog(Log, "running 'cleanup'")
            Ssystem("./cleanup")
        }
    }

    update_Rd_index <- function(oldindex, Rd_files, Log)
    {
        newindex <- tempfile()
        res <- try(Rdindex(Rd_files, newindex))
        if (inherits(res, "try-error")) {
            errorLog(Log, "computing Rd index failed")
            do_exit(1L)
        }
        checkingLog(Log, "whether ", sQuote(oldindex), " is up-to-date")
        if (file.exists(oldindex)) {
            ol <- readLines(oldindex, warn = FALSE) # e.g. BaM had missing final NL
            nl <- readLines(newindex)
            if (!identical(ol, nl)) {
                resultLog(Log, "NO")
               if (force) {
                    messageLog(Log, "removing ", sQuote(oldindex),
			      " as '--force' was given")
                    unlink(oldindex)
                } else {
                    messageLog(Log, "use '--force' to remove ",
			      "the existing ", sQuote(oldindex))
                    unlink(newindex)
                }
            } else {
                resultLog(Log, "OK")
                unlink(newindex)
            }
        } else {
            resultLog(Log, "NO")
            messageLog(Log, "creating new ", sQuote(oldindex))
            file.rename(newindex, oldindex)
        }
    }

    build_Rd_db <- function(pkgdir, libdir, desc) {
    	db <- .build_Rd_db(pkgdir, stages = NULL,
                           os = c("unix", "windows"), step = 1)
    	if (!length(db)) return(FALSE)

    	# Strip the pkgdir off the names
    	names(db) <- substring(names(db),
                               nchar(file.path(pkgdir, "man")) + 2L)

	containsSexprs <-
            which(sapply(db, function(Rd) getDynamicFlags(Rd)["\\Sexpr"]))
	if (!length(containsSexprs)) return(FALSE)

	messageLog(Log, "installing the package to process help pages")

        dir.create(libdir, mode = "0755", showWarnings = FALSE)
        savelib <- .libPaths()
        .libPaths(c(libdir, savelib))
        on.exit(.libPaths(savelib), add = TRUE)

        temp_install_pkg(pkgdir, libdir)

	containsBuildSexprs <-
            which(sapply(db, function(Rd) getDynamicFlags(Rd)["build"]))

	if (length(containsBuildSexprs)) {
	    for (i in containsBuildSexprs)
		db[[i]] <- prepare_Rd(db[[i]], stages = "build",
                                      stage2 = FALSE, stage3 = FALSE)
	    messageLog(Log, "saving partial Rd database")
	    partial <- db[containsBuildSexprs]
	    dir.create("build", showWarnings = FALSE)
	    saveRDS(partial, file.path("build", "partial.rdb"))
	}
	needRefman <- manual &&
            parse_description_field(desc, "BuildManual", TRUE) &&
            any(sapply(db, function(Rd) any(getDynamicFlags(Rd)[c("install", "render")])))
	if (needRefman) {
	    messageLog(Log, "building the PDF package manual")
	    dir.create("build", showWarnings = FALSE)
	    refman <- file.path(pkgdir, "build",
                                paste0(basename(pkgdir), ".pdf"))
	    ..Rd2pdf(c("--force", "--no-preview",
	               paste0("--output=", refman),
	               pkgdir), quit = FALSE)
        }
	return(TRUE)
    }

    ## also fixes up missing final NL
    fix_nonLF_in_files <- function(pkgname, dirPattern, Log)
    {
	if(dir.exists(sDir <- file.path(pkgname, "src"))) {
            files <- dir(sDir, pattern = dirPattern,
                         full.names = TRUE, recursive = TRUE)
            ## FIXME: This "destroys" all timestamps
            for (ff in files) {
                lines <- readLines(ff, warn = FALSE)
                writeLinesNL(lines, ff)
            }
        }
    }

    fix_nonLF_in_source_files <- function(pkgname, Log) {
        fix_nonLF_in_files(pkgname, dirPattern = "\\.([cfh]|cc|cpp)$", Log)
    }
    fix_nonLF_in_make_files <- function(pkgname, Log) {
        fix_nonLF_in_files(pkgname,
                           paste0("^",c("Makefile", "Makefile.in", "Makefile.win",
                                       "Makevars", "Makevars.in", "Makevars.win"),
                                 "$"), Log)
    }

    find_empty_dirs <- function(d)
    {
        ## dir(recursive = TRUE) did not include directories, so
        ## we needed to do this recursively
        files <- dir(d, all.files = TRUE, full.names = TRUE)
        for (dd in files[dir.exists(files)]) {
            if (grepl("/\\.+$", dd)) next
            find_empty_dirs(dd)
        }
        ## allow per-package override
        keep_empty1 <- parse_description_field(desc, "BuildKeepEmpty",
                                               keep_empty)
        if (!keep_empty1) # might have removed a dir
            files <- dir(d, all.files = TRUE, full.names = TRUE)
        if (length(files) <= 2L) { # always has ., ..
            if (keep_empty1) {
                printLog(Log, "WARNING: directory ", sQuote(d), " is empty\n")
            } else {
                unlink(d, recursive = TRUE)
                printLog(Log, "Removed empty directory ", sQuote(d), "\n")
            }
        }
    }

    fixup_R_dep <- function(pkgname, ver = "2.10")
    {
        desc <- .read_description(file.path(pkgname, "DESCRIPTION"))
        Rdeps <- .split_description(desc)$Rdepends2
        for(dep in Rdeps) {
            if(dep$op != '>=') next
            if(dep$version >= package_version(ver)) return()
        }

        on.exit(Sys.setlocale("LC_CTYPE", Sys.getlocale("LC_CTYPE")))
        Sys.setlocale("LC_CTYPE", "C")

        flatten <- function(x) {
            if(length(x) == 3L)
                paste0(x$name, " (", x$op, " ", x$version, ")")
            else x[[1L]]
        }
        deps <- desc["Depends"]
        desc["Depends"] <- if(!is.na(deps)) {
            deps <- .split_dependencies(deps)
            deps <- deps[names(deps) != "R"] # could be more than one
            paste(c(sprintf("R (>= %s)", ver), sapply(deps, flatten)),
                  collapse = ", ")
        } else sprintf("R (>= %s)", ver)

        .write_description(desc, file.path(pkgname, "DESCRIPTION"))

        printLog(Log,
                 "  NB: this package now depends on R (>= ", ver, ")\n")
    }

    resave_data_rda <- function(pkgname, resave_data)
    {
        if (resave_data == "no") return()
        ddir <- file.path(pkgname, "data")
        if(resave_data == "best") {
            files <- Sys.glob(c(file.path(ddir, "*.rda"),
                                file.path(ddir, "*.RData"),
                                file.path(pkgname, "R", "sysdata.rda")))
            messageLog(Log, "re-saving image files")
            resaveRdaFiles(files)
            rdas <- checkRdaFiles(files)
            if(any(rdas$compress %in% c("bzip2", "xz")))
                fixup_R_dep(pkgname, "2.10")
        } else {
            ## ddir need not exist if just R/sysdata.rda
            rdas <- checkRdaFiles(Sys.glob(c(file.path(ddir, "*.rda"),
                                             file.path(ddir, "*.RData"))))
            if(nrow(rdas)) {
                update <- with(rdas, ASCII | compress == "none" | version < 2)
                if(any(update)) {
                    messageLog(Log, "re-saving image files")
                    resaveRdaFiles(row.names(rdas)[update], "gzip")
                }
            }
            if(file.exists(f <- file.path(pkgname, "R", "sysdata.rda"))) {
                rdas <- checkRdaFiles(f)
                update <- with(rdas, ASCII | compress == "none" | version < 2)
                if(any(update)) {
                    messageLog(Log, "re-saving sysdata.rda")
                    resaveRdaFiles(f, "gzip")
                }
            }
        }
    }


    resave_data_others <- function(pkgname, resave_data)
    {
        if (resave_data == "no") return()
        ddir <- file.path(pkgname, "data")
        dataFiles <- grep("\\.(rda|RData)$",
                          list_files_with_type(ddir, "data"),
                          invert = TRUE, value = TRUE)
        if (!length(dataFiles)) return()
        Rs <- grep("\\.[Rr]$", dataFiles, value = TRUE)
        if (length(Rs)) { # these might use .txt etc
            messageLog(Log, "re-saving .R files as .rda")
            ## ensure utils is visible
            library("utils")
            lapply(Rs, function(x){
                envir <- new.env(hash = TRUE)
                sys.source(x, chdir = TRUE, envir = envir)
                save(list = ls(envir, all.names = TRUE),
                     file = sub("\\.[Rr]$", ".rda", x),
                     compress = TRUE, compression_level = 9,
                     envir = envir)
                unlink(x)
            })
            printLog(Log,
                     "  NB: *.R converted to .rda: other files may need to be removed\n")
        }
        tabs <- grep("\\.(CSV|csv|TXT|tab|txt)$", dataFiles, value = TRUE)
        if (length(tabs)) {
            messageLog(Log, "re-saving tabular files")
            if (resave_data == "gzip") {
                lapply(tabs, function(nm) {
                    ## DiceDesign/data/greenwood.table.txt is missing NL
                    x <- readLines(nm, warn = FALSE)
                    con <- gzfile(paste(nm, "gz", sep = "."), "wb")
                    writeLines(x, con)
                    close(con)
                    unlink(nm)
                })
            } else {
                OK <- TRUE
                lapply(tabs, function(nm) {
                    x <- readLines(nm, warn = FALSE)
                    nm3 <- paste(nm, c("gz", "bz2", "xz"), sep = ".")
                    con <- gzfile(nm3[1L], "wb", compression = 9L); writeLines(x, con); close(con)
                    con <- bzfile(nm3[2L], "wb", compression = 9L); writeLines(x, con); close(con)
                    con <- xzfile(nm3[3L], "wb", compression = 9L); writeLines(x, con); close(con)
                    sizes <- file.size(nm3) * c(0.9, 1, 1)
                    ind <- which.min(sizes)
                    if(ind > 1) OK <<- FALSE
                    unlink(c(nm, nm3[-ind]))
                })
                if (!OK) fixup_R_dep(pkgname, "2.10")
            }
        }
    }

    force <- FALSE
    vignettes <- TRUE
    manual <- TRUE  # Install the manual if Rds contain \Sexprs
    with_md5 <- FALSE
    INSTALL_opts <- character()
    pkgs <- character()
    options(showErrorCalls = FALSE, warn = 1)

    ## Read in build environment file.
    Renv <- Sys.getenv("R_BUILD_ENVIRON", unset = NA)
    if(!is.na(Renv)) {
        ## Do not read any build environment file if R_BUILD_ENVIRON is
        ## set to empty of something non-existent.
        if(nzchar(Renv) && file.exists(Renv)) readRenviron(Renv)
    } else {
        ## Read in ~/.R/build.Renviron[.rarch] (if existent).
        rarch <- .Platform$r_arch
        if (nzchar(rarch) &&
            file.exists(Renv <- paste("~/.R/build.Renviron", rarch, sep = ".")))
            readRenviron(Renv)
        else if (file.exists(Renv <- "~/.R/build.Renviron"))
            readRenviron(Renv)
    }

    ## Configurable variables.
    compact_vignettes <- Sys.getenv("_R_BUILD_COMPACT_VIGNETTES_", "no")
    resave_data <- Sys.getenv("_R_BUILD_RESAVE_DATA_", "gzip")

    keep_empty <-
        config_val_to_logical(Sys.getenv("_R_BUILD_KEEP_EMPTY_DIRS_", "FALSE"))

    if (is.null(args)) {
        args <- commandArgs(TRUE)
        ## it seems that splits on spaces, so try harder.
        args <- paste(args, collapse = " ")
        args <- strsplit(args,'nextArg', fixed = TRUE)[[1L]][-1L]
    }

    while(length(args)) {
        a <- args[1L]
        if (a %in% c("-h", "--help")) {
            Usage()
            do_exit(0L)
        }
        else if (a %in% c("-v", "--version")) {
            cat("R add-on package builder: ",
                R.version[["major"]], ".",  R.version[["minor"]],
                " (r", R.version[["svn rev"]], ")\n", sep = "")
            cat("",
                "Copyright (C) 1997-2013 The R Core Team.",
                "This is free software; see the GNU General Public License version 2",
                "or later for copying conditions.  There is NO warranty.",
                sep = "\n")
            do_exit(0L)
        } else if (a == "--force") {
            force <- TRUE
        } else if (a == "--keep-empty-dirs") {
            keep_empty <- TRUE
        } else if (a == "--no-build-vignettes") {
            vignettes <- FALSE
        } else if (a == "--no-vignettes") { # pre-3.0.0 version
            stop("'--no-vignettes' is defunct:\n  use '--no-build-vignettes' instead",
                 call. = FALSE, domain = NA)
        } else if (a == "--resave-data") {
            resave_data <- "best"
        } else if (a == "--no-resave-data") {
            resave_data <- "no"
        } else if (substr(a, 1, 14) == "--resave-data=") {
            resave_data <- substr(a, 15, 1000)
        } else if (a == "--no-manual") {
            manual <- FALSE
        } else if (substr(a, 1, 20) == "--compact-vignettes=") {
            compact_vignettes <- substr(a, 21, 1000)
        } else if (a == "--compact-vignettes") {
            compact_vignettes <- "qpdf"
        } else if (a == "--md5") {
            with_md5 <- TRUE
        } else if (substr(a, 1, 1) == "-") {
            message("Warning: unknown option ", sQuote(a))
        } else pkgs <- c(pkgs, a)
        args <- args[-1L]
    }

    if(!compact_vignettes %in% c("no", "qpdf", "gs", "gs+qpdf", "both")) {
        warning(gettextf("invalid value for '--compact-vignettes', assuming %s",
                         "\"qpdf\""),
                domain = NA)
        compact_vignettes <-"qpdf"
    }

    Sys.unsetenv("R_DEFAULT_PACKAGES")

    startdir <- getwd()
    if (is.null(startdir))
        stop("current working directory cannot be ascertained")
    R_platform <- Sys.getenv("R_PLATFORM", "unknown-binary")
    libdir <- tempfile("Rinst")

    if (WINDOWS) {
        ## Some people have *assumed* that R_HOME uses / in Makefiles
        ## Spaces in paths might still cause trouble.
        rhome <- chartr("\\", "/", R.home())
        Sys.setenv(R_HOME = rhome)
    }

    for(pkg in pkgs) {
        Log <- newLog() # if not stdin; on.exit(closeLog(Log))
        ## remove any trailing /, for Windows' sake
        pkg <- sub("/$", "", pkg)
        ## 'Older versions used $pkg as absolute or relative to $startdir.
        ## This does not easily work if $pkg is a symbolic link.
        ## Hence, we now convert to absolute paths.'
        setwd(startdir)
	res <- tryCatch(setwd(pkg), error = function(e)e)
	if (inherits(res, "error")) {
            errorLog(Log, "cannot change to directory ", sQuote(pkg))
            do_exit(1L)
        }
        pkgdir <- getwd()
        pkgname <- basename(pkgdir)
        checkingLog(Log, "for file ", sQuote(file.path(pkg, "DESCRIPTION")))
        f <- file.path(pkgdir, "DESCRIPTION")
        if (file.exists(f)) {
            desc <- try(.read_description(f))
            if (inherits(desc, "try-error") || !length(desc)) {
                resultLog(Log, "EXISTS but not correct format")
                do_exit(1L)
            }
            resultLog(Log, "OK")
        } else {
            resultLog(Log, "NO")
            do_exit(1L)
        }
        intname <- desc["Package"]
        ## make a copy, cd to parent of copy
        setwd(dirname(pkgdir))
        filename <- paste0(intname, "_", desc["Version"], ".tar")
        filepath <- file.path(startdir, filename)
        Tdir <- tempfile("Rbuild")
        dir.create(Tdir, mode = "0755")
        if (WINDOWS) {
            ## This preserves read-only for files, but not dates
            if (!file.copy(pkgname, Tdir, recursive = TRUE)) {
                errorLog(Log, "copying to build directory failed")
                do_exit(1L)
            }
        } else {
            ## This should preserve dates and permissions (subject to
            ## umask, if that is consulted which it seems it usually is not).
            ## Permissions are increased later.
	    cp_sw <- if(Sys.info()[["sysname"]] == "Linux") ## << need GNU cp
		## unfortunately, '-pr' does not dereference sym.links
		"-Lr --preserve=timestamps" else "-pr"
            if (system(paste("cp", cp_sw, shQuote(pkgname), shQuote(Tdir)))) {
                errorLog(Log, "copying to build directory failed")
                do_exit(1L)
            }
        }
        setwd(Tdir)

        ## Now correct the package name (PR#9266)
        if (pkgname != intname) {
            if (!file.rename(pkgname, intname)) {
                message(gettextf("Error: cannot rename directory to %s",
                                 sQuote(intname)), domain = NA)
                do_exit(1L)
            }
            pkgname <- intname
        }

        ## prepare the copy
        messageLog(Log, "preparing ", sQuote(pkgname), ":")
        prepare_pkg(normalizePath(pkgname, "/"), desc, Log);
        owd <- setwd(pkgname)
        ## remove exclude files
        allfiles <- dir(".", all.files = TRUE, recursive = TRUE,
                        full.names = TRUE, include.dirs = TRUE)
        allfiles <- substring(allfiles, 3L)  # drop './'
        bases <- basename(allfiles)
        exclude <- rep(FALSE, length(allfiles))
        ignore <- get_exclude_patterns()
        ## handle .Rbuildignore:
        ## 'These patterns should be Perl regexps, one per line,
        ##  to be matched against the file names relative to
        ##  the top-level source directory.'
        ignore_file <- file.path(pkgdir, ".Rbuildignore")
        if (file.exists(ignore_file))
            ignore <- c(ignore, readLines(ignore_file, warn = FALSE))
        for(e in ignore[nzchar(ignore)])
            exclude <- exclude | grepl(e, allfiles, perl = TRUE,
                                       ignore.case = TRUE)

        isdir <- dir.exists(allfiles)
        ## old (pre-2.10.0) dirnames
        exclude <- exclude | (isdir & (bases %in%
                                       c("check", "chm", .vc_dir_names)))
        exclude <- exclude | (isdir & grepl("([Oo]ld|\\.Rcheck)$", bases))
        ## FIXME: GNU make uses GNUmakefile (note capitalization)
        exclude <- exclude | bases %in% c("Read-and-delete-me", "GNUMakefile")
        ## Mac resource forks
        exclude <- exclude | grepl("^\\._", bases)
        exclude <- exclude | (isdir & grepl("^src.*/[.]deps$", allfiles))
	## Windows DLL resource file
        exclude <- exclude | (allfiles == paste0("src/", pkgname, "_res.rc"))
        ## inst/doc/.Rinstignore is a mistake
        exclude <- exclude | grepl("inst/doc/[.](Rinstignore|build[.]timestamp)$", allfiles)
        exclude <- exclude | grepl("vignettes/[.]Rinstignore$", allfiles)
        ## leftovers
        exclude <- exclude | grepl("^.Rbuildindex[.]", allfiles)
        exclude <- exclude | (bases %in% .hidden_file_exclusions)
        unlink(allfiles[exclude], recursive = TRUE, force = TRUE)
        setwd(owd)

        ## Fix up man, R, demo inst/doc directories
        res <- .check_package_subdirs(pkgname, TRUE)
        if (any(sapply(res, length))) {
            messageLog(Log, "excluding invalid files")
            print(res) # FIXME print to Log?
        }
        setwd(Tdir)
        ## Fix permissions for all files to be at least 644, and dirs 755
        ## Not restricted by umask.
	if (!WINDOWS) .Call(dirchmod, pkgname, group.writable=FALSE)
        ## Add build stamp to the DESCRIPTION file.
        add_build_stamp_to_description_file(file.path(pkgname,
                                                      "DESCRIPTION"))
        ## Add expanded R fields to the DESCRIPTION file.
        add_expanded_R_fields_to_description_file(file.path(pkgname,
                                                            "DESCRIPTION"))
        messageLog(Log,
                   "checking for LF line-endings in source and make files")
        fix_nonLF_in_source_files(pkgname, Log)
        fix_nonLF_in_make_files(pkgname, Log)
        messageLog(Log, "checking for empty or unneeded directories");
        find_empty_dirs(pkgname)
        for(dir in c("Meta", "R-ex", "chtml", "help", "html", "latex")) {
            d <- file.path(pkgname, dir)
            if (dir.exists(d)) {
                msg <- paste("WARNING: Removing directory",
                             sQuote(d),
                             "which should only occur",
                             "in an installed package")
                printLog(Log, paste(strwrap(msg, indent = 0L, exdent = 2L),
                                    collapse = "\n"), "\n")
                unlink(d, recursive = TRUE)
            }
        }
        ## remove subarch build directories
        unlink(file.path(pkgname,
                         c("src-i386", "src-x64", "src-x86_64", "src-ppc")),
               recursive = TRUE)

        ## work on 'data' directory if present
        if(dir.exists(file.path(pkgname, "data")) ||
           file_test("-f", file.path(pkgname, "R", "sysdata.rda"))) {
            messageLog(Log, "looking to see if a 'data/datalist' file should be added")
            ## in some cases data() needs the package installed as
            ## there are links to the package's namespace
            tryCatch(add_datalist(pkgname),
                     error = function(e)
                     printLog(Log, "  unable to create a 'datalist' file: may need the package to be installed\n"))
            ## allow per-package override
            resave_data1 <- parse_description_field(desc, "BuildResaveData",
                                                    resave_data, FALSE)
            resave_data_others(pkgname, resave_data1)
            resave_data_rda(pkgname, resave_data1)
        }

	## add NAMESPACE if the author didn't write one
	if(!file.exists(namespace <- file.path(pkgname, "NAMESPACE")) ) {
	    messageLog(Log, "creating default NAMESPACE file")
	    writeDefaultNamespace(namespace)
	}

        if(with_md5) {
	    messageLog(Log, "adding MD5 file")
            .installMD5sums(pkgname)
        } else {
            ## remove any stale file
            unlink(file.path(pkgname, "MD5"))
        }

        ## Finalize
        filename <- paste0(pkgname, "_", desc["Version"], ".tar.gz")
        filepath <- file.path(startdir, filename)
        ## NB: tests/reg-packages.R relies on this exact format!
        messageLog(Log, "building ", sQuote(filename))
        res <- utils::tar(filepath, pkgname, compression = "gzip",
                          compression_level = 9L,
                          tar = Sys.getenv("R_BUILD_TAR"),
                          extra_flags = NULL) # use trapdoor
        if (res) {
            errorLog(Log, "packaging into .tar.gz failed")
            do_exit(1L)
        }
        message("") # blank line

        setwd(startdir)
        unlink(Tdir, recursive = TRUE)
        on.exit() # cancel closeLog
        closeLog(Log)
    }
    do_exit(0L)
}
