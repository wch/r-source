#  File src/library/tools/R/build.R
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

#### R based engine for R CMD build

### emulation of Perl Logfile.pm

newLog <- function(filename = "")
{
    con <- if (nzchar(filename)) file(filename, "wt") else 0L
    list(filename = filename, con = con, stars = "*", warnings = 0L)
}

closeLog <- function(Log) if (Log$con > 2) close(Log$con)

printLog <- function(Log, ...) {
    cat(..., sep = "")
    if (Log$con > 0L) cat(..., file = Log$con, sep = "")
}

## unused
setStars <- function(Log, stars) {Log$stars <- stars; Log}

checkingLog <- function(Log, ...)
    printLog(Log, Log$stars, " checking ", ..., " ...")

creatingLog <- function(Log, text)
    printLog(Log, Log$stars," creating ", text, " ...")

messageLog <- function(Log, ...)
{
    text <- paste(..., sep="")
##     cat(Log$stars, " ",
##         gsub("\n", paste("\n", Log$stars, " ", sep = ""), text, fixed = TRUE),
##         sep = "\n", file = Log$con)
    printLog(Log, Log$stars, " ", ..., "\n")
}
resultLog <- function(Log, text) printLog(Log, " ", text, "\n")

errorLog <- function(Log, ...)
{
    resultLog(Log, "ERROR")
    text <- paste(..., sep="")
    if (length(text) && nzchar(text)) printLog(Log, ..., "\n")
}

warningLog <- function(Log, text="")
{
    resultLog(Log, "WARNING")
    if (nzchar(text)) messageLog(Log, text)
    Log$warnings <- Log$warnings+1L
    invisible(Log)
}

noteLog <- function(Log, text="")
{
    resultLog(Log, "NOTE")
    if (nzchar(text)) messageLog(Log, text)
}

summaryLog <- function(Log)
{
    if (Log$warnings > 1)
        printLog(Log,
                 sprintf("WARNING: There were %d warnings, see\n  %s\nfor details\n",
                         Log$warnings, sQuote(Log$filename)))
    else if (Log$warnings == 1)
        printLog(Log,
                 sprintf("WARNING: There was 1 warning, see\n  %s\nfor details\n",
                         sQuote(Log$filename)))
}

### formerly Perl R::Utils::get_exclude_patterns

## Return list of file patterns excluded by R CMD build and check.
## Kept here so that we ensure that the lists are in sync, but not
## exported.
## <NOTE>
## Has Unix-style '/' path separators hard-coded.
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
      "^src/so_locations$"
      )
## </NOTE>



### based on Perl build script

.build_packages <- function(args = NULL)
{
    ## this requires on Windows make tar gzip

    WINDOWS <- .Platform$OS.type == "windows"

    Sys.umask("022") # Perl version did not have this.

    writeLinesNL <- function(text, file)
    {
        ## a version that uses NL line endings everywhere
        con <- file(file, "wb")
        on.exit(close(con))
        writeLines(text, con)
    }

    ## This version of shell_with_capture merges stdout and stderr
    ## Used to install package and build vignettes.
    shell_with_capture <- function (command, args) {
        outfile <- tempfile("xshell")
        on.exit(unlink(outfile))
        status <- system2(command, args, outfile, outfile)
        list(status = status, stdout = readLines(outfile, warn = FALSE))
    }
    ## Run silently
    Ssystem <- function(command, args = character(), ...)
        system2(command, args, stdout = NULL, stderr = NULL, ...)


    .file_test <- function(op, x)
        switch(op,
               "-f" = !is.na(isdir <- file.info(x)$isdir) & !isdir,
               "-x" = (file.access(x, 1L) == 0L),
               stop(sprintf("test '%s' is not available", op), domain = NA))

    dir.exists <- function(x) !is.na(isdir <- file.info(x)$isdir) & isdir

    do_exit <- function(status = 1L) q("no", status = status, runLast = FALSE)

    env_path <- function(...) file.path(..., fsep = .Platform$path.sep)

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
            "  --no-vignettes        do not rebuild package vignettes",
            "  --no-manual           do not build the manual even if \\Sexprs are present",
            "",
            "  --binary              build pre-compiled binary packages, with options:",
            if (WINDOWS) "  --auto-zip            select zipping of data based on size",
            "  --use-zip-data        collect data files in zip archive",
            "  --no-docs             do not build and install documentation",
            "",
            "Report bugs to <r-bugs@r-project.org>.", sep="\n")
    }


    add_build_stamp_to_description_file <- function(ldpath)
    {
        lines <- readLines(ldpath)
        lines <- lines[nzchar(lines)] # Remove blank lines.
        ## Do not keep previous build stamps.
        lines <- lines[!grepl("^Packaged:", lines)]
        ## this is an optional function, so could fail
        user <- Sys.info()["user"]
        if (user == "unknown") user <- Sys.getenv("LOGNAME")
        lines <- c(lines,
                   paste("Packaged: ",
                         format(Sys.time(), '', tz='UTC', usetz=TRUE),
                         ";", " ", user, sep = ""))
        writeLinesNL(lines, ldpath)
    }

    temp_install_pkg <- function(pkgdir, libdir) {
	dir.create(libdir, mode = "0755")
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
	res <- shell_with_capture(cmd, args)
	if (res$status) {
	    printLog(Log, "      -----------------------------------\n")
	    printLog(Log, paste(c(res$stdout, ""),  collapse="\n"))
	    printLog(Log, "      -----------------------------------\n")
	    printLog(Log, "ERROR: Installation failed\n")
	    printLog(Log, "Removing installation dir\n")
	    unlink(libdir, recursive = TRUE)
	    do_exit(1)
	}
	TRUE
    }

    prepare_pkg <- function(pkgdir, desc, Log)
    {
        pkgname <- basename(pkgdir)
        checkingLog(Log, "DESCRIPTION meta-information")
        res <- try(.check_package_description("DESCRIPTION"))
        if (inherits(res, "try-error")) {
            resultLog(Log, "ERROR")
            messageLog(Log, "running .check_package_description failed")
        } else {
            if (any(sapply(res, length))) {
                resultLog(Log, "ERROR")
                print(res) # FIXME print to Log?
                do_exit(1L)
            } else resultLog(Log, "OK")
        }
        cleanup_pkg(pkgdir, Log)

        libdir <- tempfile("Rinst")

        pkgInstalled <- build_Rd_db(pkgdir, libdir)

        if (file.exists("INDEX")) update_Rd_index("INDEX", "man", Log)
        if (vignettes && dir.exists(file.path("inst", "doc")) &&
           length(list_files_with_type(file.path("inst", "doc"),
                                       "vignette"))) {
            if (!pkgInstalled) {
		messageLog(Log, "installing the package to re-build vignettes")
		pkgInstalled <- temp_install_pkg(pkgdir, libdir)
	    }

            ## Better to do this in a separate process: it might die
            creatingLog(Log, "vignettes")
            R_LIBS <- Sys.getenv("R_LIBS", NA_character_)
            if (!is.na(R_LIBS)) {
                on.exit(Sys.setenv(R_LIBS = R_LIBS))
                Sys.setenv(R_LIBS = env_path(libdir, R_LIBS))
            } else {
                on.exit(Sys.unsetenv("R_LIBS"))
                Sys.setenv(R_LIBS = libdir)
            }
            ## unset SWEAVE_STYLEPATH_DEFAULT here to avoid problems
            Sys.unsetenv("SWEAVE_STYLEPATH_DEFAULT")
            cmd <- file.path(R.home("bin"), "Rscript")
            args <- c("--vanilla",
                      "--default-packages=", # some vignettes assume methods
                      "-e", shQuote("tools::buildVignettes(dir = '.')"))
            res <- shell_with_capture(cmd, args)
            if (res$status) {
                resultLog(Log, "ERROR")
                printLog(Log, paste(c(res$stdout, ""),  collapse="\n"))
                do_exit(1L)
            } else resultLog(Log, "OK")
        }
        if (pkgInstalled) {
            unlink(libdir, recursive = TRUE)

	    ## And finally, clean up again.
            cleanup_pkg(pkgdir, Log)
        }
    }

    cleanup_pkg <- function(pkgdir, Log)
    {
        pkgname <- basename(pkgdir)
        if (dir.exists("src")) {
            setwd("src")
            messageLog(Log, "cleaning src")
            if (WINDOWS) {
                if (file.exists("Makefile.win")) {
                    Ssystem(Sys.getenv("MAKE", "make"),
                            "-f Makefile.win clean")
                } else {
                    if (file.exists("Makevars.win")) {
                        makefiles <- paste()
                        makefiles <- paste("-f",
                                           shQuote(file.path(R.home("share"), "make", "clean.mk")),
                                           "-f Makevars.win")
                        Ssystem(Sys.getenv("MAKE", "make"),
                                c(makefiles, "clean"))
                    }
                    unlink(c(Sys.glob(c("*.o")),
                             paste(pkgname, c(".a", ".dll", ".def"), sep="")))
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
                             paste(pkgname, c(".a", ".dll", ".def"), sep="")))
                    if (dir.exists(".libs")) unlink(".libs", recursive = TRUE)
                    if (dir.exists("_libs")) unlink("_libs", recursive = TRUE)
                }
            }
        }
        setwd(pkgdir)
        ## It is not clear that we want to do this: INSTALL should do so.
        ## Also, certain environment variables should be set according
        ## to 'Writing R Extensions', but were not in Perl version (nor
        ## was cleanup.win used).
        if (WINDOWS) {
            if (file.exists("cleanup.win")) {
                Sys.setenv(R_PACKAGE_NAME = pkgname)
                Sys.setenv(R_PACKAGE_DIR = pkgdir)
                Sys.setenv(R_LIBRARY_DIR = dirname(pkgdir))
                messageLog(Log, "running cleanup.win")
                Ssystem("sh", "./cleanup.win")
            }
        } else if (.file_test("-x", "cleanup")) {
            Sys.setenv(R_PACKAGE_NAME = pkgname)
            Sys.setenv(R_PACKAGE_DIR = pkgdir)
            Sys.setenv(R_LIBRARY_DIR = dirname(pkgdir))
            messageLog(Log, "running cleanup")
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
            ol <- readLines(oldindex)
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

    build_Rd_db <- function(pkgdir, libdir) {
    	db <- .build_Rd_db(pkgdir, stages=NULL, os=c("unix", "windows"), step=1)
    	if (!length(db)) return(FALSE)

    	# Strip the pkgdir off the names
    	names(db) <- substring(names(db), nchar(file.path(pkgdir, "man", ""))+1)

	containsSexprs <-
            which(sapply(db, function(Rd) getDynamicFlags(Rd)["\\Sexpr"]))
	if (!length(containsSexprs)) return(FALSE)

	messageLog(Log, "installing the package to process help pages")
	temp_install_pkg(pkgdir, libdir)

	containsBuildSexprs <-
            which(sapply(db, function(Rd) getDynamicFlags(Rd)["build"]))

	if (length(containsBuildSexprs)) {
	    for (i in containsBuildSexprs)
		db[[i]] <- prepare_Rd(db[[i]], stages="build",
                                      stage2=FALSE, stage3=FALSE)
	    messageLog(Log, "saving partial Rd database")
	    partial <- db[containsBuildSexprs]
	    dir.create("build", showWarnings=FALSE)
	    .saveRDS(partial, file.path("build", "partial.rdb"))
	}
	needRefman <- manual && any(sapply(db, function(Rd) any(getDynamicFlags(Rd)[c("install", "render")])))
	if (needRefman) {
	    messageLog(Log, "building the package manual")
	    dir.create("build", showWarnings=FALSE)
	    refman <- file.path(pkgdir, "build",
                                paste(basename(pkgdir), ".pdf", sep = ""))
	    ..Rd2dvi(c("--pdf", "--force", "--no-preview",
	               paste("--output=", refman, sep=""),
	               pkgdir), quit = FALSE)
        }
	return(TRUE)
    }

    ## These also fix up missing final NL
    fix_nonLF_in_source_files <- function(pkgname, Log)
    {
        if (!dir.exists(file.path(pkgname, "src"))) return()
        src_files <- dir(file.path(pkgname, "src"),
                         pattern = "\\.([cfh]|cc|cpp)$",
                         full.names=TRUE, recursive = TRUE)
        for (ff in src_files) {
            lines <- readLines(ff, warn = FALSE)
            writeLinesNL(lines, ff)
        }
    }

    fix_nonLF_in_make_files <- function(pkgname, Log)
    {
        if (!dir.exists(file.path(pkgname, "src"))) return()
         for (f in c("Makefile", "Makefile.in", "Makefile.win",
                     "Makevars", "Makevars.in", "Makevars.win")) {
             if (!file.exists(ff <- file.path(pkgname, "src", f))) next
             lines <- readLines(ff, warn = FALSE)
             writeLinesNL(lines, ff)
         }
     }

    find_empty_dirs <- function(d)
    {
        ## dir(recursive = TRUE) does not include directories, so
        ## we do need to do this recursively
        files <- dir(d, all.files = TRUE, full.names = TRUE)
        if (length(files) <= 2L) # always has ., ..
            printLog(Log, "WARNING: directory ", sQuote(d), " is empty\n")
        isdir <- file.info(files)$isdir
        for (d in files[isdir]) {
            if (grepl("/\\.+$", d)) next
            find_empty_dirs(d)
        }
    }

    force <- FALSE
    vignettes <- TRUE
    binary <- FALSE
    manual <- TRUE  # Install the manual if Rds contain \Sexprs
    INSTALL_opts <- character()
    pkgs <- character()
    options(showErrorCalls=FALSE, warn = 1)

    ## read in ~/.R/build.Renviron[.rarch]
    rarch <- .Platform$r_arch
    if (nzchar(rarch) &&
        file.exists(Renv <- paste("~/.R/build.Renviron", rarch, sep = ".")))
        readRenviron(Renv)
    else if (file.exists(Renv <- "~/.R/build.Renviron")) readRenviron(Renv)

    if (is.null(args)) {
        args <- commandArgs(TRUE)
        ## it seems that splits on spaces, so try harder.
        args <- paste(args, collapse=" ")
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
                "Copyright (C) 1997-2010 The R Core Development Team.",
                "This is free software; see the GNU General Public License version 2",
                "or later for copying conditions.  There is NO warranty.",
                sep="\n")
            do_exit(0L)
        } else if (a == "--force") {
            force <- TRUE
        } else if (a == "--no-vignettes") {
            vignettes <- FALSE
        } else if (a == "--binary") {
            binary <- TRUE
        } else if (WINDOWS && a == "--auto-zip") {
            INSTALL_opts <- c(INSTALL_opts, "--auto-zip")
        } else if (a == "--use-zip-data") {
            INSTALL_opts <- c(INSTALL_opts, "--use-zip-data")
        } else if (a == "--no-docs") {
            INSTALL_opts <- c(INSTALL_opts, "--no-docs")
        } else if (a == "--no-manual") {
            manual <- FALSE
        } else if (substr(a, 1, 1) == "-") {
            message("Warning: unknown option ", sQuote(a))
        } else pkgs <- c(pkgs, a)
        args <- args[-1L]
    }
    if (!binary && length(INSTALL_opts))
        message("** Options ",
                sQuote(paste(INSTALL_opts, collapse=" ")),
                " are only for '--binary'  and will be ignored")

    Sys.unsetenv("R_DEFAULT_PACKAGES")
    startdir <- getwd()
    if (is.null(startdir))
        stop("current working directory cannot be ascertained")
    R_platform <- Sys.getenv("R_PLATFORM", "unknown-binary")
    gzip <- Sys.getenv("R_GZIPCMD", "gzip")
    ## The tar.exe in Rtools has --force-local by default, but this
    ## enables people to use Cygwin or MSYS tar.
    TAR <- Sys.getenv("TAR", if (WINDOWS) "tar --force-local" else "tar")
    GZIP <- Sys.getenv("R_GZIPCMD", "gzip")
    libdir <- tempfile("Rinst");

    for(pkg in pkgs) {
        Log <- newLog() # if not stdin; on.exit(closeLog(Log))
        ## remove any trailing /, for Windows' sake
        pkg <- sub("/$", "", pkg)
        ## 'Older versions used $pkg as absolute or relative to $startdir.
        ## This does not easily work if $pkg is a symbolic link.
        ## Hence, we now convert to absolute paths.'
        setwd(startdir)
        res <- try(setwd(pkg), silent = TRUE)
        if (inherits(res, "try-error")) {
            errorLog(Log, "cannot change to directory ", sQuote(pkg))
            do_exit(1L)
        }
        pkgdir <- getwd()
        pkgname <- basename(pkgdir)
        checkingLog(Log, "for file ", sQuote(file.path(pkg, "DESCRIPTION")))
        f <- file.path(pkgdir, "DESCRIPTION")
        if (file.exists(f)) {
            desc <- try(read.dcf(f))
            if (inherits(desc, "try-error") || !length(desc)) {
                resultLog(Log, "EXISTS but not correct format")
                do_exit(1L)
            }
            desc <- desc[1L, ]
            resultLog(Log, "OK")
        } else {
            resultLog(Log, "NO")
            do_exit(1L)
        }
        intname <- desc["Package"]
        ## FIXME: why not copy and then prepare the copy?
        messageLog(Log, "preparing ", sQuote(intname), ":")
        prepare_pkg(pkgdir, desc, Log);
        setwd(dirname(pkgdir))
        filename <- paste(intname, "_", desc["Version"], ".tar", sep="")
        filepath <- file.path(startdir, filename)
        ## -h means dereference symbolic links: some prefer -L
        res <- system(paste(TAR, "-chf", shQuote(filepath), pkgname))
        if (!res) {
            Tdir <- tempfile("Rbuild")
            dir.create(Tdir, mode = "0755")
            setwd(Tdir)
            res <- system(paste(TAR, "-xf",  shQuote(filepath)))
        }
        if (res) {
            errorLog(Log, "copying to build directory failed")
            do_exit(1L)
        }

        ## FIXME: fix the dirname here.
        owd <- setwd(pkgname)
        ## remove exclude files
        allfiles <- dir(".", all.files = TRUE, recursive = TRUE,
                        full.names = TRUE)
        ## this does not include dirs, so add non-empty ones back
        allfiles <- c(allfiles, unique(dirname(allfiles)))
        allfiles <- substring(allfiles, 3L)  # drop './'
        bases <- basename(allfiles)
        exclude <- rep(FALSE, length(allfiles))
        ignore <- get_exclude_patterns()
        ## handle .Rbuildignore:
        ## 'These patterns should be Perl regexps, one per line,
        ##  to be matched against the file names relative to
        ##  the top-level source directory.'
        ignore_file <- file.path(pkgdir, ".Rbuildignore")
        if (file.exists(ignore_file)) ignore <- c(ignore, readLines(ignore_file))
        for(e in ignore[nzchar(ignore)])
            exclude <- exclude | grepl(e, allfiles, perl = TRUE,
                                       ignore.case = WINDOWS)

        isdir <- file_test("-d", allfiles)
        ## Version-control directories
        vcdirs <- c("CVS", ".svn", ".arch-ids", ".bzr", ".git", ".hg")
        ## old (pre-2.10.0) dirnames
        exclude <- exclude | (isdir & (bases %in% c("check", "chm", vcdirs)))
        exclude <- exclude | (isdir & grepl("([Oo]ld|\\.Rcheck)$", bases))
        ## FIXME: GNU make uses GNUmakefile (note capitalization)
        exclude <- exclude | bases %in% c("Read-and-delete-me", "GNUMakefile")
        ## Mac resource forks
        exclude <- exclude | grepl("^\\._", bases)
	## Windows DLL resource file
        exclude <- exclude | (bases == paste("src/", pkgname, "_res.rc", sep=""))
        unlink(allfiles[exclude], recursive = TRUE)
        setwd(owd)
        ## Now correct the package name (PR#9266)
        if (pkgname != intname) {
            if (!file.rename(pkgname, intname)) {
                message("Error: cannot rename directory to ", sQuote(intname))
                do_exit(1L)
            }
            pkgname <- intname
        }
        ## Fix up man, R, demo inst/doc directories
        res <- .check_package_subdirs(pkgname, TRUE)
        if (any(sapply(res, length))) {
            messageLog(Log, "excluding invalid files")
            print(res) # FIXME print to Log?
        }
        setwd(Tdir)
        if (!WINDOWS) {
            ## Fix permissions
            allfiles <- dir(pkgname, all.files = TRUE, recursive = TRUE,
                            full.names = TRUE)
            allfiles <- c(allfiles, unique(dirname(allfiles)))
            isdir <- file_test("-d", allfiles)
	    ## 'Directories should really be mode 00755 if possible.'
            Sys.chmod(allfiles[isdir], "0755")
	    ## 'Files should be readable by everyone, and writable
	    ## only for user.  This leaves a bit of uncertainty
	    ## about the execute bits.'
            files <- allfiles[!isdir]
            mode <- file.info(files)$mode
            ## equivalent of mode = (mode | 0644) & 0755
            mode <- (mode | "0644") & "0755"
            Sys.chmod(files, mode)
        }
        ## Add build stamp to the DESCRIPTION file.
        add_build_stamp_to_description_file(file.path(pkgname, "DESCRIPTION"))
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
                unlink(d, recursive=TRUE)
            }
        }
        ## remove subarch build directories
        unlink(file.path(pkgname,
                         c("src-i386", "src-x64", "src-x86_64", "src-ppc")),
               recursive = TRUE)

        ## Finalize
        if (binary) {
            messageLog(Log, "building binary distribution")
            setwd(startdir)
            libdir <- tempfile("Rinst")
            dir.create(libdir, mode = "0755")
            srcdir <- file.path(Tdir, pkgname)
            cmd <- if (WINDOWS)
                paste(shQuote(file.path(R.home("bin"), "Rcmd.exe")),
                      "INSTALL -l", shQuote(libdir),
                      "--build", paste(INSTALL_opts, collapse = " "),
                      shQuote(pkgdir))
            else
                 paste(shQuote(file.path(R.home("bin"), "R")),
                       "CMD INSTALL -l", shQuote(libdir),
                      "--build", paste(INSTALL_opts, collapse = " "),
                       shQuote(pkgdir))
            if (system(cmd)) {
                errorLog(Log, "Installation failed")
                do_exit(1)
            }
        } else {
            if (grepl("darwin", R.version$os)) {
                ## precaution for Mac OS X to omit resource forks
                ## we can't tell the running OS version from R.version$os
                Sys.setenv(COPYFILE_DISABLE = 1) # Leopard
                Sys.setenv(COPY_EXTENDED_ATTRIBUTES_DISABLE = 1) # Tiger
            }
            messageLog(Log, "building ", sQuote(paste(filename, ".gz", sep="")))
            ## should not be any symlinks, so remove -h?
            cmd <- paste(TAR, "-chf", shQuote(filepath), pkgname)
            res <- system(cmd)
            if (!res)
                res <- system(paste(shQuote(GZIP), "-9f", shQuote(filepath)))
            if (res) {
                errorLog(Log, "packaging into .tar.gz failed")
                do_exit(1L)
            }
            setwd(startdir)
            unlink(Tdir, recursive = TRUE)
            closeLog(Log)
            message("") # blank line
        }
    }
    do_exit(0L)
}
