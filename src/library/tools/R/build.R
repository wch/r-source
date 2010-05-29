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

newLog <- function(class, filename = "")
{
    con <- if(nzchar(filename)) file(filename, "wt") else stdout()
    list(filename = filename, con = con, stars = "*", warnings = 0L)
}

closeLog <- function(Log) if(Log$con > 2) close(Log$con)

printLog <- function(Log, ...) cat(..., file = Log$con, sep = "")

## unused
setStars <- function(Log, stars) {Log$stars <- stars; Log}

checkingLog <- function(Log, ...)
    cat(Log$stars, " checking ", ..., " ...", file = Log$con, sep = "")

creatingLog <- function(Log, text)
    cat(Log$stars, " creating ", text, " ...", file = Log$con, sep = "")

messageLog <- function(Log, ...)
{
    text <- paste(..., sep="")
##     cat(Log$stars, " ",
##         gsub("\n", paste("\n", Log$stars, " ", sep = ""), text, fixed = TRUE),
##         sep = "\n", file = Log$con)
    cat(Log$stars, " ", ..., "\n", file = Log$con, sep = "")
}
resultLog <- function(Log, text) cat(" ", text, "\n", file = Log$con)

errorLog <- function(Log, ...)
{
    resultLog(Log, "ERROR")
    if(nzchar(paste(..., sep=""))) printLog(Log, ..., "\n")
}

warningLog <- function(Log, text)
{
    resultLog(Log, "WARNING")
    if(nzchar(text)) messageLog(Log, text)
    Log$warnings <- Log$warnings+1L
    invisible(Log)
}

noteLog <- function(Log, text)
{
    resultLog(Log, "NOTE")
    if(nzchar(text)) messageLog(text)
}

summaryLog <- function(Log, text)
{
    if(Log$warnings > 1)
        cat(sprintf("WARNING: There were %d warnings, see\n%%s\nfor details\n",
                    Log$warnings, sQuote(Log$filename)),
            file = Log$con)
    else if (Log$warnings == 1)
        cat(sprintf("WARNING: There were 1 warning, see\n%s\nfor details\n",
                    sQuote(Log$filename)),
            file = Log$con)
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

## FIXME: could use .shell_with_capture

.build_packages <- function(args = NULL)
{
    WINDOWS <- .Platform$OS.type == "windows"

    Sys.umask("022") # Perl version did not have this.

    writeLinesNL <- function(text, file)
    {
        ## a version that uses NL line endings everywhere
        con <- file(file, "wb")
        on.exit(close(con))
        writeLines(text, con)
    }

    ## Run silently
    Ssystem <- function(command, ...)
        system(paste(if(WINDOWS) "sh", command, ">/dev/null 2>&1"), ...)

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
            "  --force               force overwriting of INDEX file",
            "  --no-vignettes        do not rebuild package vignettes",
            "",
            "  --binary              build pre-compiled binary packages, with options:",
            if(WINDOWS) "  --auto-zip            select zipping of data based on size",
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
        lines <- c(lines,
                   paste("Packaged: ",
                         format(Sys.time(), '', tz='UTC', usetz=TRUE),
                         ";", " ", Sys.info()["login"], sep = ""))
        writeLinesNL(lines, ldpath)
    }

    prepare_pkg <- function(pkgdir, desc, Log)
    {
        pkgname <- basename(pkgdir)
        checkingLog(Log, "DESCRIPTION meta-information")
        res <- try(.check_package_description("DESCRIPTION"))
        if(inherits(res, "try-error")) {
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
        if(file.exists("INDEX")) update_Rd_index("INDEX", "man", Log)
        if(vignettes && dir.exists(file.path("inst", "doc")) &&
           length(list_files_with_type(file.path("inst", "doc"),
                                       "vignette"))) {
            messageLog(Log, "installing the package to re-build vignettes")
            libdir <- tempfile("Rinst")
            dir.create(libdir, mode = "0755")
            cmd <- if(WINDOWS)
                paste(file.path(R.home("bin"), "Rcmd.exe"),
                      "INSTALL -l", shQuote(libdir), shQuote(pkgdir))
            else
                 paste(file.path(R.home("bin"), "R"),
                       "CMD INSTALL -l", shQuote(libdir), shQuote(pkgdir))
            res <- Ssystem(cmd)
            if(res) {
                errorLog(Log, "Installation failed")
                printLog(Log, "Removing installation dir\n")
                unlink(libdir, recursive = TRUE)
                do_exit(1)
            }
            creatingLog(Log, "vignettes")
            R_LIBS <- Sys.getenv("R_LIBS", NA_character_)
            if(!is.na(R_LIBS)) {
                on.exit(Sys.setenv(R_LIBS = R_LIBS))
                Sys.setenv(R_LIBS = env_path(libdir, R_LIBS))
            }
            Tfile <- tempfile()
            msgcon <- file(Tfile, "w")
            sink(msgcon, type = "message")
            sink(msgcon)
            res <- try(buildVignettes(dir = "."))
            sink()
            sink(stderr(), type = "message")
            close(msgcon)
            if(inherits(res, "try-error")) {
                errorLog(Log , "")
                Lines <- readLines(Tfile)
                Lines <- grep("^>", Lines, invert=TRUE, value=TRUE)
                printLog(Log, paste(c(Lines, ""),  collapse="\n"))
                do_exit(1L)
            } else resultLog(Log, "OK")
            unlink(libdir, recursive = TRUE)
            if(inherits(res, 'try-error')) do_exit(1L)

	    ## And finally, clean up again.
            cleanup_pkg(pkgdir, Log)
        }
    }

    cleanup_pkg <- function(pkgdir, Log)
    {
        pkgname <- basename(pkgdir)
        if(dir.exists("src")) {
            setwd("src")
            messageLog(Log, "cleaning src")
            if(WINDOWS) {
                if(file.exists("Makefile.win")) {
                    Ssystem(paste(Sys.getenv("MAKE", "make"), "-f Makefile.win clean"))
                } else {
                    if(file.exists("Makevars.win")) {
                        makefiles <- paste()
                        makefiles <- paste("-f",
                                           shQuote(file.path(R.home("share"), "make", "clean.mk")),
                                           "-f Makevars.win")
                        Ssystem(paste(Sys.getenv("MAKE", "make"), makefiles, "clean"))
                    }
                    if(dir.exists("_libs")) unlink("_libs", recursive = TRUE)
                }
            } else {
                makefiles <- paste("-f",
                                   shQuote(file.path(R.home("etc"),
                                                     Sys.getenv("R_ARCH"),
                                                     "Makeconf")))
                if(file.exists("Makefile")) {
                    makefiles <- paste(makefiles, "-f", "Makefile")
                    Ssystem(paste(Sys.getenv("MAKE", "make"), makefiles, "clean"))
                } else {
                    if(file.exists("Makevars")) {
                        ## ensure we do have a 'clean' target.
                        makefiles <- paste(makefiles, "-f",
                                       shQuote(file.path(R.home("share"), "make", "clean.mk")),
                                           "-f Makevars")
                        Ssystem(paste(Sys.getenv("MAKE", "make"), makefiles, "clean"))
                    }
                    ## Also cleanup possible Windows leftovers ...
                    unlink(c(Sys.glob(c("*.o", "*.sl", "*.so", "*.dylib")),
                             paste(pkgname, c(".a", ".dll", ".def"), sep="")))
                    if(dir.exists(".libs")) unlink(".libs", recursive = TRUE)
                    if(dir.exists("_libs")) unlink("_libs", recursive = TRUE)
                }
            }
        }
        setwd(pkgdir)
        if(!WINDOWS && .file_test("-x", "./cleanup")) {
            messageLog(Log, "running cleanup")
            Ssystem("./cleanup")
        }
    }

    update_Rd_index <- function(oldindex, Rd_files, Log)
    {
        newindex <- tempfile()
        res <- try(Rdindex(Rd_files, newindex))
        if(inherits(res, "try-error")) {
            errorLog(Log, "computing Rd index failed")
            do_exit(1L)
        }
        checkingLog(Log, "whether ", sQuote(oldindex), " is up-to-date")
        if(file.exists(oldindex)) {
            ol <- readLines(oldindex)
            nl <- readLines(newindex)
            if (!identical(ol, nl)) {
                resultLog(Log, "NO")
                if (force) {
                    messageLog(Log, "overwriting ", sQuote(oldindex),
			      " as '--force' was given")
                    unlink(oldindex)
                    file.rename(newindex, oldindex)
                } else {
                    messageLog(Log, "use '--force' to overwrite ",
			      "the existing ", sQuote(oldindex))
                    unlink(newindex)
                }
            } else {
                resultLog(Log, "OK")
                unlink(newindex)
            }
        } else {
            resultLog(Log, "NO")
            messageLog("creating new ", sQuote(oldindex))
            file.rename(newindex, oldindex)
        }
    }

    ## These also fix up missing final NL
    fix_nonLF_in_source_files <- function(pkgname, Log)
    {
        if(!dir.exists(file.path(pkgname, "src"))) return()
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
        if(!dir.exists(file.path(pkgname, "src"))) return()
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
        if(length(files) <= 2L) # always has ., ..
            printLog(Log, "WARNING: directory ", sQuote(d), " is empty\n")
        isdir <- file.info(files)$isdir
        isdir[1:2] <- FALSE
        for (d in files[isdir]) find_empty_dirs(d)
    }

    force <- FALSE
    vignettes <- TRUE
    binary <- FALSE
    INSTALL_opts <- character()
    pkgs <- character()
    options(showErrorCalls=FALSE)

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
        } else if (substr(a, 1, 1) == "-") {
            message("Warning: unknown option ", sQuote(a))
        } else pkgs <- c(pkgs, a)
        args <- args[-1L]
    }
    if(!binary && length(INSTALL_opts))
        message("** Options ",
                sQuote(paste(INSTALL_opts, collapse=" ")),
                " are only for '--binary'  and will be ignored")

    Sys.unsetenv("R_DEFAULT_PACKAGES")
    startdir <- getwd()
    R_platform <- Sys.getenv("R_PLATFORM", "unknown-binary")
    gzip <- Sys.getenv("R_GZIPCMD", "gzip")
    ## The tar.exe in Rtools has --force-local by default, but this
    ## enables people to use Cygwin or MSYS tar.

    TAR <- Sys.getenv("TAR", NA)
    TAR <- if(is.na(TAR)) {if(WINDOWS) "tar --force-local" else "tar"}
    else shQuote(TAR)
    GZIP <- Sys.getenv("R_GZIPCMD")
    if (!nzchar(GZIP)) GZIP <- "gzip"
    libdir <- tempfile("Rinst");

    for(pkg in pkgs) {
        Log <- newLog() # if not stdin; on.exit(closeLog(Log))
        ## remove any trailing /, for Windows' sake
        pkg <- sub("/$", "", pkg)
        ## 'Older versions used $pkg as absolute or relative to $startdir.
        ## This does not easily work if $pkg is a symbolic link.
        ## Hence, we now convert to absolute paths.'
        setwd(startdir)
        res <- try(setwd(pkg))
        if(inherits(res, "try-error")) {
            errorLog(Log, "cannot change to directory ", sQuote(pkg))
            do_exit(1L)
        }
        pkgdir <- getwd()
        pkgname <- basename(pkgdir)
        checkingLog(Log, "for file ", sQuote(file.path(pkg, "DESCRIPTION")))
        f <- file.path(pkgdir, "DESCRIPTION")
        if(file.exists(f)) {
            desc <- read.dcf(f)[1L, ]
            resultLog(Log, "OK")
        } else {
            resultLog(Log, "NO")
            do_exit(1L)
        }
        intname <- desc["Package"]
        messageLog(Log, "preparing ", sQuote(intname), ":")
        prepare_pkg(pkgdir, desc, Log);
        messageLog(Log, "removing junk files")
        setwd(pkgdir)
        if(file.exists("DESCRIPTION.in") && file.exists("DESCRIPTION"))
            unlink("DESCRIPTION")
        ff <- dir(".", all.files = TRUE, recursive = TRUE, full.names = TRUE)
        unlink(grep("^\\.(RData|Rhistory)$", ff, value = TRUE))
        setwd(dirname(pkgdir))
        filename <- paste(intname, "_", desc["Version"], ".tar", sep="")
        filepath <- file.path(startdir, filename)
        res <- system(paste(TAR, "-chf", shQuote(filepath), pkgname))
        if(!res) {
            Tdir <- tempfile("Rbuild")
            dir.create(Tdir, mode = "0755")
            setwd(Tdir)
            res <- system(paste(TAR, "-xf",  shQuote(filepath)))
        }
        if(res) {
            errorLog(Log, "copying to build directory failed")
            do_exit(1L)
        }

        ## remove exclude files
        ## FIXME: what about case-insensitivity on Windows?
        allfiles <- dir(".", all.files = TRUE, recursive = TRUE,
                        full.names = TRUE)
        allfiles <- c(allfiles, unique(dirname(allfiles)))
        ## FIXME check for '.'
        allfiles <- substring(allfiles, 3L)  # drop './'
        bases <- basename(allfiles)
        exclude <- rep(FALSE, length(allfiles))
        ## basic exclusions: could be streamlined
        for(e in get_exclude_patterns())
            exclude <- exclude | grepl(e, allfiles, perl = TRUE)
        ## handle .Rbuildignore
        ignore_file <- file.path(pkgdir, ".Rbuildignore")
        if(file.exists(ignore_file)) {
            ## 'These patterns should be Perl regexps, one per line,
            ##  to be matched against the file names relative to
            ##  the top-level source directory.'
            ignore <- readLines(ignore_file)
            for(e in ignore)
                exclude <- exclude | grepl(e, allfiles, perl = TRUE)
        }
        isdir <- file_test("-d", allfiles)
        vcdirs <- c("CVS", ".svn", ".arch-ids", ".bzr", ".git", ".hg")
        exclude <- exclude | (isdir & (bases %in% c("check", "chm", vcdirs)))
        exclude <- exclude | (isdir & grepl("([Oo]ld|\\.Rcheck)$", bases))
        exclude <- exclude | bases %in% c("Read-and-delete-me", "GNUMakefile")
        ## Mac resource forks
        exclude <- exclude | grepl("^\\._", bases)
	## Windows DLL resource file
        exclude <- exclude | bases == paste("src/", pkgname, "_res.rc", sep="")
        unlink(allfiles[exclude], recursive = TRUE)
        ## Now correct the package name (PR#9266)
        if (pkgname != intname) {
            if(!file.rename(pkgname, intname)) {
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
        if(!WINDOWS) {
            ## Fix permissions
            ## FIXME does not include directories.
            allfiles <- dir(".", all.files = TRUE, recursive = TRUE,
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
            if(dir.exists(d)) {
                print(Log, strwrap(paste("WARNING: Removing directory",
                                         sQuote(d),
                                         "which should only occur",
                                         "in an installed package\n"),
                                   indent = 0L, exdent = 2L))
                unlink(f, recursive=TRUE)
            }
        }
        ## remove subarch build directories
        unlink(c("src-i386", "src-x64", "src-x86_64", "src-ppc"),
               recursive = TRUE)

        ## Finalize
        if (binary) {
            messageLog(Log, "building binary distribution")
            setwd(startdir)
            libdir <- tempfile("Rinst")
            dir.create(libdir, mode = "0755")
            srcdir <- file.path(Tdir, pkgname)
            cmd <- if(WINDOWS)
                paste(file.path(R.home("bin"), "Rcmd.exe"),
                      "INSTALL -l", shQuote(libdir),
                      "--build", paste(INSTALL_opts, collapse = " "),
                      shQuote(pkgdir))
            else
                 paste(file.path(R.home("bin"), "R"),
                       "CMD INSTALL -l", shQuote(libdir),
                      "--build", paste(INSTALL_opts, collapse = " "),
                       shQuote(pkgdir))
            res <- system(cmd)
            if(res) {
                errorLog(Log, "Installation failed")
                do_exit(1)
            }
        } else {
            if(grepl("darwin", R.version$os)) {
                ## precaution for Mac OS X to omit resource forks
                Sys.setenv(COPYFILE_DISABLE = 1) # Leopard
                Sys.setenv(COPY_EXTENDED_ATTRIBUTES_DISABLE = 1) # Tiger
            }
            messageLog(Log, "building ", sQuote(paste(filename, ".gz", sep="")))
            cmd <- paste(TAR, "-chf", shQuote(filepath), pkgname)
            res <- system(cmd)
            if(!res)
                res <- system(paste(shQuote(GZIP), "-9f", shQuote(filepath)))
            if(res) {
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
