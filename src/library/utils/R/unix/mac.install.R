#  File src/library/utils/R/unix/mac.install.R
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


if(substr(R.version$os, 1L, 6L) != "darwin") {
.install.macbinary <-
    function(pkgs, lib, repos = getOption("repos"),
             contriburl = contrib.url(repos, type="mac.binary"),
             method, available = NULL, destdir = NULL,
             dependencies = FALSE,
             lock = getOption("install.lock", FALSE), quiet = FALSE,
             ...)
    {}
} else {
## edited from windows/.install.winbinary
##
.install.macbinary <-
    function(pkgs, lib, repos = getOption("repos"),
             contriburl = contrib.url(repos, type="mac.binary"),
             method, available = NULL, destdir = NULL,
             dependencies = FALSE,
             lock = getOption("install.lock", FALSE), quiet = FALSE,
             ...)
{
    untar <- function(what, where)
    {
        ## FIXME: should this look for Sys.getenv('TAR')?
        ## Leopard has GNU tar, SL has BSD tar.
        xcode <- system(paste0("tar zxf \"", path.expand(what), "\" -C \"",
                               path.expand(where), "\""), intern=FALSE)
        if (xcode)
            warning(gettextf("'tar' returned non-zero exit code %d", xcode),
                    domain = NA, call. = FALSE)
    }

    unpackPkg <- function(pkg, pkgname, lib, lock = FALSE)
    {
        dir.exists <- function(x)
            !is.na(isdir <- file.info(x, extra_cols = FALSE)$isdir) & isdir
        ## Create a temporary directory and unpack the zip to it
        ## then get the real package & version name, copying the
        ## dir over to the appropriate install dir.
        tmpDir <- tempfile(, lib)
        if (!dir.create(tmpDir))
            stop(gettextf("unable to create temporary directory %s",
                          sQuote(tmpDir)),
                 domain = NA, call. = FALSE)
        cDir <- getwd()
        on.exit(setwd(cDir), add = TRUE)
        res <- untar(pkg, tmpDir)
        setwd(tmpDir)
        ## sanity check: people have tried to install source .tgz files
        if (!file.exists(file <- file.path(pkgname, "Meta", "package.rds")))
            stop(gettextf("file %s is not an OS X binary package", sQuote(pkg)),
                 domain = NA, call. = FALSE)
        desc <- readRDS(file)$DESCRIPTION
        if (length(desc) < 1L)
            stop(gettextf("file %s is not an OS X binary package", sQuote(pkg)),
                 domain = NA, call. = FALSE)
        desc <- as.list(desc)
        if (is.null(desc$Built))
            stop(gettextf("file %s is not an OS X binary package", sQuote(pkg)),
                 domain = NA, call. = FALSE)

        res <- tools::checkMD5sums(pkgname, file.path(tmpDir, pkgname))
        if(!quiet && !is.na(res) && res) {
            cat(gettextf("package %s successfully unpacked and MD5 sums checked\n",
                         sQuote(pkgname)))
            flush.console()
        }

        instPath <- file.path(lib, pkgname)
        if(identical(lock, "pkglock") || isTRUE(lock)) {
	    lockdir <- if(identical(lock, "pkglock"))
                file.path(lib, paste("00LOCK", pkgname, sep = "-"))
            else file.path(lib, "00LOCK")
	    if (file.exists(lockdir)) {
                stop(gettextf("ERROR: failed to lock directory %s for modifying\nTry removing %s",
                              sQuote(lib), sQuote(lockdir)), domain = NA)
	    }
	    dir.create(lockdir, recursive = TRUE)
	    if (!dir.exists(lockdir))
                stop(gettextf("ERROR: failed to create lock directory %s",
                              sQuote(lockdir)), domain = NA)
            ## Back up a previous version
            if (file.exists(instPath)) {
                file.copy(instPath, lockdir, recursive = TRUE)
        	on.exit({
         	    if (restorePrevious) {
                        try(unlink(instPath, recursive = TRUE))
        	    	savedcopy <- file.path(lockdir, pkgname)
        	    	file.copy(savedcopy, lib, recursive = TRUE)
        	    	warning(gettextf("restored %s", sQuote(pkgname)),
                                domain = NA, call. = FALSE, immediate. = TRUE)
        	    }
        	}, add=TRUE)
        	restorePrevious <- FALSE
            }
	    on.exit(unlink(lockdir, recursive = TRUE), add=TRUE)
        }
        ## If the package is already installed, remove it.  If it
        ## isn't there, the unlink call will still return success.
        ret <- unlink(instPath, recursive=TRUE)
        if (ret == 0L) {
            ## Move the new package to the install lib and
            ## remove our temp dir
            ret <- file.rename(file.path(tmpDir, pkgname), instPath)
            if(!ret) {
                warning(gettextf("unable to move temporary installation %s to %s",
                                 sQuote(file.path(tmpDir, pkgname)),
                                 sQuote(instPath)),
                        domain = NA, call. = FALSE)
                restorePrevious <- TRUE # Might not be used
            }
        } else
        stop(gettextf("cannot remove prior installation of package %s",
                      sQuote(pkgname)), call. = FALSE, domain = NA)
        setwd(cDir)
        unlink(tmpDir, recursive=TRUE)
    }

    if(!length(pkgs)) return(invisible())

    if(is.null(contriburl)) {
        pkgnames <- basename(pkgs)
        pkgnames <- sub("\\.tgz$", "", pkgnames)
        pkgnames <- sub("\\.tar\\.gz$", "", pkgnames)
        pkgnames <- sub("_.*$", "", pkgnames)
        ## there is no guarantee we have got the package name right:
        ## foo.zip might contain package bar or Foo or FOO or ....
        ## but we can't tell without trying to unpack it.
        for(i in seq_along(pkgs))
            unpackPkg(pkgs[i], pkgnames[i], lib, lock = lock)
        return(invisible())
    }
    tmpd <- destdir
    nonlocalcran <- length(grep("^file:", contriburl)) < length(contriburl)
    if(is.null(destdir) && nonlocalcran) {
        tmpd <- file.path(tempdir(), "downloaded_packages")
        if (!file.exists(tmpd) && !dir.create(tmpd))
            stop(gettextf("unable to create temporary directory %s",
                          sQuote(tmpd)),
                 domain = NA)
    }

    if(is.null(available))
        available <- available.packages(contriburl = contriburl,
                                        method = method)
    pkgs <- getDependencies(pkgs, dependencies, available, lib, binary = TRUE)

    foundpkgs <- download.packages(pkgs, destdir = tmpd, available = available,
                                   contriburl = contriburl, method = method,
                                   type = "mac.binary", quiet = quiet, ...)

    if(length(foundpkgs)) {
        update <- unique(cbind(pkgs, lib))
        colnames(update) <- c("Package", "LibPath")
        for(lib in unique(update[,"LibPath"])) {
            oklib <- lib==update[,"LibPath"]
            for(p in update[oklib, "Package"])
            {
                okp <- p == foundpkgs[, 1L]
                if(any(okp))
                    unpackPkg(foundpkgs[okp, 2L], foundpkgs[okp, 1L], lib,
                              lock = lock)
            }
        }
        if(!quiet && !is.null(tmpd) && is.null(destdir))
            cat("\n", gettextf("The downloaded binary packages are in\n\t%s", tmpd),
                "\n", sep = "")
    } else if(!is.null(tmpd) && is.null(destdir)) unlink(tmpd, recursive = TRUE)

    invisible()
}
}
