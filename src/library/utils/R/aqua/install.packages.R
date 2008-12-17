#  File src/library/utils/R/aqua/install.packages.R
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


## edited from windows/.install.winbinary
##
.install.macbinary <-
    function(pkgs, lib, repos = getOption("repos"),
             contriburl = contrib.url(repos, type="mac.binary"),
             method, available = NULL, destdir = NULL,
             dependencies = FALSE, ...)
{
    link.html.help<-function(verbose = FALSE, ...)
    {
        html <- getOption("htmlhelp")
        # update only if temporary help files exist
        if (!is.null(html) && html && file.exists(paste(tempdir(),"/.R/doc",sep=''))) {
            #.Script("sh", "help-links.sh", paste(tempdir(), paste(.libPaths(),
            #                                                      collapse = " ")))
            make.packages.html()
        }
    }
    untar<-function(what, where)
    {
        xcode <- system(paste("tar zxf \"", path.expand(what), "\" -C \"",
                              path.expand(where), "\"", sep=''), intern=FALSE)
        if (xcode)
            warning(gettextf("'tar' returned non-zero exit code %d", ,xcode),
                    domain = NA, call. = FALSE)
    }

    unpackPkg <- function(pkg, pkgname, lib)
    {
        ## Create a temporary directory and unpack the zip to it
        ## then get the real package & version name, copying the
        ## dir over to the appropriate install dir.
        tmpDir <- tempfile(, lib)
        if (!dir.create(tmpDir))
            stop(gettextf("unable to create temporary directory '%s'",
                          tmpDir),
                 domain = NA, call. = FALSE)
        cDir <- getwd()
        on.exit(setwd(cDir), add = TRUE)
        res <- untar(pkg, tmpDir)
        setwd(tmpDir)
        res <- tools::checkMD5sums(pkgname, file.path(tmpDir, pkgname))
        if(!is.na(res) && res) {
            cat(gettextf("package '%s' successfully unpacked and MD5 sums checked\n",
                         pkgname))
            flush.console()
        }

        ## Check to see if this is a bundle or a single package
        if (file.exists("DESCRIPTION")) {
            ## Bundle
            conts <- read.dcf("DESCRIPTION", fields="Contains")[1,]
            if (is.na(conts))
                stop("malformed bundle DESCRIPTION file, no Contains field")
            else
                pkgs <- strsplit(conts," ")[[1L]]
            ## now check the MD5 sums
            res <- TRUE
            for (curPkg in pkgs) res <- res &
            tools::checkMD5sums(pkgname, file.path(tmpDir, curPkg))
            if(!is.na(res) && res) {
                cat(gettextf("bundle '%s' successfully unpacked and MD5 sums checked\n",
                             pkgname))
                flush.console()
            }
        } else pkgs <- pkgname

        for (curPkg in pkgs) {
            desc <- read.dcf(file.path(curPkg, "DESCRIPTION"),
                             c("Package", "Version"))
            instPath <- file.path(lib, desc[1L,1L])

            ## If the package is already installed w/ this
            ## instName, remove it.  If it isn't there, the unlink call will
            ## still return success.
            ret <- unlink(instPath, recursive=TRUE)
            if (ret == 0L) {
                ## Move the new package to the install lib and
                ## remove our temp dir
                ret <- file.rename(file.path(tmpDir, curPkg), instPath)
                if(!ret)
                    warning(gettextf("unable to move temporary installation '%s' to '%s'",
                                     file.path(tmpDir, curPkg), instPath),
                            domain = NA, call. = FALSE)
            } else
                stop("cannot remove prior installation of package ",
                     sQuote(curPkg), call. = FALSE)
        }
        setwd(cDir)
        unlink(tmpDir, recursive=TRUE)
    }

    if(!length(pkgs)) return(invisible())
    oneLib <- length(lib) == 1L

    pkgnames <- basename(pkgs)
    pkgnames <- sub("\\.tgz$", "", pkgnames)
    pkgnames <- sub("\\.tar\\.gz$", "", pkgnames)
    pkgnames <- sub("_.*$", "", pkgnames)
    ## there is no guarantee we have got the package name right:
    ## foo.zip might contain package bar or Foo or FOO or ....
    ## but we can't tell without trying to unpack it.
    if(is.null(contriburl)) {
        for(i in seq_along(pkgs))
            unpackPkg(pkgs[i], pkgnames[i], lib)
        link.html.help(verbose=TRUE)
        return(invisible())
    }
    tmpd <- destdir
    nonlocalcran <- length(grep("^file:", contriburl)) < length(contriburl)
    if(is.null(destdir) && nonlocalcran) {
        tmpd <- file.path(tempdir(), "downloaded_packages")
        if (!file.exists(tmpd) && !dir.create(tmpd))
            stop(gettextf("unable to create temporary directory '%s'", tmpd),
                 domain = NA)
    }

    depends <- is.character(dependencies) ||
    (is.logical(dependencies) && dependencies)
    if(depends && is.logical(dependencies))
        dependencies <-  c("Depends", "Imports", "Suggests")
    if(depends && !oneLib) {
        warning("Do not know which element of 'lib' to install dependencies into\nskipping dependencies")
        depends <- FALSE
    }
    if(is.null(available))
        available <- available.packages(contriburl = contriburl,
                                        method = method)
    bundles <- .find_bundles(available)
    for(bundle in names(bundles))
        pkgs[ pkgs %in% bundles[[bundle]] ] <- bundle
    p0 <- unique(pkgs)
    miss <-  !p0 %in% row.names(available)
    if(sum(miss)) {
        warning(sprintf(ngettext(sum(miss),
                                 "package %s is not available",
                                 "packages %s are not available"),
                        paste(sQuote(p0[miss]), collapse=", ")),
                domain = NA, call. = FALSE)
        flush.console()
    }
    p0 <- p0[!miss]

    if(depends) { # check for dependencies, recursively
        p1 <- p0 # this is ok, as 1 lib only
        ## where should we be looking?
        ## should this add the library we are installing to?
        installed <- installed.packages(fields = c("Package", "Version"))
        not_avail <- character(0L)
	repeat {
	    deps <- apply(available[p1, dependencies, drop = FALSE],
                          1L, function(x) paste(x[!is.na(x)], collapse=", "))
	    res <- .clean_up_dependencies2(deps, installed, available)
            not_avail <- c(not_avail, res[[2L]])
            deps <- unique(res[[1L]])
            ## R should not get to here, but be safe
            deps <- deps[!deps %in% c("R", pkgs)]
	    if(!length(deps)) break
	    pkgs <- c(deps, pkgs)
	    p1 <- deps
	}
        if(length(not_avail)) {
            warning(sprintf(ngettext(length(not_avail),
                                     "dependency %s is not available",
                                     "dependencies %s are not available"),
                            paste(sQuote(not_avail), collapse=", ")),
                    domain = NA, call. = FALSE, immediate. = TRUE)
            flush.console()
        }

        for(bundle in names(bundles))
            pkgs[ pkgs %in% bundles[[bundle]] ] <- bundle
        pkgs <- unique(pkgs)
        pkgs <- pkgs[pkgs %in% row.names(available)]
        if(length(pkgs) > length(p0)) {
            added <- setdiff(pkgs, p0)
            message(sprintf(ngettext(length(added),
                                     "also installing the dependency %s",
                                     "also installing the dependencies %s"),
                            paste(sQuote(added), collapse=", ")),
                    "\n", domain = NA)
            flush.console()
            pkgnames <- pkgs # not zips, now
        }
        p0 <- pkgs
    }

    foundpkgs <- download.packages(p0, destdir = tmpd, available = available,
                                   contriburl = contriburl, method = method,
                                   type = "mac.binary", ...)

    if(length(foundpkgs)) {
        update <- unique(cbind(pkgs, lib))
        colnames(update) <- c("Package", "LibPath")
        for(lib in unique(update[,"LibPath"])) {
            oklib <- lib==update[,"LibPath"]
            for(p in update[oklib, "Package"])
            {
                okp <- p == foundpkgs[, 1L]
                if(any(okp))
                    unpackPkg(foundpkgs[okp, 2L], foundpkgs[okp, 1L], lib)
            }
        }
        if(!is.null(tmpd) && is.null(destdir))
            cat("\n", gettextf("The downloaded packages are in\n\t%s", tmpd),
                "\n", sep = "")
        link.html.help(verbose = TRUE)
    } else if(!is.null(tmpd) && is.null(destdir)) unlink(tmpd, TRUE)

    invisible()
}
