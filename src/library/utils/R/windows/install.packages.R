#  File src/library/utils/R/windows/install.packages.R
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

## called as
# .install.winbinary(pkgs = pkgs, lib = lib, contriburl = contriburl,
#                    method = method, available = available,
#                    destdir = destdir,
#                    dependencies = dependencies)

.install.winbinary <-
    function(pkgs, lib, repos = getOption("repos"),
             contriburl = contrib.url(repos),
             method, available = NULL, destdir = NULL,
             dependencies = FALSE, ...)
{
    unpackPkg <- function(pkg, pkgname, lib)
    {
        ## Create a temporary directory and unpack the zip to it
        ## then get the real package name, copying the
        ## dir over to the appropriate install dir.
        lib <- normalizePath(lib)
        tmpDir <- tempfile(, lib)
        if (!dir.create(tmpDir))
            stop(gettextf("unable to create temporary directory '%s'",
                          normalizePath(tmpDir)),
                 domain = NA, call. = FALSE)
        cDir <- getwd()
        on.exit(setwd(cDir), add = TRUE)
        res <- zip.unpack(pkg, tmpDir)
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
                             c("Package", "Version", "Type"))
            if(desc[1, "Type"] %in% "Translation") {
                fp <- file.path(curPkg, "share", "locale")
                if(file.exists(fp)) {
                    langs <- dir(fp)
                    for(lang in langs) {
                        path0 <- file.path(fp, lang, "LC_MESSAGES")
                        mos <- dir(path0, full.names = TRUE)
                        path <- file.path(R.home("share"), "locale", lang,
                                          "LC_MESSAGES")
                        if(!file.exists(path))
                            if(!dir.create(path, FALSE, TRUE))
                                warning(gettextf("failed to create '%s'", path),
                                        domain = NA)
                        res <- file.copy(mos, path, overwrite = TRUE)
                        if(any(!res))
                            warning(gettextf("failed to create '%s'",
                                             paste(mos[!res], collapse=",")),
                                    domain = NA)
                    }
                }
                fp <- file.path(curPkg, "library")
                if(file.exists(fp)) {
                    spkgs <- dir(fp)
                    for(spkg in spkgs) {
                        langs <- dir(file.path(fp, spkg, "po"))
                        for(lang in langs) {
                            path0 <- file.path(fp, spkg, "po", lang,
                                               "LC_MESSAGES")
                            mos <- dir(path0, full.names = TRUE)
                            path <- file.path(R.home(), "library", spkg, "po",
                                              lang, "LC_MESSAGES")
                            if(!file.exists(path))
                                if(!dir.create(path, FALSE, TRUE))
                                    warning(gettextf("failed to create '%s'", path),
                                            domain = NA)
                            res <- file.copy(mos, path, overwrite = TRUE)
                        if(any(!res))
                            warning(gettextf("failed to create '%s'",
                                             paste(mos[!res], collapse=",")),
                                    domain = NA)
                        }
                    }
                }
            } else {
                ## We can't use CHM help -- this works even if not there.
                unlink(file.path(tmpDir, curPkg, "chtml"), recursive = TRUE)
                instPath <- file.path(lib, desc[1,1])

                ## If the package is already installed w/ this
                ## instName, remove it.  If it isn't there, the unlink call will
                ## still return success.
                ret <- unlink(instPath, recursive=TRUE)
                if (ret == 0) {
                    ## Move the new package to the install lib and
                    ## remove our temp dir
                    ret <- file.rename(file.path(tmpDir, curPkg), instPath)
                    if(!ret)
                        warning(gettextf("unable to move temporary installation '%s' to '%s'",
                                         normalizePath(file.path(tmpDir, curPkg)),
                                         normalizePath(instPath)),
                                domain = NA, call. = FALSE, immediate. = TRUE)
                } else {
                    warning(gettextf("cannot remove prior installation of package '%s'",
                                     curPkg),
                            domain = NA, call. = FALSE, immediate. = TRUE)
                }
            }
        }
        setwd(cDir)
        unlink(tmpDir, recursive=TRUE)
    }

    if(!length(pkgs)) return(invisible())

    ## look for packages/bundles in use.
    pkgnames <- basename(pkgs)
    pkgnames <- sub("\\.zip$", "", pkgnames)
    pkgnames <- sub("_[0-9.-]+$", "", pkgnames)
    ## there is no guarantee we have got the package name right:
    ## foo.zip might contain package bar or Foo or FOO or ....
    ## but we can't tell without trying to unpack it.
    inuse <- search()
    inuse <- sub("^package:", "", inuse[grep("^package:", inuse)])
    if(!is.null(contriburl)) { # otherwise no info on bundles
        if(is.null(available))
            available <- available.packages(contriburl = contriburl,
                                            method = method)
        bundles <- .find_bundles(available)
        for(bundle in names(bundles))
            if(any(bundles[[bundle]] %in% inuse)) inuse <- c(inuse, bundle)
    }
    inuse <- pkgnames %in% inuse
    if(any(inuse)) {
        warning(sprintf(ngettext(sum(inuse),
                "package '%s' is in use and will not be installed",
                "packages '%s' are in use and will not be installed"),
                        paste(pkgnames[inuse], collapse=", ")),
                call. = FALSE, domain = NA, immediate. = TRUE)
        pkgs <- pkgs[!inuse]
        pkgnames <- pkgnames[!inuse]
    }

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
            stop(gettextf("unable to create temporary directory '%s'",
                          normalizePath(tmpd)),
                 domain = NA)
    }

    if(is.null(available))
        available <- available.packages(contriburl = contriburl,
                                        method = method)
    pkgs <- getDependencies(pkgs, dependencies, available, lib)

    foundpkgs <- download.packages(pkgs, destdir = tmpd, available = available,
                                   contriburl = contriburl, method = method,
                                   type = "win.binary", ...)

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
            ## tends to be a long path on Windows
            cat("\n", gettextf("The downloaded packages are in\n\t%s",
                               normalizePath(tmpd)), "\n", sep = "")
        link.html.help(verbose = TRUE)
    } else if(!is.null(tmpd) && is.null(destdir)) unlink(tmpd, TRUE)

    invisible()
}

menuInstallPkgs <- function(type = getOption("pkgType"))
{
    install.packages(NULL, .libPaths()[1L], dependencies=NA, type = type)
}

menuInstallLocal <- function()
{
    install.packages(choose.files('',filters=Filters[c('zip','All'),]),
                     .libPaths()[1L], repos = NULL)
}

### the following function supports .install.winbinaries()

zip.unpack <- function(zipname, dest)
{
    if(file.exists(zipname)) {
        if((unzip <- getOption("unzip")) != "internal") {
            system(paste(unzip, "-oq", zipname, "-d", dest),
                   show = FALSE, invisible = TRUE)
        } else unzip(zipname, exdir = dest)
    } else stop(gettextf("zipfile '%s' not found", zipname),
                domain = NA)
}
