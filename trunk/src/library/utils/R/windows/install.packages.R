## called as
# .install.winbinary(pkgs = pkgs, lib = lib, contriburl = contriburl,
#                    method = method, available = available,
#                    destdir = destdir,
#                    installWithVers = installWithVers,
#                    dependencies = dependencies)

.install.winbinary <-
    function(pkgs, lib, repos = getOption("repos"),
             contriburl = contrib.url(repos),
             method, available = NULL, destdir = NULL,
             installWithVers = FALSE, dependencies = FALSE)
{
    unpackPkg <- function(pkg, pkgname, lib, installWithVers = FALSE)
    {
        ## Create a temporary directory and unpack the zip to it
        ## then get the real package & version name, copying the
        ## dir over to the appropriate install dir.
        tmpDir <- tempfile(, lib)
        if (!dir.create(tmpDir))
            stop(sprintf(gettext("unable to create temporary directory '%s'"),
                         normalizePath(tmpDir)), domain = NA, call. = FALSE)
        cDir <- getwd()
        on.exit(setwd(cDir), add = TRUE)
        res <- zip.unpack(pkg, tmpDir)
        setwd(tmpDir)
        res <- tools::checkMD5sums(pkgname, file.path(tmpDir, pkgname))
        if(!is.na(res) && res) {
            cat(sprintf(gettext("package '%s' successfully unpacked and MD5 sums checked\n"), pkgname))
            flush.console()
        }

        ## Check to see if this is a bundle or a single package
        if (file.exists("DESCRIPTION")) {
            ## Bundle
            conts <- read.dcf("DESCRIPTION", fields="Contains")[1,]
            if (is.na(conts))
                stop("malformed bundle DESCRIPTION file, no Contains field")
            else
                pkgs <- strsplit(conts," ")[[1]]
            ## now check the MD5 sums
            res <- TRUE
            for (curPkg in pkgs) res <- res &
            tools::checkMD5sums(pkgname, file.path(tmpDir, curPkg))
            if(!is.na(res) && res) {
                cat(sprintf(gettext("bundle '%s' successfully unpacked and MD5 sums checked\n"), pkgname))
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
                if (installWithVers) {
                    instPath <- file.path(lib,
                                          paste(desc[1,1], desc[1,2], sep="_"))
                }
                else instPath <- file.path(lib, desc[1,1])

                ## If the package is already installed w/ this
                ## instName, remove it.  If it isn't there, the unlink call will
                ## still return success.
                ret <- unlink(instPath, recursive=TRUE)
                if (ret == 0) {
                    ## Move the new package to the install lib and
                    ## remove our temp dir
                    ret <- file.rename(file.path(tmpDir, curPkg), instPath)
                    if(!ret)
                        warning(sprintf(gettext(
                             "unable to move temporary installation '%s' to '%s'"),
                                        normalizePath(file.path(tmpDir, curPkg)),
                                        normalizePath(instPath)),
                                domain = NA, call. = FALSE, immediate. = TRUE)
                } else {
                    warning(sprintf(gettext(
                             "cannot remove prior installation of package '%s'"),
                                    curPkg),
                            domain = NA, call. = FALSE, immediate. = TRUE)
                }
            }
        }
        setwd(cDir)
        unlink(tmpDir, recursive=TRUE)
    }

    if(!length(pkgs)) return(invisible())
    oneLib <- length(lib) == 1


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
        for(i in seq(along=pkgs))
            unpackPkg(pkgs[i], pkgnames[i], lib, installWithVers)
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

    for(bundle in names(bundles))
        pkgs[ pkgs %in% bundles[[bundle]] ] <- bundle
    depends <- is.character(dependencies) ||
    (is.logical(dependencies) && dependencies)
    if(depends && is.logical(dependencies))
        dependencies <-  c("Depends", "Imports", "Suggests")
    if(depends && !oneLib) {
        warning("Do not know which element of 'lib' to install dependencies into\nskipping dependencies")
        depends <- FALSE
    }
    if(depends) { # check for dependencies, recursively
        p0 <- p1 <- unique(pkgs) # this is ok, as 1 lib only
        have <- .packages(all.available = TRUE)
        repeat {
            if(any(miss <- ! p1 %in% row.names(available))) {
                cat(sprintf(ngettext(sum(miss),
                                     "dependency '%s' is not available",
                                     "dependencies '%s' are not available"),
                    paste(sQuote(p1[miss]), sep=", ")), "\n\n", sep ="")
                flush.console()
            }
            p1 <- p1[!miss]
            deps <- as.vector(available[p1, dependencies])
            deps <- .clean_up_dependencies(deps, available)
            if(!length(deps)) break
            toadd <- deps[! deps %in% c("R", have, pkgs)]
            if(length(toadd) == 0) break
            pkgs <- c(toadd, pkgs)
            p1 <- toadd
        }
        for(bundle in names(bundles))
            pkgs[ pkgs %in% bundles[[bundle]] ] <- bundle
        pkgs <- unique(pkgs)
        pkgs <- pkgs[pkgs %in% row.names(available)]
        if(length(pkgs) > length(p0)) {
            added <- setdiff(pkgs, p0)
            cat(ngettext(length(added),
                         "also installing the dependency ",
                         "also installing the dependencies "),
                paste(sQuote(added), collapse=", "), "\n\n", sep="")
            flush.console()
            pkgnames <- pkgs # not zips, now
        }
    }

    foundpkgs <- download.packages(pkgs, destdir = tmpd, available = available,
                                   contriburl = contriburl, method = method,
                                   type = "win.binary")

    if(length(foundpkgs)) {
        update <- unique(cbind(pkgs, lib))
        colnames(update) <- c("Package", "LibPath")
        for(lib in unique(update[,"LibPath"])) {
            oklib <- lib==update[,"LibPath"]
            for(p in update[oklib, "Package"])
            {
                okp <- p == foundpkgs[, 1]
                if(any(okp))
                    unpackPkg(foundpkgs[okp, 2], foundpkgs[okp, 1], lib,
                              installWithVers)
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
    install.packages(NULL, .libPaths()[1], dependencies=TRUE, type = type)
}

menuInstallLocal <- function()
{
    install.packages(choose.files('',filters=Filters[c('zip','All'),]),
                     .libPaths()[1], repos = NULL)
}

### the following function supports .install.winbinaries()

zip.unpack <- function(zipname, dest)
{
    if(file.exists(zipname)) {
        if((unzip <- getOption("unzip")) != "internal") {
            system(paste(unzip, "-oq", zipname, "-d", dest),
                   show = FALSE, invisible = TRUE)
        } else {
            .Internal(int.unzip(zipname, NULL, dest))
        }
    } else stop(sprintf(gettext("zipfile '%s' not found"), zipname),
                domain = NA)
}
