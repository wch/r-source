.install.winbinary <-
    function(pkgs, lib, repos = CRAN,
             contriburl = contrib.url(repos),
             CRAN = getOption("repos"),
             method, available = NULL, destdir = NULL,
             installWithVers = FALSE, dependencies = FALSE)
{
    unpackPkg <- function(pkg, pkgname, lib, installWithVers = FALSE)
    {
        ## the `spammy' (his phrase) comments are from Gentry
        ## However, at least some of his many errors have been removed

        ## Create a temporary directory and unpack the zip to it
        ## then get the real package & version name, copying the
        ## dir over to the appropriate install dir.
        tmpDir <- tempfile(, lib)
        if (!dir.create(tmpDir))
            stop('Unable to create temp directory ', tmpDir)
        cDir <- getwd()
        on.exit(setwd(cDir), add = TRUE)
        res <- zip.unpack(pkg, tmpDir)
        setwd(tmpDir)
        res <- tools::checkMD5sums(pkgname, file.path(tmpDir, pkgname))
        if(!is.na(res) && res) {
            cat("package", sQuote(pkgname),
                gettext("successfully unpacked and MD5 sums checked\n"))
            flush.console()
        }

        ## Check to see if this is a bundle or a single package
        if (file.exists("DESCRIPTION")) {
            ## Bundle
            conts <- read.dcf("DESCRIPTION",fields="Contains")[1,]
            if (is.na(conts))
                stop("malformed bundle DESCRIPTION file, no Contains field")
            else
                pkgs <- strsplit(conts," ")[[1]]
            ## now check the MD5 sums
            res <- TRUE
            for (curPkg in pkgs) res <- res &
            tools::checkMD5sums(pkgname, file.path(tmpDir, curPkg))
            if(!is.na(res) && res) {
                cat("bundle", sQuote(pkgname),
                    gettext("successfully unpacked and MD5 sums checked\n"))
                flush.console()
            }
        } else pkgs <- pkgname

        for (curPkg in pkgs) {
            desc <- read.dcf(file.path(curPkg, "DESCRIPTION"),
                             c("Package", "Version"))
            if (installWithVers) {
                instPath <- file.path(lib, paste(desc[1,1], desc[1,2], sep="_"))
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
                if(!ret) warning("unable to move temp installation ",
                                 sQuote(file.path(tmpDir, curPkg)),
                                 " to ",
                                 sQuote(instPath), call. = FALSE)
            } else
                stop("cannot remove prior installation of package ",
                     sQuote(curPkg), call. = FALSE)
        }
        setwd(cDir)
        unlink(tmpDir, recursive=TRUE)
    }

    if(!length(pkgs)) return(invisible())
    oneLib <- length(lib) == 1

    pkgnames <- basename(pkgs)
    pkgnames <- sub("\\.zip$", "", pkgnames)
    pkgnames <- sub("_[0-9.-]+$", "", pkgnames)
    ## there is no guarantee we have got the package name right:
    ## foo.zip might contain package bar or Foo or FOO or ....
    ## but we can't tell without trying to unpack it.
    inuse <- search()
    inuse <- sub("^package:", "", inuse[grep("^package:", inuse)])
    inuse <- pkgnames %in% inuse
    if(any(inuse)) {
        if(sum(inuse) == 1)
            warning("package ", pkgnames[inuse],
                    " is in use and will not be installed", call. = FALSE)
        else
            warning("packages ", paste(pkgnames[inuse], collapse=", "),
                    " are in use and will not be installed", call. = FALSE)
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
            stop('Unable to create temp directory ', tmpd)
    }

    if(is.null(available))
        available <- available.packages(contriburl = contriburl,
                                        method = method)
    bundles <- .find_bundles(available)
    for(bundle in names(bundles))
        pkgs[ pkgs %in% bundles[[bundle]] ] <- bundle
    if(dependencies && !oneLib) {
        warning("Do not know which element of 'lib' to install dependencies into\n", "skipping dependencies")
        dependencies <- FALSE
    }
    if(dependencies) { # check for dependencies, recursively
        p0 <- p1 <- unique(pkgs) # this is ok, as 1 lib only
        have <- .packages(all.available = TRUE)
        repeat {
            if(any(miss <- ! p1 %in% row.names(available))) {
                cat(gettext("dependencies "),
                    paste(sQuote(p1[miss]), sep=", "),
                    gettext(" are not available"), "\n\n", sep ="")
                flush.console()
            }
            p1 <- p1[!miss]
            deps <- as.vector(available[p1, c("Depends", "Suggests", "Imports")])
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
            cat(gettext("also installing the dependencies "),
                paste(sQuote(added), collapse=", "), "\n\n", sep="")
            flush.console()
            pkgnames <- pkgs # not zips, now
        }
    }

    foundpkgs <- download.packages(pkgs, destdir = tmpd, available = available,
                                   contriburl = contriburl, method = method,
                                   type = "win.binary")

    if(!is.null(foundpkgs)) {
        update <- cbind(pkgs, lib)
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
            cat("\n", gettext("The downloaded packages are in "), "\n",
                normalizePath(tmpd), "\n", sep = "")
        link.html.help(verbose = TRUE)
    } else if(!is.null(tmpd) && is.null(destdir)) unlink(tmpd, TRUE)

    invisible()
}

menuInstallCran <- function(type = getOption("pkgType"))
{
    a <- available.packages(contrib.url(getOption("repos"), type=type))
    if(NROW(a)) {
        contains <- .find_bundles(a, FALSE)
        extras <- unlist(lapply(names(contains), function(x)
                                paste(contains[[x]], " (", x, ")", sep="")))
        p <- sort(as.vector(c(a[, 1], extras)))
        sel <- select.list(p, , TRUE)
        if(!length(sel)) return(invisible())
        bundles <- grep("(", sel, fixed = TRUE)
        if(length(bundles)) sel[bundles] <-
            sub("[^(]*\\((.*)\\)", "\\1" , sel[bundles])
        install.packages(unique(sel), .libPaths()[1], available=a,
                         dependencies=TRUE, type = type)
    }
}

menuInstallLocal <- function()
{
    install.packages(choose.files('',filters=Filters[c('zip','All'),]),
                     .libPaths()[1], repos = NULL)
}

menuInstallBioc <- function()
{
    a <- available.packages(contrib.url(getOption("BIOC")))
    install.packages(select.list(a[,1], , TRUE), .libPaths()[1],
                     available = a, repos = getOption("BIOC"),
                     dependencies = TRUE)
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
    } else stop("zipfile ", zipname, " not found")
}
