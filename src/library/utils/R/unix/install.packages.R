install.packages <- function(pkgs, lib, repos = CRAN,
                             contriburl = contrib.url(repos, type),
                             CRAN = getOption("CRAN"),
                             method, available = NULL, destdir = NULL,
                             installWithVers = FALSE, dependencies = FALSE,
                             type)
{
    if(missing(pkgs) || !length(pkgs))
        stop("no packages were specified")

    if(missing(lib) || is.null(lib)) {
        lib <- .libPaths()[1]
        if(length(.libPaths()) > 1)
            warning("argument 'lib' is missing: using\n\t",
                    sQuote(lib), immediate.=TRUE)
    }

    if(!file.exists(file.path(R.home(),"bin","INSTALL")))
        stop("This version of R is not set up to install source packages\nIf it was installed from an RPM, you may need the R-devel RPM")

    if(is.null(repos) & missing(contriburl)) {
        update <- cbind(pkgs, lib) # for side-effect of recycling to same length
        cmd0 <- paste(file.path(R.home(),"bin","R"), "CMD INSTALL")
        if (installWithVers)
            cmd0 <- paste(cmd0, "--with-package-versions")
        for(i in 1:nrow(update)) {
            cmd <- paste(cmd0, "-l", shQuote(update[i, 2]),
                         shQuote(update[i, 1]))
            if(system(cmd) > 0)
                warning("Installation of package ", sQuote(update[i, 1]),
                        " had non-zero exit status")
        }
        return(invisible())
    }

    oneLib <- length(lib) == 1
    tmpd <- destdir
    nonlocalcran <- length(grep("^file:", contriburl)) < length(contriburl)
    if(is.null(destdir) && nonlocalcran) {
        tmpd <- file.path(tempdir(), "downloaded_packages")
        if (!file.exists(tmpd) && !dir.create(tmpd))
            stop('Unable to create temp directory ', tmpd)
    }

    if(dependencies && !oneLib) {
        warning("Do not know which element of 'lib' to install dependencies into\n", "skipping dependencies")
        dependencies <- FALSE
    }
    if(is.null(available))
        available <- available.packages(contriburl = contriburl,
                                        method = method)
    bundles <- .find_bundles(available)
    for(bundle in names(bundles))
        pkgs[ pkgs %in% bundles[[bundle]] ] <- bundle
    if(dependencies) { # check for dependencies, recursively
        p0 <- p1 <- unique(pkgs) # this is ok, as 1 lib only
        have <- .packages(all.available = TRUE)
        repeat {
            if(any(miss <- ! p1 %in% row.names(available))) {
                cat(gettext("dependencies "),
                    paste(sQuote(p1[miss]), sep=", "),
                    gettext(" are not available"), "\n\n", sep ="")
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
        }
    }

    foundpkgs <- download.packages(pkgs, destdir = tmpd, available = available,
                                   contriburl = contriburl, method = method)

    ## at this point pkgs may contain duplicates,
    ## the same pkg in different libs
    if(!is.null(foundpkgs)) {
        update <- cbind(pkgs, lib)
        colnames(update) <- c("Package", "LibPath")
        found <- pkgs %in% foundpkgs[, 1]
        files <- foundpkgs[match(pkgs[found], foundpkgs[, 1]), 2]
        update <- cbind(update[found, , drop=FALSE], file = files)
        if(nrow(update) > 1) {
            upkgs <- unique(pkgs <- update[, 1])
            DL <- .make_dependency_list(upkgs, available)
            p0 <- .find_install_order(upkgs, DL)
            ## can't use update[p0, ] due to possible multiple matches
            update <- update[sort.list(match(pkgs, p0)), ]
        }
        cmd0 <- paste(file.path(R.home(),"bin","R"), "CMD INSTALL")
        if (installWithVers)
            cmd0 <- paste(cmd0, "--with-package-versions")
        for(i in 1:nrow(update)) {
            cmd <- paste(cmd0, "-l", shQuote(update[i, 2]), update[i, 3])
            status <- system(cmd)
            if(status > 0)
                warning("Installation of package ", sQuote(update[i, 1]),
                        "had non-zero exit status")
        }
        if(!is.null(tmpd) && is.null(destdir))
            cat("\n", gettext("The downloaded packages are in "),
                tmpd, "\n", sep = "")
    } else if(!is.null(tmpd) && is.null(destdir)) unlink(tmpd, TRUE)

    invisible()
}


download.packages <- function(pkgs, destdir, available = NULL,
                              repos = CRAN,
                              contriburl = contrib.url(repos, type),
                              CRAN = getOption("CRAN"),
                              method, type)
{
    dirTest <- function(x) !is.na(isdir <- file.info(x)$isdir) & isdir

    nonlocalcran <- length(grep("^file:", contriburl)) < length(contriburl)
    if(nonlocalcran && !dirTest(destdir))
        stop("destdir is not a directory")
    if(is.null(available))
        available <- available.packages(contriburl=contriburl, method=method)

    retval <- NULL
    for(p in unique(pkgs))
    {
        ok <- (available[,"Package"] == p) | (available[,"Bundle"] == p)
        ok <- ok & !is.na(ok)
        if(!any(ok))
            warning("No package ", sQuote(p), " at the repositories")
        else {
            if(sum(ok) > 1) { # have multiple copies
                vers <- package_version(available[ok, "Version"])
                keep <- vers == max(vers)
                keep[duplicated(keep)] <- FALSE
                ok[ok][!keep] <- FALSE
            }
            fn <- paste(p, "_", available[ok, "Version"], ".tar.gz", sep="")
            repos <- available[ok, "Repository"]
            if(length(grep("^file:", repos)) > 0) { # local repository
                fn <- paste(substring(repos, 6), fn, sep = "/")
                retval <- rbind(retval, c(p, fn))
            } else {
                url <- paste(repos, fn, sep="/")
                destfile <- file.path(destdir, fn)

                if(download.file(url, destfile, method) == 0)
                    retval <- rbind(retval, c(p, destfile))
                else
                    warning("download of package", sQuote(p), "failed")
            }
        }
    }

    retval
}

contrib.url <- function(repos, type = c("source", "mac.binary"))
{
    type <- if(missing(type)) "source" else match.arg(type)
    switch(type,
           source = paste(gsub("/$", "", repos), "/src/contrib", sep = ""),
           mac.binary = paste(gsub("/$", "", repos), "/bin/macosx/",
           version$major, ".", substr(version$minor, 1, 1), sep = "")
           )
}
