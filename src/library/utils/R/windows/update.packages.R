install.packages <- function(pkgs, lib, CRAN=getOption("CRAN"),
                             contriburl=contrib.url(CRAN),
                             method, available=NULL, destdir=NULL,
                             installWithVers=FALSE, dependencies=FALSE)
{
    unpackPkg <- function(pkg, pkgname, lib, installWithVers=FALSE)
    {
        ## the `spammy' (his phrase) comments are from Gentry
        ## However, at least some of his many errors have been removed

        ## Create a temporary directory and unpack the zip to it
        ## then get the real package & version name, copying the
        ## dir over to the appropriate install dir.
        tmpDir <- tempfile(, lib)
        if (!dir.create(tmpDir)) stop('Unable to create temp directory ',tmpDir)
        cDir <- getwd()
        on.exit(setwd(cDir), add = TRUE)
        res <- zip.unpack(pkg, tmpDir)
        setwd(tmpDir)
        res <- tools::checkMD5sums(pkgname, file.path(tmpDir, pkgname))
        if(!is.na(res) && res) {
            cat("package", sQuote(pkgname),
                "successfully unpacked and MD5 sums checked\n")
            flush.console()
        }

        ## Check to see if this is a bundle or a single package
        if (file.exists("DESCRIPTION")) {
            ## Bundle
            conts <- read.dcf("DESCRIPTION",fields="Contains")[1,]
            if (is.na(conts))
                stop("Malformed bundle DESCRIPTION file, no Contains field")
            else
                pkgs <- strsplit(conts," ")[[1]]
            ## now check the MD5 sums
            res <- TRUE
            for (curPkg in pkgs) res <- res &
            tools::checkMD5sums(pkgname, file.path(tmpDir, curPkg))
            if(!is.na(res) && res) {
                cat("bundle", sQuote(pkgname),
                    "successfully unpacked and MD5 sums checked\n")
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
                file.rename(file.path(tmpDir, curPkg), instPath)
            } else {
                ## !! Can't revert to old 'zip.unpack' as it would
                ## !! potentially leave cruft from a bundle in there
                stop("Can not remove prior installation of package")
            }
        }
        setwd(cDir)
        unlink(tmpDir, recursive=TRUE)
    }

    if(!length(pkgs)) return(invisible())
    if(missing(lib) || is.null(lib)) {
        lib <- .libPaths()[1]
        if(length(.libPaths()) > 1)
            warning(paste("argument", sQuote("lib"),
                          "is missing: using", lib))
    }
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
    if(is.null(CRAN) & missing(contriburl)) {
        for(i in seq(along=pkgs))
            unpackPkg(pkgs[i], pkgnames[i], lib, installWithVers)
        link.html.help(verbose=TRUE)
        return(invisible())
    }
    localcran <- length(grep("^file:", contriburl)) > 0
    if(!localcran) {
        if (is.null(destdir)) {
            tmpd <- tempfile("Rinstdir")
            if (!dir.create(tmpd))
                stop('Unable to create temp directory ', tmpd)
        } else tmpd <- destdir
    }

    if(dependencies) { # go and look for dependencies, recursively
        pkgs0 <- pkgs
        l <- length(pkgs0)
        if(is.null(available))
            available <- CRAN.packages(contriburl=contriburl, method=method)
        have <- .packages(all.available = TRUE)
        repeat {
            ## what about bundles?
            deps <- available[match(pkgs0, available[, "Package"]), "Depends"]
            deps <- deps[!is.na(deps)]
            if(!length(deps)) break
            deps <- unlist(strsplit(deps, ","))
            deps <- unique(sub("^[[:space:]]*([[:alnum:].]+).*$", "\\1" , deps))
            toadd <- deps[! deps %in% c("R", have)]
            if(length(toadd) == 0) break
            pkgs <- c(pkgs, toadd)
            pkgs0 <- toadd
        }
        if(length(pkgs) > l) {
            added <- pkgs[-(1:l)]
            cat("also installing the dependencies ",
                paste(sQuote(added), collapse=", "), "\n\n", sep="")
            flush.console()
            pkgnames <- pkgs # not zips, now
        }
    }

    foundpkgs <- download.packages(pkgs, destdir=tmpd,
                                   available=available,
                                   contriburl=contriburl, method=method)

    if(!is.null(foundpkgs)) {
        update <- cbind(pkgs, lib)
        colnames(update) <- c("Package", "LibPath")
        for(lib in unique(update[,"LibPath"])) {
            oklib <- lib==update[,"LibPath"]
            for(p in update[oklib, "Package"])
            {
                okp <- p == foundpkgs[, 1]
                if(length(okp) > 0)
                    unpackPkg(foundpkgs[okp, 2], pkgnames[okp], lib,
                              installWithVers)
            }
        }
        cat("\n")
        if(!localcran && is.null(destdir)) {
            answer <- substr(readline("Delete downloaded files (y/N)? "), 1,
1)
            if(answer == "y" | answer == "Y") {
                for(file in foundpkgs[, 2]) unlink(file)
                unlink(tmpd)
            } else
                cat("The packages are in", tmpd)
            cat("\n")
        }
        link.html.help(verbose=TRUE)
    } else unlink(tmpd)
    invisible()
}


download.packages <- function(pkgs, destdir, available=NULL,
                              CRAN=getOption("CRAN"),
                              contriburl=contrib.url(CRAN),
                              method)
{
    dirTest <- function(x) !is.na(isdir <- file.info(x)$isdir) & isdir

    localcran <- length(grep("^file:", contriburl)) > 0
    if(!localcran && !dirTest(destdir)) stop("destdir is not a directory")
    if(is.null(available))
        available <- CRAN.packages(contriburl=contriburl, method=method)

    retval <- NULL
    for(p in unique(pkgs))
    {
        ok <- (available[,"Package"] == p) | (available[,"Bundle"] == p)
        ok <- ok & !is.na(ok)
        if(!any(ok))
            warning(paste("No package \"", p, "\" on CRAN.", sep=""))
        else{
            fn <- paste(p, "_", available[ok, "Version"], ".zip", sep="")
            if(localcran){
                fn <- paste(substring(contriburl, 6), fn, sep="/")
                retval <- rbind(retval, c(p, fn))
            }
            else{
                url <- paste(contriburl, fn, sep="/")
                destfile <- file.path(destdir, fn)

                if(download.file(url, destfile, method, mode="wb") == 0)
                    retval <- rbind(retval, c(p, destfile))
                else
                    warning(paste("Download of package", p, "failed"))
            }
        }
    }

    retval
}

contrib.url <- function(CRAN) {
    ver <- paste(R.version$major, substring(R.version$minor,1,1), sep=".")
    file.path(CRAN, "bin", "windows", "contrib", ver)
}


### the following function supports install.packages()

zip.unpack <- function(zipname, dest)
{
    if(file.exists(zipname)) {
        if((unzip <- getOption("unzip")) != "internal") {
            system(paste(unzip, "-oq", zipname, "-d", dest),
                   show = FALSE, invisible = TRUE)
        } else {
            .Internal(int.unzip(zipname, NULL, dest))
        }
    } else stop(paste("zipfile", zipname, "not found"))
}

