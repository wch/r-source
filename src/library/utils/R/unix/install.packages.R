install.packages <- function(pkgs, lib, CRAN = getOption("CRAN"),
                             contriburl = contrib.url(CRAN),
                             method, available = NULL, destdir = NULL,
                             installWithVers = FALSE, dependencies = FALSE)
{
    if(missing(lib) || is.null(lib)) {
        lib <- .libPaths()[1]
        if(length(.libPaths()) > 1)
            warning(paste("argument", sQuote("lib"),
                          "is missing: using", lib))
    }
    oneLib <- length(lib) == 1
    localcran <- length(grep("^file:", contriburl)) > 0
    if(!localcran) {
        if (is.null(destdir)) {
            tmpd <- tempfile("Rinstdir")
            if (!dir.create(tmpd))
                stop('Unable to create temp directory ', tmpd)
        } else tmpd <- destdir
    }

    if(dependencies && !oneLib) {
        warning("Don't know which element of 'lib' to install dependencies into\n", "skipping dependencies")
        dependencies <- FALSE
    }
    if(is.null(available))
        available <- CRAN.packages(contriburl = contriburl, method = method)
    if(dependencies) { # check for dependencies, recursively
        p0 <- p1 <- unique(pkgs) # this is ok, as 1 lib only
        have <- .packages(all.available = TRUE)
        repeat {
            if(any(miss <- ! p1 %in% row.names(available))) {
                cat("dependencies ", paste(sQuote(p1[miss]), sep=", "),
                    " are not available\n\n", sep ="")
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
        bundles <- .find_bundles(available)
        for(bundle in names(bundles))
            pkgs[ pkgs %in% bundles[[bundle]] ] <- bundle
        pkgs <- unique(pkgs)
        pkgs <- pkgs[pkgs %in% row.names(available)]
        if(length(pkgs) > length(p0)) {
            added <- setdiff(pkgs, p0)
            cat("also installing the dependencies ",
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
            cmd <- paste(cmd0, "-l", update[i, 2], update[i, 3])
            status <- system(cmd)
            if(status > 0)
                warning(paste("Installation of package", update[i, 1],
                              "had non-zero exit status"))
        }
        cat("\n")
        if(!localcran && is.null(destdir)) {
            answer <- substr(readline("Delete downloaded files (y/N)? "), 1, 1)
            if(answer == "y" | answer == "Y")
                unlink(tmpd, TRUE)
            else
                cat("The packages are in", tmpd)
            cat("\n")
        }
    } else unlink(tmpd, TRUE)

    invisible()
}


download.packages <- function(pkgs, destdir, available = NULL,
                              CRAN = getOption("CRAN"),
                              contriburl = contrib.url(CRAN),
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
            fn <- paste(p, "_", available[ok, "Version"], ".tar.gz", sep="")[1]
            if(localcran){
                fn <- paste(substring(contriburl, 6), fn, sep="/")
                retval <- rbind(retval, c(p, fn))
            }
            else{
                url <- paste(available[ok, "Repository"], fn, sep="/")
                destfile <- file.path(destdir, fn)

                if(download.file(url, destfile, method) == 0)
                    retval <- rbind(retval, c(p, destfile))
                else
                    warning(paste("Download of package", p, "failed"))
            }
        }
    }

    retval
}

contrib.url <- function(CRAN, type=c("source","mac.binary"))
{
  type <- match.arg(type)
  switch(type,
         source = paste(gsub("/$", "", CRAN), "/src/contrib", sep=""),
         mac.binary = paste(gsub("/$", "", CRAN), "/bin/macosx/",
         version$major, ".", substr(version$minor,1,1), sep="")
         )
}
