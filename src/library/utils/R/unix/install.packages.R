install.packages <- function(pkgs, lib, CRAN=getOption("CRAN"),
                             contriburl=contrib.url(CRAN),
                             method, available=NULL, destdir=NULL,
                             installWithVers=FALSE, dependencies=FALSE)
{
    if(missing(lib) || is.null(lib)) {
        lib <- .libPaths()[1]
        if(length(.libPaths()) > 1)
            warning(paste("argument", sQuote("lib"),
                          "is missing: using", lib))
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
                if(length(okp) > 0){
                    cmd <- paste(file.path(R.home(),"bin","R"),
				 "CMD INSTALL")
		    if (installWithVers)
			cmd <- paste(cmd,"--with-package-versions")

		    cmd <- paste(cmd,"-l",lib,foundpkgs[okp, 2])
                    status <- system(cmd)
                    if(status > 0){
                        warning(paste("Installation of package",
                                      foundpkgs[okp, 1],
                                      "had non-zero exit status"))
                    }
                }
            }
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
    }
    else
        unlink(tmpd, TRUE)
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
            fn <- paste(p, "_", available[ok, "Version"], ".tar.gz", sep="")
            if(localcran){
                fn <- paste(substring(contriburl, 6), fn, sep="/")
                retval <- rbind(retval, c(p, fn))
            }
            else{
                url <- paste(contriburl, fn, sep="/")
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

contrib.url <- function(CRAN, type=c("source","mac.binary")){
  type<-match.arg(type)
  switch(type,
         source=paste(CRAN,"/src/contrib",sep=""),
         mac.binary=paste(CRAN,"/bin/macosx/",version$major, ".", substr(version$minor,1,1),sep="")
         )
}
