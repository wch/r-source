install.packages <- function(pkgs, lib, CRAN=getOption("CRAN"),
                             contriburl=contrib.url(CRAN),
                             method, available=NULL, destdir=NULL,
                             installWithVers=FALSE)
{
    unpackPkg <- function(file, lib, installWithVers=FALSE) {
        ## Create a temporary directory and unpack the zip to it
        ## then get the real package & version name, copying the
        ## dir over to the appropriate install dir.
        tmpDir <- tempfile()
        dir.create(tmpDir)
        cDir <- getwd()
        on.exit(setwd(cDir), add=TRUE)
        res <- zip.unpack(file, tmpDir)
        setwd(tmpDir)

        ## From original install.packages
        require(tools)
        pkgname <- sub("\\.zip$", "", basename(pkg))
        checkMD5sums(pkgname)

        ## back to changes
        ## Get the real package name, and if watned, the real version
        ## num, and use this to install the package
        desc <- read.dcf(file.path(pkgname,"DESCRIPTION"),
                         c("Package","Version"))
        if (installWithVers) {
            instPath <- file.path(lib,paste(desc[1,1],desc[1,2],sep="_"))
        }
        else instPath <- file.path(lib, desc[1,1])

        ## If the package is already installed w/ this
        ## instName, remove it.  If it isn't there, the unlink call will
        ## still return success.
        ret <- unlink(instPath,recursive=TRUE)
        if (ret == 0) {
            ## Move the new package to the install lib and
            ## remove our temp dir
            file.rename(pkgname, instPath)
        }
        else {
            ## Would prefer to make sure it is a "clean" install, but
            ## if the directory could not be removed for some reason,
            ## default to old behaviour where it would just copy all
            ## files on top of previous install.
            zip.unpack(file, lib)
        }
        setwd(cDir)
        unlink(tmpDir, recursive=TRUE)
    }

    if(!length(pkgs)) return(invisible())
    if(missing(lib) || is.null(lib)) {
        lib <- .libPaths()[1]
        if(length(.libPaths()) > 1)
            warning(paste("argument `lib' is missing: using", lib))
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
    }
    if(is.null(CRAN) & missing(contriburl)) {
        for(i in seq(along=pkgs)) {
            unpackPkg(pkgs[i], lib, installWithVers)
        }
        link.html.help(verbose=TRUE)
        return(invisible())
    }
    localcran <- length(grep("^file:", contriburl)) > 0
    if(!localcran) {
        if (is.null(destdir)){
            tmpd <- tempfile("Rinstdir")
            dir.create(tmpd)
        } else tmpd <- destdir
    }

    foundpkgs <- download.packages(pkgs, destdir=tmpd,
                                   available=available,
                                   contriburl=contriburl, method=method)

    if(!is.null(foundpkgs))
    {
        update <- cbind(pkgs, lib)
        colnames(update) <- c("Package", "LibPath")
        for(lib in unique(update[,"LibPath"]))
        {
            oklib <- lib==update[,"LibPath"]
            for(p in update[oklib, "Package"])
            {
                okp <- p == foundpkgs[, 1]
                if(length(okp) > 0){
                    for(pkg in foundpkgs[okp, 2]) {
                        ## changes begin - JG
                        unpackPkg(pkg, lib, installWithVers)
                    }
                }
            }
        }
        cat("\n")
        if(!localcran && is.null(destdir)){
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
    }
    else
        unlink(tmpd)
    invisible()
}


download.packages <- function(pkgs, destdir, available=NULL,
                              CRAN=getOption("CRAN"),
                              contriburl=contrib.url(CRAN),
                              method)
{
    localcran <- length(grep("^file:", contriburl)) > 0
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
            fn <- paste(p, ".zip", sep="")
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

contrib.url <- function(CRAN)
    file.path(CRAN, "bin", "windows", "contrib")
