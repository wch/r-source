install.packages <- function(pkgs, lib, CRAN=.Options$CRAN,
                             contriburl=contrib.url(CRAN),
                             method="auto", available=NULL)
{
    if(missing(lib) || is.null(lib)) {
        lib <- .lib.loc[1]
        warning(paste("argument `lib' is missing: using", lib))
    }
    localcran <- length(grep("^file:", contriburl)) > 0
    if(!localcran) {
        tmpd <- tempfile("Rinstdir")
        dir.create(tmpd)
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
                    cmd <- paste(file.path(R.home(),"bin","R"),
                                 "INSTALL -l", lib,
                                 foundpkgs[okp, 2])
                    system(cmd)
                }
            }
        }
        cat("\n")
        if(!localcran){
            answer <- substr(readline("Delete downloaded files (y/N)? "), 1, 1)
            if(answer == "y" | answer == "Y")
                unlink(tmpd)
            else
                cat("The packages are in", tmpd)
            cat("\n")
        }
    }
    else
        unlink(tmpd)
    invisible()
}


download.file <- function(url, destfile, method="auto")
{
    method <- match.arg(method,
                        c("auto", "wget", "lynx", "cp"))

    if(method == "auto") {
        if(length(grep("^file:", url)))
            method <- "cp"
        else if(system("wget --help > /dev/null")==0)
            method <- "wget"
        else if(system("lynx -help > /dev/null")==0)
            method <- "lynx"
        else
            stop("No download method found")
    }

    if(method=="wget")
        status <- system(paste("wget '", url, "' -O", destfile, sep=""))
    else if(method=="lynx")
        status <- system(paste("lynx -dump '", url, "' >", destfile, sep=""))
    else if(method=="cp") {
        url <- sub("^file:","",url)
        status <- system(paste("cp", url, destfile))
        if(status !=0)
            status <- shell(paste("copy", url, destfile))
    }
    invisible(status)
}

download.packages <- function(pkgs, destdir, available=NULL,
                              CRAN=.Options$CRAN,
                              contriburl=contrib.url(CRAN),
                              method="auto")
{
    localcran <- length(grep("^file:", contriburl)) > 0
    if(is.null(available))
        available <- CRAN.packages(contriburl=contriburl, method=method)

    retval <- NULL
    for(p in unique(pkgs))
    {
        ok <- (available[,"Package"] == p) | (available[,"Bundle"] == p)
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

    retval
}

contrib.url <- function(CRAN) paste(CRAN,"/src/contrib",sep="")
