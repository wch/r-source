CRAN.packages <- function(CRAN=.Options$CRAN, method="auto")
{
    localcran <- length(grep("^file:", CRAN)) > 0
    if(localcran)
        tmpf <- file.path(substring(CRAN,6),
                          "src", "contrib", "PACKAGES")
    else{
        tmpf <- tempfile()
        download.file(url=paste(CRAN, "/src/contrib/PACKAGES", sep=""),
                      destfile=tmpf, method=method)
    }
    parse.dcf(file=tmpf, fields=c("Package", "Version", "Priority"),
              versionfix=TRUE)
}


update.packages <- function(lib.loc=.lib.loc, CRAN=.Options$CRAN,
                            method="auto", instlib=NULL)
{
    instp <- installed.packages(lib.loc=lib.loc)
    cranp <- CRAN.packages(CRAN=CRAN, method=method)

    update <- NULL
    for(k in 1:nrow(instp)){
        ok <- (instp[k, "Priority"] != "base") &
              (cranp[,"Package"] == instp[k, "Package"])
        if(any(cranp[ok, "Version"] > instp[k, "Version"]))
        {
            cat(instp[k, "Package"], ":\n",
                "Version", instp[k, "Version"],
                "in", instp[k, "LibPath"], "\n",
                "Version", cranp[ok, "Version"], "on CRAN")
            cat("\n")
            answer <- substr(readline("Update Package (y/N)?  "), 1, 1)
            if(answer == "y" | answer == "Y")
                update <- rbind(update, instp[k, c("Package", "LibPath")])
            cat("\n")
        }
    }

    if(!is.null(update)){
        if(is.null(instlib))
            instlib <-  update[,"LibPath"]

        install.packages(update[,"Package"], instlib, CRAN=CRAN,
                         method=method, available=cranp)
    }
}


install.packages <- function(pkglist, lib, CRAN=.Options$CRAN,
                             method="auto", available=NULL)
{
    localcran <- length(grep("^file:", CRAN)) > 0
    pkgs <- NULL
    if(missing(lib) || is.null(lib)) {
        lib <- .lib.loc[1]
        warning(paste("argument `lib' is missing: using", lib))
    }
    tmpd <- tempfile("Rinstdir")
    system(paste("mkdir", tmpd))
    pkgs <- download.packages(pkglist, destdir=tmpd,
                              available=available,
                              CRAN=CRAN, method=method)
    update <- cbind(pkglist, lib)
    colnames(update) <- c("Package", "LibPath")

    if(!is.null(pkgs))
    {
        for(lib in unique(update[,"LibPath"]))
        {
            oklib <- lib==update[,"LibPath"]
            for(p in update[oklib,"Package"])
            {
                okp <- p==pkgs[, 1]
                if(length(okp)>0){
                    cmd <- paste(file.path(R.home(),"bin","R"),
                                 "INSTALL -l", lib,
                                 pkgs[okp, 2])
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
}


download.file <- function(url, destfile, method="auto")
{
    method <- match.arg(method,
                        c("auto", "wget", "lynx", "cp"))

    if(method=="auto"){
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
        status <- system(paste("wget", url, "-O", destfile))
    else if(method=="lynx")
        status <- system(paste("lynx -dump", url, ">", destfile))
    else if(method=="cp"){
        url <- sub("^file:","",url)
        status <- system(paste("cp", url, destfile))
    }
    invisible(status)
}



download.packages <- function(pkgs, destdir, available=NULL,
                              CRAN=.Options$CRAN, method="auto")
{
    localcran <- length(grep("^file:", CRAN)) > 0
    if(is.null(available))
        available <- CRAN.packages(CRAN=CRAN, method=method)

    retval <- NULL
    for(p in unique(pkgs))
    {
        ok <- available[,"Package"] == p
        fn <- paste(p, "_", available[ok, "Version"], ".tar.gz", sep="")
        if(localcran){
            fn <- file.path(substring(CRAN, 6), "src", "contrib", fn)
            retval <- rbind(retval, c(p, fn))
        }
        else{
            url <- paste(CRAN, "src/contrib", fn, sep="/")
            destfile <- file.path(destdir, fn)

            if(download.file(url, destfile, method) == 0)
                retval <- rbind(retval, c(p, destfile))
            else
                warning(paste("Download of package", p, "failed"))
        }
    }

    retval
}
