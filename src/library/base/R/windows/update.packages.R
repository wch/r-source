CRAN.packages <- function(CRAN=.Options$CRAN, method="auto")
{
    localcran <- length(grep("^file:", CRAN)) > 0
    if(localcran)
        tmpf <- file.path(substring(CRAN,6),
                          "src", "contrib", "PACKAGES")
    else{
        tmpf <- tempfile()
        download.file(url=paste(CRAN,
                      "/bin/windows/windows-NT/contrib/README", sep=""),
                      destfile=tmpf, method=method)
    }
    alldesc <- scan("", file=tmpf, sep="\n", quiet=TRUE)
    if(!localcran)
        unlink(tmpf)

    pkgstart <- c(grep("^.+\\.zip", alldesc), length(alldesc)+1)
    retval <- NULL
    for(k in 1:(length(pkgstart)-1)){
        line <- alldesc[pkgstart[k]]
        Package <- sub("\\.zip.*", "", line)
        Version <- sub("^.+\\.zip[ \t]*", "", line)
        Version <- sub("[ \t]+.*$", "", Version)
        retval <- rbind(retval, c(Package, Version))
    }
    colnames(retval) <- c("Package", "Version")
    retval
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
    if(missing(lib) || is.null(lib)) {
        lib <- .lib.loc[1]
        warning(paste("argument `lib' is missing: using", lib))
    }
    if(is.null(CRAN)) {
        for(pkg in pkglist) zip.unpack(pkg, lib)
        link.html.help()
        return(invisible())
    }
    localcran <- length(grep("^file:", CRAN)) > 0
    pkgs <- NULL
    tmpd <- tempfile("Rinstdir")
    shell(paste("mkdir", tmpd))
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
                for(pkg in pkgs[okp, 2]) zip.unpack(pkg, lib)
            }
        }
        cat("\n")
        if(!localcran){
            answer <- substr(readline("Delete downloaded files (y/N)? "), 1, 1)
            if(answer == "y" | answer == "Y") {
                for(file in pkgs[, 2]) unlink(file)
                unlink(tmpd)
            } else
                cat("The packages are in", tmpd)
            cat("\n")
        }
        link.html.help()
    }
    else
        unlink(tmpd)
    invisible()
}


download.file <- function(url, destfile, method="auto")
{
    method <- match.arg(method,
                        c("auto", "wget", "lynx", "cp"))

   if(method=="auto"){
        if(length(grep("^file:", url)))
            method <- "cp"
        else if(system("wget --help", invisible=TRUE)==0)
            method <- "wget"
        else if(shell("lynx -help", invisible=TRUE)==0)
            method <- "lynx"
        else
            stop("No download method found")
    }

    if(method=="wget")
        status <- system(paste("wget", url, "-O", destfile))
    else if(method=="lynx")
        status <- shell(paste("lynx -dump", url, ">", destfile))

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
        fn <- paste(p, ".zip", sep="")
        url <- paste(CRAN, "bin/windows/windows-NT/contrib", fn, sep="/")
        destfile <- file.path(destdir, fn)
        if(download.file(url, destfile, method) == 0)
            retval <- rbind(retval, c(p, destfile))
        else
            warning(paste("Download of package", p, "failed"))
    }

    retval
}
