getenv <- function(x) {
    if (missing(x)) {
	x <- strsplit(.Internal(getenv(character())), "=")
	v <- n <- character(LEN <- length(x))
	for (i in 1:LEN) {
	    n[i] <- x[[i]][1]
	    v[i] <- paste(x[[i]][-1], collapse = "=")
	}
	structure(v, names = n)
    } else {
	structure(.Internal(getenv(x)), names = x)
    }
}

help.start <- function (gui = "irrelevant", browser = .Options$browser,
			remote = NULL) {
    if(is.null(browser))
	stop("Invalid browser name, check options(\"browser\").")
    url <- paste(if (is.null(remote)) "$HOME/.R" else remote,
		 "/doc/html/index.html", sep = "")
    cat("If", browser, " is already running,\tit is *not* restarted,\n",
	"and you must switch to its window.\nOtherwise, be patient..\n")
    system(paste("${R_HOME}/bin/help.links",
		 paste(unique(.lib.loc), sep=" ", collapse=" "),
		 sep =" "))
    system(paste(browser, " -remote \"openURL(", url, ")\" 2>/dev/null || ",
		 browser, " ", url, " &", sep = ""))
    options(htmlhelp=TRUE)
}

system <- function(command, intern = FALSE, ignore.stderr = FALSE)
    .Internal(system(if(ignore.stderr) paste(command, "2>/dev/null") else
                     command, intern))

unix <- function(call, intern = FALSE) {
    .Deprecated("system"); system(call,intern)
}

##--- The following 2  should/could really be done in C [platform !] :
tempfile <- function(pattern = "file") {
    system(paste("for p in", paste(pattern, collapse = " "), ";",
		 "do echo /tmp/$p$$; done"),
	   intern = TRUE)
}

unlink <- function(x) { system(paste("rm -rf ", paste(x, collapse = " "))) }

zip.file.extract <- function(file, zipname="R.zip")
{
    ## somewhat system-specific.
    unzip <- options()$unzip
    if(!length(unzip)) return(file)
    path <- sub("[^/]*$","", file)
    topic <- substr(file, nchar(path)+1, 1000)
    if(file.exists(file.path(path, zipname))) {
        tempdir <- sub("[^/]*$", "", tempfile())
        if(!system(paste(unzip, "-o",
                         file.path(path, zipname), topic, "-d", tempdir,
                         " > /dev/null")))
            file <- paste(tempdir,  topic, sep="")
    }
    file
}


### the following functions are currently only tested on unix, maybe
### we get them for more platforms soon.

parse.description <- function(desc)
{
    ## remove empty lines
    ok <- grep("^[ \t]+$", desc)
    if(length(ok)>0){
        desc <- desc[!ok]
    }

    ## put continuation lines into single fields\
    ## remove leading whitespace
    lastok <- 1
    for(k in 1:length(desc)){
        if(length(grep("^[ \t]+", desc[k])) > 0){
            desc[lastok] <- paste(desc[lastok],
                                  sub("^[ \t]+", "", desc[k]),
                                  sep="\n")
            desc[k] <- NA
        }
        else
        {
            lastok <- k
        }
    }
    desc <- desc[!is.na(desc)]

    retval <- list(Package=NA, Version=NA)

    ## all before the first `:' is the field name, the rest is the
    ## value of the field. For Version make sure that only the number
    ## gets extracted (some people put dates or something else on the
    ## same line).

    for(d in desc){
        x <- sub("^([^:]*):.*$", "\\1", d)
        y <- sub("^[^:]*:[ \t]*(.*)$", "\\1", d)
        if(x=="Version")
            y <- unlist(strsplit(y, " "))[1]
        retval[[x]] <- y
    }
    retval
}



installed.packages <- function(lib.loc = .lib.loc)
{
    retval <- NULL
    for(lib in lib.loc)
    {
        pkgs <- .packages(all.available=TRUE, lib.loc = lib)
        for(p in pkgs){
            descfile <- system.file("DESCRIPTION", pkg=p, lib=lib)
            if(descfile != ""){
                desc <- scan("", file=descfile, sep="\n", quiet=TRUE)
                desc <- parse.description(desc)
            }
            else
            {
                desc <- list(Version=NA)
            }

            retval <- rbind(retval, c(p, lib, desc$Version))

        }
    }
    colnames(retval) <- c("Package", "LibPath", "Version")
    retval
}



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
    alldesc <- scan("", file=tmpf, sep="\n", quiet=TRUE)
    if(!localcran)
        unlink(tmpf)

    pkgstart <- c(grep("^Package:", alldesc), length(alldesc)+1)
    retval <- NULL
    for(k in 1:(length(pkgstart)-1)){
        desc <- parse.description(alldesc[pkgstart[k]:(pkgstart[k+1]-1)])
        retval <- rbind(retval, c(desc$Package, desc$Version))
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
        ok <- cranp[,"Package"] == instp[k, "Package"]
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
    tmpd <- tempfile("dir")
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
        else if(system("lynx --help > /dev/null")==0)
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

dev2bitmap <- function(file, type="png256", height=6, width=6, res=72,
                   pointsize, ...)
{
    gsexe <- getenv("R_GSCMD")
    if(is.null(gsexe) || nchar(gsexe) == 0) {
        gsexe <- "gs"
        rc <- system(paste(gsexe, "-help > /dev/null"))
        if(rc != 0) stop("Sorry, gs cannot be found")
    }
    gshelp <- system(paste(gsexe, "-help"), intern=TRUE)
    st <- grep("^Available", gshelp)
    en <- grep("^Search", gshelp)
    gsdevs <- gshelp[(st+1):(en-1)]
    devs <- c(strsplit(gsdevs, " "), recursive=TRUE)
    if(match(type, devs, 0) == 0)
        stop(paste(paste("Device ", type, "is not available"),
                   "Available devices are",
                   paste(gsdevs, collapse="\n"), sep="\n"))
    if(missing(pointsize)) pointsize <- 1.5*min(width, height)
    tmp <- tempfile("Rbit")
    on.exit(unlink(tmp))
    dev.print(device=postscript, file=tmp, width=width, height=height,
              pointsize=pointsize, paper="special", horizontal=FALSE, ...)
    cmd <- paste(gsexe, " -dNOPAUSE -dBATCH -q -sDEVICE=", type,
                 " -r", res,
                 " -g", ceiling(res*width), "x", ceiling(res*height),
                 " -sOutputFile=", file, " ", tmp, sep="")
    system(cmd)
    invisible()
}
