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

help.start <- function(gui = "irrelevant", browser = "irrelevant")
{
    a <- system.file("index.html", pkg="doc/html", lib=R.home())
    if (a == "")
        a <- system.file("index.htm", pkg="doc/html", lib=R.home())
    if (a == "")
        stop("I can't find the html help\n")
    else {
        a <- gsub("/", "\\\\", a)
        cat("If nothing happens, you have to open `",a,"' yourself\n")
        .Internal(help.start());
    }
    invisible("")
}

system <- function(command, intern = FALSE, wait = TRUE, input = "",
                   show.output.on.console = FALSE, minimized = FALSE,
                   invisible = FALSE)
{
    f <- ""
    if (input!="") {
        f <- tempfile()
        on.exit(unlink(f))
        cat(input,file=f,sep="\n")
    }
    if (intern)
        flag <- 3
    else {
        if  (wait)
            flag <- ifelse(show.output.on.console, 2, 1)
        else
            flag <- 0
    }
    if (invisible) flag <- 20 + flag
    else if (minimized) flag <- 10 + flag
    .Internal(system(command, as.integer(flag), f))
}

unix <- function(call, intern = FALSE)
{
    .Deprecated("system")
    system(call,intern)
}

tempfile <- function(pattern = "file") .Internal(tempfile(pattern))

unlink <- function(x) invisible(.Internal(unlink(x)))

flush.console <- function() .Internal(flush.console())

shell <- function(cmd, shell, flag="/c", intern=FALSE,
                  wait=TRUE, translate=FALSE, mustWork=FALSE)
{
    if(missing(shell)) {
        shell <- getenv("R_SHELL")
        if(!nchar(shell)) shell <- getenv("SHELL")
        if(!nchar(shell)) shell <- getenv("COMSPEC")
    }
    if(missing(flag) && any(!is.na(match(c("bash", "tcsh"), shell))))
        flag <- "-c"
    if(translate) cmd <- gsub("/", "\\\\", cmd)
    if(!is.null(shell)) cmd <- paste(shell, flag, cmd)
    res <- system(cmd, intern=intern, wait=wait | intern,
                  show.output.on.console=wait)
    if(!intern && res !=0)
        if(mustWork)
            if(res == -1) stop("cmd could not be run")
            else stop(paste("cmd execution failed with error code", res))
        else
            if(res == -1) warning("cmd could not be run")
            else warning(paste("cmd execution failed with error code", res))
    if(intern) res else invisible(res)
}

zip.file.extract <- function(file, zipname="R.zip")
{
    ofile <- gsub("\\\\", "/", file)
    path <- sub("[^/]*$","", ofile)
    topic <- substr(ofile, nchar(path)+1, 1000)
    if(file.exists(file.path(path, zipname))) {
        tempdir <- sub("[^\\]*$","", tempfile())
        if((unzip <- options()$unzip) != "internal") {
            if(!system(paste(unzip, "-oq",
                             file.path(path, zipname), topic,
                             "-d", tempdir), show = FALSE, invisible = TRUE))
                file <- paste(tempdir,  topic, sep="")
        } else {
            rc <- .Internal(int.unzip(file.path(path, zipname), topic, tempdir))
            if (rc == 10)
                warning(paste(R.home(),
                              "unzip\\unzip32.dll cannot be loaded", sep="\\"))
            if (rc == 0)
                file <- paste(tempdir, topic, sep="")
        }
    }
    file
}

### the following functions support update.packages()

zip.unpack <- function(zipname, dest)
{
    if(file.exists(zipname)) {
        if((unzip <- options()$unzip) != "internal") {
            system(paste(unzip, "-oq", zipname, "-d", dest),
                   show = FALSE, invisible = TRUE)
        } else {
            rc <- .Internal(int.unzip(zipname, NULL, dest))
            if (rc == 10)
                warning(paste(R.home(),
                              "unzip\\unzip32.dll cannot be loaded", sep="\\"))
            rc
        }
    } else stop(paste("zipfile", zipname, "not found"))
}

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
        else if(system("wget --help", invisible=TRUE)==0)
            method <- "wget"
        else if(shell("lynx --help", invisible=TRUE)==0)
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
dev2bitmap <- function(file, type="png256", height=6, width=6, res=72,
                   pointsize, ...)
{
    gsexe <- getenv("R_GSCMD")
    if(is.null(gsexe) || nchar(gsexe) == 0) gsexe <- "gswin32c.exe"
    gshelp <- system(paste(gsexe, "-help"), intern=TRUE, invisible=TRUE)
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
    system(cmd, invisible=TRUE)
    invisible()
}
