CRAN.packages <- function(CRAN=.Options$CRAN, method="auto",
                          contriburl=contrib.url(CRAN))
{
    localcran <- length(grep("^file:", contriburl)) > 0
    if(localcran)
        tmpf <- paste(substring(contriburl,6), "PACKAGES", sep="/")
    else{
        tmpf <- tempfile()
        download.file(url=paste(contriburl, "PACKAGES", sep="/"),
                      destfile=tmpf, method=method)
        on.exit(unlink(tmpf))
    }
    parse.dcf(file=tmpf, fields=c("Package", "Version",
                         "Priority", "Bundle"),
              versionfix=TRUE)
}

update.packages <- function(lib.loc=.lib.loc, CRAN=.Options$CRAN,
                            contriburl=contrib.url(CRAN),
                            method="auto", instlib=NULL)
{
    instp <- installed.packages(lib.loc=lib.loc)
    cranp <- CRAN.packages(contriburl=contriburl, method=method)

    ## for bundles it is sufficient to install the first package
    ## contained in the bundle, as this will install the complete bundle
    for(b in unique(instp[,"Bundle"])){
        if(!is.na(b)){
            ok <- which(instp[,"Bundle"] == b)
            if(length(ok)>1){
                instp <- instp[-ok[-1],]
            }
        }
    }

    ## for packages contained in bundles use bundle names from now on
    ok <- !is.na(instp[,"Bundle"])
    instp[ok,"Package"] <- instp[ok,"Bundle"]
    ok <- !is.na(cranp[,"Bundle"])
    cranp[ok,"Package"] <- cranp[ok,"Bundle"]

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
            answer <- substr(readline("Update (y/N)?  "), 1, 1)
            if(answer == "y" | answer == "Y")
                update <- rbind(update, instp[k, c("Package", "LibPath")])
            cat("\n")
        }
    }

    if(!is.null(update)){
        if(is.null(instlib))
            instlib <-  update[,"LibPath"]

        install.packages(update[,"Package"], instlib,
                         contriburl=contriburl,
                         method=method, available=cranp)
    }
}


package.contents <- function(pkg, lib=.lib.loc){

    file <- system.file("CONTENTS", pkg=pkg, lib=lib)
    if(file == ""){
        warning(paste("Cannot find CONTENTS file of package", pkg))
        return(NA)
    }

    contents <- scan("", file=file, sep="\n", quiet=TRUE)
    parse.dcf(contents, fields=c("Entry", "Keywords", "Description"))
}


package.description <- function(pkg, lib=.lib.loc, fields=NULL){

    file <- system.file("DESCRIPTION", pkg=pkg, lib=lib)
    if(file == ""){
        warning(paste("Cannot find DESCRIPTION file of package", pkg))
        if(!is.null(fields)){
            retval <- rep(NA, length(fields))
            names(retval) <- fields
        }
        else
            retval <- NA

        return(retval)
    }

    contents <- scan("", file=file, sep="\n", quiet=TRUE)
    parse.dcf(contents, fields=fields, versionfix=TRUE)
}


installed.packages <- function(lib.loc = .lib.loc)
{
    retval <- NULL
    for(lib in lib.loc)
    {
        pkgs <- .packages(all.available=TRUE, lib.loc = lib)
        for(p in pkgs){
            desc <- package.description(p, lib=lib,
                                        fields=c("Version", "Priority",
                                        "Bundle"))

            retval <- rbind(retval, c(p, lib, desc))
        }
    }
    colnames(retval) <- c("Package", "LibPath", "Version", "Priority", "Bundle")
    retval
}
