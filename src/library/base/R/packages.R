CRAN.packages <- function(CRAN=getOption("CRAN"), method="auto",
                          contriburl=contrib.url(CRAN))
{
    localcran <- length(grep("^file:", contriburl)) > 0
    if(localcran)
        tmpf <- paste(substring(contriburl,6), "PACKAGES", sep="/")
    else{
        tmpf <- tempfile()
        on.exit(unlink(tmpf))
        download.file(url=paste(contriburl, "PACKAGES", sep="/"),
                      destfile=tmpf, method=method)
    }
    parse.dcf(file=tmpf, fields=c("Package", "Version",
                         "Priority", "Bundle", "Depends"),
              versionfix=TRUE)
}

update.packages <- function(lib.loc=.lib.loc, CRAN=getOption("CRAN"),
                            contriburl=contrib.url(CRAN),
                            method="auto", instlib=NULL, ask=TRUE,
                            available=NULL)
{
    if(is.null(available))
        available <- CRAN.packages(contriburl=contriburl, method=method)

    old <- old.packages(lib.loc=lib.loc,
                        contriburl=contriburl,
                        method=method,
                        available=available)

    update <- NULL
    if(ask & !is.null(old)){
        for(k in 1:nrow(old)){
            cat(old[k, "Package"], ":\n",
                "Version", old[k, "Installed"],
                "in", old[k, "LibPath"], "\n",
                "Version", old[k, "CRAN"], "on CRAN")
            cat("\n")
            answer <- substr(readline("Update (y/N)?  "), 1, 1)
            if(answer == "y" | answer == "Y")
                update <- rbind(update, old[k,])
        }
    }
    else
        update <- old


    if(!is.null(update)){
        if(is.null(instlib))
            instlib <-  update[,"LibPath"]

        install.packages(update[,"Package"], instlib,
                         contriburl=contriburl,
                         method=method,
                         available=available)
    }
}

old.packages <- function(lib.loc=.lib.loc, CRAN=getOption("CRAN"),
                         contriburl=contrib.url(CRAN),
                         method="auto", available=NULL)
{
    instp <- installed.packages(lib.loc=lib.loc)
    if(is.null(available))
        available <- CRAN.packages(contriburl=contriburl, method=method)

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
    ok <- !is.na(available[,"Bundle"])
    available[ok,"Package"] <- available[ok,"Bundle"]

    update <- NULL
    for(k in 1:nrow(instp)){
        ok <- (instp[k, "Priority"] != "base") &
                (available[,"Package"] == instp[k, "Package"])
        ok[ok] <- available[ok, "Version"] > instp[k, "Version"]
        if(any(ok) && any(package.dependencies(available[ok, ], check=TRUE)))
        {
            update <- rbind(update,
                            c(instp[k, c("Package", "LibPath", "Version")],
                              available[ok, "Version"]))
        }
    }
    if(!is.null(update))
        colnames(update) <- c("Package", "LibPath",
                              "Installed", "CRAN")
    update
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
                                        "Bundle", "Depends"))

            retval <- rbind(retval, c(p, lib, desc))
        }
    }
    if (!is.null(retval))
        colnames(retval) <- c("Package", "LibPath", "Version",
                              "Priority", "Bundle", "Depends")
    retval
}


package.dependencies <- function(x, check=FALSE)
{
    if(!is.matrix(x))
        x <- matrix(x, nrow=1, dimnames=list(NULL, names(x)))

    deps <- list()
    for(k in 1:nrow(x)){
        z <- x[k, "Depends"]
        if(!is.na(z) & z != ""){
            ## split dependencies, remove leading and trailing whitespace
            z <- unlist(strsplit(z, ","))
            z <- sub("^[[:space:]]*(.*)", "\\1", z)
            z <- sub("(.*)[[:space:]]*$", "\\1", z)

            ## split into package names and version
            deps[[k]] <- cbind(sub("^([^[:space:]\\(]*).*", "\\1", z),
                               sub(".*\\((.*)\\).*", "\\1", z), NA)

            noversion <- deps[[k]][,1] == deps[[k]][,2]
            deps[[k]][noversion,2] <- NA

            ## split version dependency into operator and version number
            pat <- "[[:space:]]*([[<>=]*)[[:space:]]*(.*)"
            deps[[k]][!noversion,2:3] <-
                c(sub(pat,"\\1", deps[[k]][!noversion,2]),
                  sub(pat,"\\2", deps[[k]][!noversion,2]))
        }
        else
            deps[[k]] <- NA
    }

    if(check){
        z <- rep(TRUE, nrow(x))
        for(k in 1:nrow(x)){
            ## currently we only check the version of R itself
            if(!is.na(deps[[k]]) &&
               any(ok <- deps[[k]][,1] == "R"))
            {
                if(!is.na(deps[[k]][ok,2])){
                    comptext <-
                        paste('"', R.version$major, ".",
                              R.version$minor, '" ',
                              deps[[k]][ok,2], ' "',
                              deps[[k]][ok,3], '"', sep="")
                }
                z[k] <- eval(parse(text=comptext))
            }
        }
        names(z) <- x[,"Package"]
        return(z)
    }
    else{
        names(deps) <- x[,"Package"]
        return(deps)
    }
}


