CRAN.packages <- function(CRAN=getOption("CRAN"), method,
                          contriburl=contrib.url(CRAN))
{
    localcran <- length(grep("^file:", contriburl)) > 0
    if(localcran)
        tmpf <- paste(substring(contriburl,6), "PACKAGES", sep="/")
    else{
        tmpf <- tempfile()
        on.exit(unlink(tmpf))
        download.file(url=paste(contriburl, "PACKAGES", sep="/"),
                      destfile=tmpf, method=method, cacheOK=FALSE)
    }
    read.dcf(file=tmpf, fields=c("Package", "Version",
                       "Priority", "Bundle", "Depends"))
}

update.packages <- function(lib.loc=NULL, CRAN=getOption("CRAN"),
                            contriburl=contrib.url(CRAN),
                            method, instlib=NULL, ask=TRUE,
                            available=NULL, destdir=NULL)
{
    if(is.null(lib.loc))
        lib.loc <- .libPaths()
   
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
                         available=available, destdir=destdir)
    }
}

old.packages <- function(lib.loc=NULL, CRAN=getOption("CRAN"),
                         contriburl=contrib.url(CRAN),
                         method, available=NULL)
{
    if(is.null(lib.loc))
        lib.loc <- .libPaths()
    
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

    newerVersion <- function(a, b){
        a <- as.integer(strsplit(a, "[\\.-]")[[1]])
        b <- as.integer(strsplit(b, "[\\.-]")[[1]])
        if(any(is.na(a)))
            return(FALSE)
        if(any(is.na(b)))
            return(TRUE)
        for(k in 1:length(a)){
            if(k <= length(b)){
                if(a[k]>b[k])
                    return(TRUE)
                else if(a[k]<b[k])
                    return(FALSE)
            }
            else{
                return(TRUE)
            }
        }
        return(FALSE)
    }

    for(k in 1:nrow(instp)){
        ok <- (instp[k, "Priority"] != "base") &
                (available[,"Package"] == instp[k, "Package"])
        if(any(ok))
            ok[ok] <- sapply(available[ok, "Version"], newerVersion,
                             instp[k, "Version"])
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

package.contents <- function(pkg, lib.loc=NULL)
{
    if(is.null(lib.loc))
        lib.loc <- .libPaths()
    
    file <- system.file("CONTENTS", package = pkg, lib.loc = lib.loc)
    if(file == "") {
        warning(paste("Cannot find CONTENTS file of package", pkg))
        return(NA)
    }

    read.dcf(file=file, fields=c("Entry", "Keywords", "Description"))
}


package.description <- function(pkg, lib.loc=NULL, fields=NULL)
{
    if(is.null(lib.loc))
        lib.loc <- .libPaths()

    file <- system.file("DESCRIPTION", package = pkg, lib.loc = lib.loc)
    if(file != "") {
        retval <- read.dcf(file=file, fields=fields)[1,]
    }

    if((file == "") || (length(retval) == 0)){
        warning(paste("DESCRIPTION file of package", pkg,
                      "missing or broken"))
        if(!is.null(fields)){
            retval <- rep(NA, length(fields))
            names(retval) <- fields
        }
        else
            retval <- NA
    }

    retval
}


installed.packages <- function(lib.loc = NULL)
{
    if(is.null(lib.loc))
        lib.loc <- .libPaths()

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

package.dependencies <- function(x, check = FALSE)
{
    if(!is.matrix(x))
        x <- matrix(x, nrow = 1, dimnames = list(NULL, names(x)))

    deps <- list()
    for(k in 1:nrow(x)){
        z <- x[k, "Depends"]
        if(!is.na(z) & z != ""){
            ## split dependencies, remove leading and trailing whitespace
            z <- unlist(strsplit(z, ","))
            z <- sub("^[[:space:]]*(.*)", "\\1", z)
            z <- sub("(.*)[[:space:]]*$", "\\1", z)

            ## split into package names and version
            pat <- "^([^\\([:space:]]+)[[:space:]]*\\(([^\\)]+)\\).*"
            deps[[k]] <-
                cbind(sub(pat, "\\1", z), sub(pat, "\\2", z), NA)

            noversion <- deps[[k]][,1] == deps[[k]][,2]
            deps[[k]][noversion,2] <- NA

            ## split version dependency into operator and version number
            pat <- "[[:space:]]*([[<>=]+)[[:space:]]+(.*)"
            deps[[k]][!noversion, 2:3] <-
                c(sub(pat, "\\1", deps[[k]][!noversion, 2]),
                  sub(pat, "\\2", deps[[k]][!noversion, 2]))
        }
        else
            deps[[k]] <- NA
    }

    if(check){
        z <- rep(TRUE, nrow(x))
        for(k in 1:nrow(x)){
            ## currently we only check the version of R itself
            if(!is.na(deps[[k]]) &&
               any(ok <- deps[[k]][,1] == "R")) {
                ## NOTE: currently operators must be `<=' or `>='.
                if(!is.na(deps[[k]][ok, 2])
                   && deps[[k]][ok, 2] %in% c("<=", ">=")) {
                    comptext <-
                        paste('"', R.version$major, ".",
                              R.version$minor, '" ',
                              deps[[k]][ok,2], ' "',
                              deps[[k]][ok,3], '"', sep = "")
                    compres <- try(eval(parse(text = comptext)))
                    if(!inherits(compres, "try-error"))
                        z[k] <- compres
                }
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

remove.packages <- function(pkgs, lib) {

    updateIndices <- function(lib) {
        ## This should eventually be made public, as it could also be
        ## used by install.packages() && friends.
        if(lib == .Library) {
            ## R version of
            ##   ${R_HOME}/bin/build-help --htmllists
            ##   cat ${R_HOME}/library/*/CONTENTS \
            ##     > ${R_HOME}/doc/html/search/index.txt
            if(exists("link.html.help", mode = "function"))
                link.html.help()
        }
    }

    if(missing(lib) || is.null(lib)) {
        lib <- .libPaths()[1]
        warning(paste("argument `lib' is missing: using", lib))
    }

    paths <- .find.package(pkgs, lib)
    unlink(paths, TRUE)
    for(lib in unique(dirname(paths)))
        updateIndices(lib)
}
