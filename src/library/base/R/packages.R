compareVersion <- function(a, b){
    if(is.na(a))
        return(-1)
    if(is.na(b))
        return(1)
    a <- as.integer(strsplit(a, "[\\.-]")[[1]])
    b <- as.integer(strsplit(b, "[\\.-]")[[1]])
    for(k in 1:length(a)){
        if(k <= length(b)){
            if(a[k]>b[k])
                return(1)
            else if(a[k]<b[k])
                return(-1)
        }
        else{
            return(1)
        }
    }
    if(length(b)>length(a))
        return(-1)
    else
        return(0)
}

package.dependencies <- function(x, check = FALSE,
                                 depLevel=c("Depends", "Suggests"))
{
    depLevel <- match.arg(depLevel)

    if(!is.matrix(x))
        x <- matrix(x, nrow = 1, dimnames = list(NULL, names(x)))

    deps <- list()
    for(k in 1:nrow(x)){
        z <- x[k, depLevel]
        if(!is.na(z) & z != ""){
            ## split dependencies, remove leading and trailing whitespace
            z <- unlist(strsplit(z, ",", fixed=TRUE))
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
        z <- rep.int(TRUE, nrow(x))
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

packageDescription <- function(pkg, lib.loc=NULL, fields=NULL, drop=TRUE)
{ 
    retval <- list()
    if(!is.null(fields)){
        fields <- as.character(fields)
        retval[fields] <- NA
    }
    
    file <- system.file("DESCRIPTION", package = pkg, lib.loc = lib.loc)
    
    if(file != "") {
        desc <- as.list(read.dcf(file=file)[1,])
        if(!is.null(fields)){
            ok <- names(desc) %in% fields
            retval[names(desc)[ok]] <- desc[ok]
        }
        else
            retval[names(desc)] <- desc
    }

    if((file == "") || (length(retval) == 0)){
        warning(paste("DESCRIPTION file of package ", sQuote(pkg),
                      " missing or broken\n"))
        return(NA)
    }

    if(drop & length(fields)==1)
        return(retval[[1]])
    
    class(retval) <- "packageDescription"
    if(!is.null(fields)) attr(retval, "fields") <- fields
    attr(retval, "file") <- file
    retval
}


print.packageDescription <- function(x, ...)
{
    write.dcf(as.data.frame.list(x))
    cat("-- File:", attr(x, "file"), "\n")
    if(!is.null(attr(x, "fields"))){
        cat("-- Fields read: ")
        cat(attr(x, "fields"), sep=", ")
        cat("\n")
    }
}
