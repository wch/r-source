parse.dcf <- function(text=NULL, file="", fields=NULL, versionfix=FALSE)
{

    parse.dcf.entry <- function(text, fields=NULL, versionfix=FALSE)
    {
        contlines <- grep("^[ \t]+", text)
        
        if(is.null(fields)){
            if(length(contlines))
                fields <- sub("^([^:]*):.*$", "\\1", text[-contlines])
            else
                fields <- sub("^([^:]*):.*$", "\\1", text)
        }
        
        retval <- as.list(rep(NA, length(fields)))
        names(retval) <- fields
        
        for(d in 1:length(text)){
            if(any(contlines == d))
                y <- sub("^[ \t]+(.*)$", "\\1", text[d])
            else{
                x <- sub("^([^:]*):.*$", "\\1", text[d])
                y <- sub("^[^:]*:[ \t]*(.*)$", "\\1", text[d])
            }
            
            if(versionfix & x=="Version")
                y <- unlist(strsplit(y, " "))[1]
            
            if(any(fields==x))
                if(is.na(retval[[x]]))
                    retval[[x]] <- y
                else
                    retval[[x]] <- paste(retval[[x]], y, sep="\n")
        }
        retval
    }

    if(missing(text))
        text <- scan(file=file, what="",  sep="\n", quiet=TRUE)
    
    ## remove empty lines
    ok <- grep("^[ \t]+$", text)
    if(length(ok)>0){
        text <- text[!ok]
    }

    ## use the field name of the first line as record separator
    recsep <- sub("^([^:]*):.*$", "\\1", text[1])

    start <- grep(paste("^", recsep, ":", sep=""), text)
    start <- c(start, length(text)+1)
    retval <- list()
    for(k in 1:(length(start)-1)){
        retval[[k]] <- parse.dcf.entry(text[start[k]:(start[k+1]-1)],
                                               fields=fields,
                                               versionfix=versionfix)
    }

    if(!is.null(fields))
        retval <- t(sapply(retval, unlist))
    else if(length(retval)==1)
        retval <- unlist(retval, recursive=FALSE)

    retval
}


package.contents <- function(pkg, lib=.lib.loc){
    
    contentsfile <- system.file("CONTENTS", pkg=pkg, lib=lib)
    if(contentsfile == "")
        stop("Cannot find CONTENTS file")
    
    contents <- scan("", file=contentsfile, sep="\n", quiet=TRUE)
    parse.dcf(contents, fields=c("Entry", "Keywords", "Description"))
}

package.description <- function(pkg, lib=.lib.loc){
    
    contentsfile <- system.file("DESCRIPTION", pkg=pkg, lib=lib)
    if(contentsfile == "")
        stop("Cannot find DESCRIPTION file")

    contents <- scan("", file=contentsfile, sep="\n", quiet=TRUE)
    parse.dcf(contents, versionfix=TRUE)
}

installed.packages <- function(lib.loc = .lib.loc)
{
    retval <- NULL
    for(lib in lib.loc)
    {
        pkgs <- .packages(all.available=TRUE, lib.loc = lib)
        for(p in pkgs){
            descfile <- system.file("DESCRIPTION", pkg=p, lib=lib)
            if(descfile != "")
                desc <- parse.dcf(file=descfile, versionfix=TRUE,
                                  fields=c("Version", "Priority"))
            else
                desc <- c(NA, NA)
            
            retval <- rbind(retval, c(p, lib, desc))
        }
    }
    colnames(retval) <- c("Package", "LibPath", "Version", "Priority")
    retval
}

