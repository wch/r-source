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
        text <- scan(file=file, what="",  quote="", sep="\n", quiet=TRUE)

    ## remove empty lines
    notok <- grep("^[ \t]+$", text)
    if (length(notok) > 0){
        text <- text[-notok]
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



