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
                retval[[x]] <-
                    if(is.na(retval[[x]])) y
                    else paste(retval[[x]], y, sep="\n")
        }
        retval
    }

    if(missing(text))
        text <- scan(file=file, what="",  quote="", sep="\n", quiet=TRUE)

    if(length(text) == 0) {
        warning("zero length `text'")
        return(list())
    }
        
    ## remove empty lines
    notok <- grep("^[ \t]+$", text)
    if (length(notok) > 0)
        text <- text[-notok]

    ## use the field name of the first line as record separator
    recsep <- sub("^([^:]*):.*$", "\\1", text[1])

    start <- grep(paste("^", recsep, ":", sep=""), text)
    start <- c(start, length(text)+1)
    retval <- list()
    for(k in seq(length = length(start)-1)) {
        retval[[k]] <- parse.dcf.entry(text[start[k]:(start[k+1]-1)],
                                       fields = fields,
                                       versionfix = versionfix)
    }
   if(!is.null(fields))
        return( t(sapply(retval, unlist)) )
    if(length(retval) == 1)
        return( unlist(retval, recursive=FALSE) )
    ## else
    retval
}



