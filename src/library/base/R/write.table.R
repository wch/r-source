write.table <-
function (x, file = "", append = FALSE, quote = TRUE, sep = " ",
    eol = "\n", na = "NA", dec = ".", row.names = TRUE,
    col.names = TRUE, qmethod = c("escape", "double"))
{
    qmethod <- match.arg(qmethod)

    if(!is.data.frame(x))
	x <- data.frame(x)

    nocols <- NCOL(x)==0
    if (nocols) quote <- FALSE

    if(is.logical(quote) && quote)
	quote <- which(unlist(lapply(x, function(x)
                                     is.character(x) || is.factor(x))))
    if(dec != ".") {
        ## only need to consider numeric non-integer columns
    	num <- which(unlist(lapply(x, is.double)))
	if(length(num))
           x[num] <- lapply(x[num],
                            function(z) gsub("\\.", ",", as.character(z)))
    }
    x <- as.matrix(x)
    if (!nocols){
        i <- is.na(x)
        if(any(i))
            x[i] <- na
    }
    p <- ncol(x)
    d <- dimnames(x)

    if(is.logical(quote))
	quote <- if(quote) 1 : p else NULL
    else if(is.numeric(quote)) {
	if(any(quote < 1 | quote > p))
	    stop(paste("invalid numbers in", sQuote("quote")))
    }
    else
	stop(paste("invalid", sQuote("quote"), "specification"))

    rn <- FALSE
    if(is.logical(row.names)) {
	if(row.names) {
	    x <- cbind(d[[1]], x)
            rn <- TRUE
        }
    }
    else {
	row.names <- as.character(row.names)
	if(length(row.names) == nrow(x))
	    x <- cbind(row.names, x)
	else
	    stop(paste("invalid", sQuote("row.names"),
                       "specification"))
    }
    if(!is.null(quote) && (p < ncol(x)))
	quote <- c(0, quote) + 1

    if(is.logical(col.names))
        col.names <- if(is.na(col.names) && rn) c("", d[[2]])
        else if(col.names) d[[2]] else NULL
    else {
	col.names <- as.character(col.names)
	if(length(col.names) != p)
	    stop(paste("invalid", sQuote("col.names"), "specification"))
    }

    if(file == "")
        file <- stdout()
    else if(is.character(file)) {
        file <- file(file, ifelse(append, "a", "w"))
        on.exit(close(file))
    }
    if(!inherits(file, "connection"))
        stop(paste("argument", sQuote("file"),
                   "must be a character string or connection"))

    qstring <-                          # quoted embedded quote string
        switch(qmethod,
               "escape" = '\\\\"',
               "double" = '""')
    if(!is.null(col.names)) {
	if(append)
	    warning("appending column names to file")
	if(!is.null(quote))
	    col.names <- paste("\"", gsub('"', qstring, col.names),
                               "\"", sep = "")
        writeLines(paste(col.names, collapse = sep), file, sep = eol)
    }

    if (NROW(x) == 0)
        return(invisible(x))

    for(i in quote)
	x[, i] <- paste('"', gsub('"', qstring, as.character(x[, i])),
                        '"', sep = "")
    if (ncol(x))
        writeLines(paste(c(t(x)), c(rep.int(sep, ncol(x) - 1), eol),
                         sep = "", collapse = ""),
                   file, sep = "")
    else
        cat(eol,file=file)
}
