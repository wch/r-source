write.table <-
function (x, file = "", append = FALSE, quote = TRUE, sep = " ",
          eol = "\n", na = "NA", dec = ".", row.names = TRUE,
          col.names = TRUE, qmethod = c("escape", "double"))
{
    qmethod <- match.arg(qmethod)

    if(!is.data.frame(x) && !is.matrix(x)) x <- data.frame(x)

    nocols <- NCOL(x)==0
    if (nocols) quote <- FALSE

    p <- ncol(x)
    d <- dimnames(x)

    if(is.data.frame(x)) {
        if(is.logical(quote) && quote)
            quote <- which(unlist(lapply(x, function(x)
                                         is.character(x) || is.factor(x))))
        if(dec != ".") {
            ## only need to consider numeric non-integer columns
            num <- which(unlist(lapply(x, function(x) is.double(x)
                                       || is.complex(x))))
            if(length(num))
                x[num] <- lapply(x[num],
                                 function(z) gsub("\\.", dec, as.character(z)))
        }
        ## as.matrix might turn integer or numeric columns into a complex matrix
        cmplx <- sapply(x, is.complex)
        if(length(cmplx) && any(cmplx) && !all(cmplx))
            x[cmplx] <- lapply(x[cmplx], as.character)
        x <- as.matrix(x)
    } else { # a matrix
        if(is.logical(quote) && quote)
            quote <- if(is.character(x)) seq(length=p) else numeric(0)
        if(dec != "." && typeof(x) %in% c("double", "complex"))
            x[] <- gsub("\\.", dec, as.character(x))
        ## fix up dimnames as as.data.frame would
        if(is.null(d)) d <- list(NULL, NULL)
        if(is.null(d[[1]])) d[[1]] <- seq(length=nrow(x))
        if(is.null(d[[2]]) && p > 0) d[[2]] <-  paste("V", 1:p, sep="")
    }

    if (!nocols){
        i <- is.na(x)
        if(any(i))
            x[i] <- na
    }

    if(is.logical(quote)) # should be false
	quote <- if(quote) 1 : p else NULL
    else if(is.numeric(quote)) {
	if(any(quote < 1 | quote > p))
	    stop(paste("invalid numbers in", sQuote("quote")))
    } else
	stop(paste("invalid", sQuote("quote"), "specification"))

    rn <- FALSE
    if(is.logical(row.names)) {
	if(row.names) {
	    x <- cbind(d[[1]], x)
            rn <- TRUE
        }
    } else {
	rnames <- as.character(row.names)
        rn <- TRUE
	if(length(rnames) == nrow(x))
	    x <- cbind(rnames, x)
	else
	    stop(paste("invalid", sQuote("row.names"), "specification"))
    }
    if(!is.null(quote) && rn) # quote the row names
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
	if(length(quote))
	    col.names <- paste("\"", gsub('"', qstring, col.names),
                               "\"", sep = "")
        writeLines(paste(col.names, collapse = sep), file, sep = eol)
    }

    if (NROW(x) == 0) return(invisible())
    if (nocols && !rn) return(cat(rep.int(eol, NROW(x)), file=file, sep=""))

    for(i in quote)
	x[, i] <- paste('"', gsub('"', qstring, as.character(x[, i])),
                        '"', sep = "")
    writeLines(paste(c(t(x)), c(rep.int(sep, ncol(x) - 1), eol),
                     sep = "", collapse = ""),
               file, sep = "")
}

write.table2 <-
function (x, file = "", append = FALSE, quote = TRUE, sep = " ",
          eol = "\n", na = "NA", dec = ".", row.names = TRUE,
          col.names = TRUE, qmethod = c("escape", "double"))
{
    qmethod <- match.arg(qmethod)

    if(!is.data.frame(x) && !is.matrix(x)) x <- data.frame(x)
    p <- ncol(x)

    nocols <- p==0
    if (nocols) quote <- FALSE

    p <- ncol(x)
    d <- dimnames(x)
    if(is.matrix(x)) {
        ## fix up dimnames as as.data.frame would
        if(is.null(d)) d <- list(NULL, NULL)
        if(is.null(d[[1]])) d[[1]] <- seq(length=nrow(x))
        if(is.null(d[[2]]) && p > 0) d[[2]] <-  paste("V", 1:p, sep="")
    }

    if(is.logical(quote) && quote) {
        quote <- if(is.data.frame(x))
            which(unlist(lapply(x, function(x)
                                is.character(x) || is.factor(x))))
        else (if(is.character(x)) seq(length=p) else numeric(0))
    }

    if(is.logical(quote)) # must be false
	quote <- NULL
    else if(is.numeric(quote)) {
	if(any(quote < 1 | quote > p))
	    stop(paste("invalid numbers in", sQuote("quote")))
    } else
	stop(paste("invalid", sQuote("quote"), "specification"))

    rn <- FALSE
    rnames <- NULL
    if(is.logical(row.names)) {
	if(row.names) {rnames <- as.character(d[[1]]); rn <- TRUE}
    } else {
	rnames <- as.character(row.names)
        rn <- TRUE
	if(length(rnames) != nrow(x))
            stop(paste("invalid", sQuote("row.names"), "specification"))
    }
    if(!is.null(quote) && rn) # quote the row names
	quote <- c(0, quote)

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
	if(length(quote))
	    col.names <- paste("\"", gsub('"', qstring, col.names),
                               "\"", sep = "")
        writeLines(paste(col.names, collapse = sep), file, sep = eol)
    }

    if (nrow(x) == 0) return(invisible())
    if (nocols && !rn) return(cat(rep.int(eol, NROW(x)), file=file, sep=""))

    .Internal(write.table(x, file, nrow(x), p, rnames, sep, eol, na, dec,
                          as.integer(quote), qmethod != "double"))
}
