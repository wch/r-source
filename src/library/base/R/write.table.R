write.table <-
function (x, file = "", append = FALSE, quote = TRUE, sep = " ",
    eol = "\n", na = "NA", dec = ".", row.names = TRUE,
    col.names = TRUE, qmethod = c("escape", "double"))
{
    qmethod <- match.arg(qmethod)

    if(!is.data.frame(x))
	x <- data.frame(x)
    else if(is.logical(quote) && quote)
	quote <- which(unlist(lapply(x, function(x)
                                     is.character(x) || is.factor(x))))
    if(dec != ".") {
    	num <- which(unlist(lapply(x, is.numeric)))
	x[num] <- lapply(x[num],
                         function(z) gsub("\\.", ",", as.character(z)))
    }
    i <- is.na(x)
    x <- as.matrix(x)
    if(any(i))
        x[i] <- na
    p <- ncol(x)
    d <- dimnames(x)

    if(is.logical(quote))
	quote <- if(quote) 1 : p else NULL
    else if(is.numeric(quote)) {
	if(any(quote < 1 | quote > p))
	    stop("invalid numbers in `quote'")
    }
    else
	stop("invalid `quote' specification")

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
	    stop("invalid `row.names' specification")
    }
    if(!is.null(quote) && (p < ncol(x)))
	quote <- c(0, quote) + 1

    if(is.logical(col.names))
        col.names <- if(is.na(col.names) && rn) c("", d[[2]])
        else if(col.names) d[[2]] else NULL
    else {
	col.names <- as.character(col.names)
	if(length(col.names) != p)
	    stop("invalid `col.names' specification")
    }

    if(file == "")
        file <- stdout()
    else if(is.character(file)) {
        file <- file(file, ifelse(append, "a", "w"))
        on.exit(close(file))
    }
    if(!inherits(file, "connection"))
        stop("argument `file' must be a character string or connection")

    if(!is.null(col.names)) {
	if(append)
	    warning("appending column names to file")
	if(!is.null(quote))
	    col.names <- paste("\"", col.names, "\"", sep = "")
        writeLines(paste(col.names, collapse = sep), file, sep = eol)
    }

    qstring <-                          # quoted embedded quote string
        switch(qmethod,
               "escape" = '\\\\"',
               "double" = '""')
    for(i in quote)
	x[, i] <- paste('"', gsub('"', qstring, as.character(x[, i])),
                        '"', sep = "")

    writeLines(paste(c(t(x)), c(rep(sep, ncol(x) - 1), eol),
                     sep = "", collapse = ""),
               file, sep = "")
}
