#  File src/library/utils/R/write.table.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2012 The R Core Team
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  https://www.R-project.org/Licenses/

write.table <-
function (x, file = "", append = FALSE, quote = TRUE, sep = " ",
          eol = "\n", na = "NA", dec = ".", row.names = TRUE,
          col.names = TRUE, qmethod = c("escape", "double"),
          fileEncoding = "")
{
    qmethod <- match.arg(qmethod)
    if(is.logical(quote) && (length(quote) != 1L || is.na(quote)))
        stop("'quote' must be 'TRUE', 'FALSE' or numeric")
    ## quote column names unless quote == FALSE (see help).
    quoteC <- if(is.logical(quote)) quote else TRUE
    qset <- is.logical(quote) && quote

    if(!is.data.frame(x) && !is.matrix(x)) x <- data.frame(x)

    makeRownames <- isTRUE(row.names)
    ## need col names if col.names is TRUE or NA
    makeColnames <- is.logical(col.names) && !identical(FALSE, col.names)
    if(is.matrix(x)) {
        ## fix up dimnames as as.data.frame would
        p <- ncol(x)
        d <- dimnames(x)
        if(is.null(d)) d <- list(NULL, NULL)
        if(is.null(d[[1L]]) && makeRownames) d[[1L]] <- seq_len(nrow(x))
        if(is.null(d[[2L]]) && makeColnames && p > 0L)
            d[[2L]] <- paste0("V", 1L:p)
        if(qset)
            quote <- if(is.character(x)) seq_len(p) else numeric()
    } else { ## data.frame
        if(qset)
            quote <- if(length(x))
                which(unlist(lapply(x, function(x)
                                    is.character(x) || is.factor(x))))
            else numeric()
        ## fix up embedded matrix columns into separate cols:
        if(any(sapply(x, function(z) length(dim(z)) == 2 && dim(z)[2L] > 1))) {
            c1 <- names(x)
	    x <- as.matrix(x, rownames.force = makeRownames)
	    d <- dimnames(x)
	    if(qset) {
		ord <- match(c1, d[[2L]], 0L)
		quote <- ord[quote]; quote <- quote[quote > 0L]
	    }
        }
        else
            d <- list(if(makeRownames) row.names(x),
                      if(makeColnames) names(x))
        p <- ncol(x)
    }
    nocols <- p == 0L

    if(is.logical(quote)) # must be false
	quote <- NULL
    else if(is.numeric(quote)) {
	if(any(quote < 1L | quote > p))
	    stop("invalid numbers in 'quote'")
    } else
	stop("invalid 'quote' specification")

    rn <- FALSE
    rnames <- NULL
    if(is.logical(row.names)) {
	if(row.names) {rnames <- as.character(d[[1L]]); rn <- TRUE}
    } else {
	rnames <- as.character(row.names)
        rn <- TRUE
	if(length(rnames) != nrow(x))
            stop("invalid 'row.names' specification")
    }
    if(!is.null(quote) && rn) # quote the row names
	quote <- c(0, quote)

    if(is.logical(col.names)) {
        if(!rn && is.na(col.names))
            stop("'col.names = NA' makes no sense when 'row.names = FALSE'")
        col.names <- if(is.na(col.names) && rn) c("", d[[2L]])
        else if(col.names) d[[2L]] else NULL
    } else {
	col.names <- as.character(col.names)
	if(length(col.names) != p)
	    stop("invalid 'col.names' specification")
    }

    if(file == "") file <- stdout()
    else if(is.character(file)) {
        file <- if(nzchar(fileEncoding))
            file(file, ifelse(append, "a", "w"), encoding = fileEncoding)
            else file(file, ifelse(append, "a", "w"))
        on.exit(close(file))
    } else if(!isOpen(file, "w")) {
        open(file, "w")
        on.exit(close(file))
    }
    if(!inherits(file, "connection"))
        stop("'file' must be a character string or connection")

    qstring <-                          # quoted embedded quote string
        switch(qmethod,
               "escape" = '\\\\"',
               "double" = '""')
    if(!is.null(col.names)) {
	if(append)
	    warning("appending column names to file")
	if(quoteC)
	    col.names <- paste("\"", gsub('"', qstring, col.names),
                               "\"", sep = "")
        writeLines(paste(col.names, collapse = sep), file, sep = eol)
    }

    if (nrow(x) == 0L) return(invisible())
    if (nocols && !rn) return(cat(rep.int(eol, NROW(x)), file=file, sep=""))

    ## convert list matrices to character - maybe not much use?
    if(is.matrix(x) && !is.atomic(x)) mode(x) <- "character"
    if(is.data.frame(x)) {
        ## convert columns we can't handle in C code
        x[] <- lapply(x, function(z) {
            if(is.object(z) && !is.factor(z)) as.character(z) else z
        })
    }

    invisible(.External2(C_writetable, x, file, nrow(x), p, rnames, sep, eol,
                         na, dec, as.integer(quote), qmethod != "double"))
}

write.csv <- function(...)
{
    Call <- match.call(expand.dots = TRUE)
    for(argname in c("append", "col.names", "sep", "dec", "qmethod"))
        if(!is.null(Call[[argname]]))
            warning(gettextf("attempt to set '%s' ignored", argname),
                    domain = NA)
    rn <- eval.parent(Call$row.names)
    Call$append <- NULL
    Call$col.names <- if(is.logical(rn) && !rn) TRUE else NA
    Call$sep <- ","
    Call$dec <- "."
    Call$qmethod <- "double"
    Call[[1L]] <- as.name("write.table")
    eval.parent(Call)
}

write.csv2 <- function(...)
{
    Call <- match.call(expand.dots = TRUE)
    for(argname in c("append", "col.names", "sep", "dec", "qmethod"))
        if(!is.null(Call[[argname]]))
            warning(gettextf("attempt to set '%s' ignored", argname),
                    domain = NA)
    rn <- eval.parent(Call$row.names)
    Call$append <- NULL
    Call$col.names <- if(is.logical(rn) && !rn) TRUE else NA
    Call$sep <- ";"
    Call$dec <- ","
    Call$qmethod <- "double"
    Call[[1L]] <- as.name("write.table")
    eval.parent(Call)
}
