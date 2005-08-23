###----- NOTE:	../man/Deprecated.Rd   must be synchronized with this!
###		--------------------
.Deprecated <- function(new, package=NULL) {
    msg <- gettextf("'%s' is deprecated.\n",
                    as.character(sys.call(sys.parent())[[1]]))
    if(!missing(new))
        msg <- c(msg, gettextf("Use '%s' instead.\n", new))
    if(!is.null(package))
        msg <- c(msg,
                 gettextf("See help(\"Deprecated\") and help(\"%s-deprecated\").", package))
    else msg <- c(msg, gettext("See help(\"Deprecated\")"))
    warning(paste(msg, collapse=""), call. = FALSE, domain = NA)
}

## consider keeping one (commented) entry here, for easier additions


## <entry>
## Deprecated in 2.2.0
write.table0 <-
function (x, file = "", append = FALSE, quote = TRUE, sep = " ",
          eol = "\n", na = "NA", dec = ".", row.names = TRUE,
          col.names = TRUE, qmethod = c("escape", "double"))
{
    .Deprecated("write.table")
    qmethod <- match.arg(qmethod)

    if(!is.data.frame(x) && !is.matrix(x)) x <- data.frame(x)

    if(is.data.frame(x)) {
        if(is.logical(quote) && quote)
            quote <- if(length(x)) which(unlist(lapply(x, function(x) is.character(x) || is.factor(x)))) else numeric(0)
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
        ## we may have gained some columns here, as embedded matrices/dfs
        ## are split up into columns.
        d <- dimnames(x)
    } else { # a matrix
        if(is.logical(quote) && quote)
            quote <- if(is.character(x)) seq(length=p) else numeric(0)
        if(dec != "." && typeof(x) %in% c("double", "complex"))
            x[] <- gsub("\\.", dec, as.character(x))
        ## fix up dimnames as as.data.frame would
        p <- ncol(x)
        d <- dimnames(x)
        if(is.null(d)) d <- list(NULL, NULL)
        if(is.null(d[[1]])) d[[1]] <- seq(length=nrow(x))
        if(is.null(d[[2]]) && p > 0) d[[2]] <-  paste("V", 1:p, sep="")
    }
    ## from this point on we have a matrix, possibly even a matrix list.

    p <- ncol(x)
    nocols <- NCOL(x)==0
    if (!nocols){
        i <- is.na(x)
        if(any(i))
            x[i] <- na
    }

    if(is.logical(quote)) # should be false
	quote <- if(quote) 1 : p else NULL
    else if(is.numeric(quote)) {
	if(any(quote < 1 | quote > p))
	    stop("invalid numbers in 'quote'")
    } else
	stop("invalid 'quote' specification")

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
	    stop("invalid 'row.names' specification")
    }
    if(!is.null(quote) && rn) # quote the row names
	quote <- c(0, quote) + 1

    if(is.logical(col.names)) {
        if(!rn && is.na(col.names))
            stop("col.names = NA makes no sense when row.names = FALSE")
        col.names <- if(is.na(col.names) && rn) c("", d[[2]])
        else if(col.names) d[[2]] else NULL
    } else {
	col.names <- as.character(col.names)
	if(length(col.names) != p)
	    stop("invalid 'col.names' specification")
    }

    if(file == "")
        file <- stdout()
    else if(is.character(file)) {
        file <- file(file, ifelse(append, "a", "w"))
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
## </entry>

## <entry>
format.char <- function(x, width = NULL, flag = "-")
{
    .Deprecated("format.default")
    ## Character formatting, flag: if "-" LEFT-justify
    if (is.null(x)) return("")
    if(!is.character(x)) {
	warning("format.char: coercing 'x' to 'character'")
	x <- as.character(x)
    }
    if(is.null(width) && flag == "-")
	return(format(x))		# Left justified; width= max.width

    at <- attributes(x)
    nc <- nchar(x, type="w")	       	#-- string widths
    nc[is.na(nc)] <- 2
    if(is.null(width)) width <- max(nc)
    else if(width<0) { flag <- "-"; width <- -width }
    ##- 0.90.1 and earlier:
    ##- pad <- sapply(pmax(0,width - nc),
    ##-			function(no) paste(character(no+1), collapse =" "))
    ## Speedup by Jens Oehlschlaegel:
    tab <- unique(no <- pmax(0, width - nc))
    tabpad <- sapply(tab+1, function(n) paste(character(n), collapse = " "))
    pad <- tabpad[match(no, tab)]

    r <-
	if(flag=="-")	paste(x, pad, sep="")#-- LEFT  justified
	else		paste(pad, x, sep="")#-- RIGHT justified
    if(!is.null(at))
	attributes(r) <- at
    r
}
## </entry>
