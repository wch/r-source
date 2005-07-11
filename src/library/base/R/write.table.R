write.table <-
function (x, file = "", append = FALSE, quote = TRUE, sep = " ",
          eol = "\n", na = "NA", dec = ".", row.names = TRUE,
          col.names = TRUE, qmethod = c("escape", "double"))
{
    qmethod <- match.arg(qmethod)

    if(!is.data.frame(x) && !is.matrix(x)) x <- data.frame(x)

    if(is.matrix(x)) {
        ## fix up dimnames as as.data.frame would
        p <- ncol(x)
        d <- dimnames(x)
        if(is.null(d)) d <- list(NULL, NULL)
        if(is.null(d[[1]])) d[[1]] <- seq(length=nrow(x))
        if(is.null(d[[2]]) && p > 0) d[[2]] <-  paste("V", 1:p, sep="")
        if(is.logical(quote) && quote)
            quote <- if(is.character(x)) seq(length=p) else numeric(0)
    } else {
        qset <- FALSE
        if(is.logical(quote) && quote) {
            quote <- if(length(x)) which(unlist(lapply(x, function(x) is.character(x) || is.factor(x)))) else numeric(0)
            qset <- TRUE
        }
        ## fix up embedded matrix columns into separate cols
        ismat <- sapply(x, function(z) length(dim(z)) == 2 &&  dim(z)[2] > 1)
        if(any(ismat)) {
            c1 <- names(x)
            x <- as.matrix(x)
            if(qset) {
                c2 <- colnames(x)
                ord <- match(c1, c2, 0)
                quote <- ord[quote]; quote <- quote[quote > 0]
            }
        }
        d <- dimnames(x)
        p <- ncol(x)
    }
    nocols <- p==0

    if(is.logical(quote)) # must be false
	quote <- NULL
    else if(is.numeric(quote)) {
	if(any(quote < 1 | quote > p))
	    stop("invalid numbers in 'quote'")
    } else
	stop("invalid 'quote' specification")

    rn <- FALSE
    rnames <- NULL
    if(is.logical(row.names)) {
	if(row.names) {rnames <- as.character(d[[1]]); rn <- TRUE}
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

    if (nrow(x) == 0) return(invisible())
    if (nocols && !rn) return(cat(rep.int(eol, NROW(x)), file=file, sep=""))

    ## convert list matrices to character - maybe not much use?
    if(is.matrix(x) && !is.atomic(x)) mode(x) <- "character"
    if(is.data.frame(x)) {
        ## convert columns we can't handle in C code
        x[] <- lapply(x, function(z) {
            if(is.object(z) && !is.factor(z)) as.character(z) else z
        })
    }

    .Internal(write.table(x, file, nrow(x), p, rnames, sep, eol, na, dec,
                          as.integer(quote), qmethod != "double"))
}

write.csv <- function(...)
{
    Call <- match.call(expand.dots = TRUE)
    for(argname in c("col.names", "sep", "dec", "qmethod"))
        if(!is.null(Call[[argname]]))
            warning(gettextf("attempt to change '%s' ignored", argname),
                    domain = NA)
    rn <- Call$row.names
    Call$col.names <- if(is.logical(rn) && !rn) TRUE else NA
    Call$sep <- ","
    Call$dec <- "."
    Call$qmethod <- "double"
    Call[[1]] <- as.name("write.table")
    eval.parent(Call)
}

write.csv2 <- function(...)
{
    Call <- match.call(expand.dots = TRUE)
    for(argname in c("col.names", "sep", "dec", "qmethod"))
        if(!is.null(Call[[argname]]))
            warning(gettextf("attempt to change '%s' ignored", argname),
                    domain = NA)
    rn <- Call$row.names
    Call$col.names <- if(is.logical(rn) && !rn) TRUE else NA
    Call$sep <- ";"
    Call$dec <- ","
    Call$qmethod <- "double"
    Call[[1]] <- as.name("write.table")
    eval.parent(Call)
}
