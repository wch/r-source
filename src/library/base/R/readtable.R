count.fields <- function(file, sep = "", quote = "\"'", skip = 0,
                         blank.lines.skip = TRUE, comment.char = "#")
{
    if(is.character(file)) {
        file <- file(file)
        on.exit(close(file))
    }
    if(!inherits(file, "connection"))
        stop("argument `file' must be a character string or connection")
    .Internal(count.fields(file, sep, quote, skip, blank.lines.skip,
                           comment.char))
}


type.convert <- function(x, na.strings = "NA", as.is = FALSE, dec = ".")
    .Internal(type.convert(x, na.strings, as.is, dec))


read.table <-
    function (file, header = FALSE, sep = "", quote = "\"'", dec = ".",
              row.names, col.names, as.is = FALSE,
	      na.strings = "NA", colClasses = NA,
              nrows = -1, skip = 0,
              check.names = TRUE, fill = !blank.lines.skip,
              strip.white = FALSE, blank.lines.skip = TRUE,
              comment.char = "#")
{
    if(is.character(file)) {
        file <- file(file, "r")
        on.exit(close(file))
    }
    if(!inherits(file, "connection"))
        stop("argument `file' must be a character string or connection")
    if(!isOpen(file)) {
        open(file, "r")
        on.exit(close(file))
    }

    if(skip > 0) readLines(file, skip)
    ## read a few lines to determine header, no of cols.
#    nlines <- 0
#    lines <- NULL
#     while(nlines < 5) {
#         ## read up to five non-blank and non-comment lines.
#         line <- readLines(file, 1, ok = TRUE)
#         if(length(line) == 0) break
#         if(blank.lines.skip && length(grep("^[ \\t]*$", line))) next
#         if(length(comment.char) && nchar(comment.char)) {
#             pattern <- paste("^[ \\t]*", substring(comment.char,1,1),
#                              sep ="")
#             if(length(grep(pattern, line))) next
#         }
#         lines <- c(lines, line)
#     }

    nlines <- if (nrows < 0) 5 else min(5, (header + nrows))

    lines <- .Internal(readTableHead(file, nlines, comment.char,
                                     blank.lines.skip))
    nlines <- length(lines)
    if(!nlines) {
        if(missing(col.names))
            stop("no lines available in input")
        else {
            tmp <- vector("list", length(col.names))
            names(tmp) <- col.names
            class(tmp) <- "data.frame"
            return(tmp)
        }
    }
    if(all(nchar(lines) == 0)) stop("empty beginning of file")
    pushBack(c(lines, lines), file)
    first <- scan(file, what = "", sep = sep, quote = quote,
                  nlines = 1, quiet = TRUE, skip = 0,
                  strip.white = TRUE,
                  blank.lines.skip = blank.lines.skip,
                  comment.char = comment.char)
    col1 <- if(missing(col.names)) length(first) else length(col.names)
    col <- numeric(nlines - 1)
    if (nlines > 1)
        for (i in seq(along=col))
            col[i] <- length(scan(file, what = "", sep = sep,
                                  quote = quote,
                                  nlines = 1, quiet = TRUE, skip = 0,
                                  strip.white = strip.white,
                                  blank.lines.skip = blank.lines.skip,
                                  comment.char = comment.char))
    cols <- max(col1, col)

    ##	basic column counting and header determination;
    ##	rlabp (logical) := it looks like we have column names

    rlabp <- (cols - col1) == 1
    if(rlabp && missing(header))
	header <- TRUE
    if(!header) rlabp <- FALSE

    if (header) {
        readLines(file, 1) # skip over header
        if(missing(col.names)) col.names <- first
        else if(length(first) != length(col.names))
            warning("header and `col.names' are of different lengths")

    } else if (missing(col.names))
	col.names <- paste("V", 1:cols, sep = "")
    if(length(col.names) + rlabp < cols)
        stop("more columns than column names")
    if(fill && length(col.names) > cols)
        cols <- length(col.names)
    if(!fill && cols > 0 && length(col.names) > cols)
        stop("more column names than columns")
    if(cols == 0) stop("first five rows are empty: giving up")


    if(check.names) col.names <- make.names(col.names, unique = TRUE)
    if (rlabp) col.names <- c("row.names", col.names)

    if(length(colClasses) < cols) colClasses <- rep(colClasses, len=cols)

    ##	set up for the scan of the file.
    ##	we read unknown values as character strings and convert later.

    what <- rep(list(""), cols)
    names(what) <- col.names

    colClasses[colClasses %in% c("real", "double")] <- "numeric"
    known <- colClasses %in%
                c("logical", "integer", "numeric", "complex", "character")
    what[known] <- sapply(colClasses[known], do.call, list(0))

    data <- scan(file = file, what = what, sep = sep, quote = quote,
                 dec = dec, nmax = nrows, skip = 0,
		 na.strings = na.strings, quiet = TRUE, fill = fill,
                 strip.white = strip.white,
                 blank.lines.skip = blank.lines.skip, multi.line = FALSE,
                 comment.char = comment.char)

    nlines <- length(data[[1]])

    ##	now we have the data;
    ##	convert to numeric or factor variables
    ##	(depending on the specified value of "as.is").
    ##	we do this here so that columns match up

    if(cols != length(data)) { # this should never happen
	warning(paste("cols =", cols," != length(data) =", length(data)))
	cols <- length(data)
    }

    if(is.logical(as.is)) {
	as.is <- rep(as.is, length=cols)
    } else if(is.numeric(as.is)) {
	if(any(as.is < 1 | as.is > cols))
	    stop("invalid numeric as.is expression")
	i <- rep(FALSE, cols)
	i[as.is] <- TRUE
	as.is <- i
    } else if(is.character(as.is)) {
        i <- match(as.is, col.names, 0)
        if(any(i <= 0))
            warning("not all columns named in as.is exist")
        i <- i[i > 0]
        as.is <- rep(FALSE, cols)
        as.is[i] <- TRUE
    } else if (length(as.is) != cols)
	stop(paste("as.is has the wrong length",
		   length(as.is),"!= cols =", cols))
    for (i in 1:cols) {
#        if(known[i] || as.is[i]) next
        if(known[i]) next
        data[[i]] <-
            if (!is.na(colClasses[i])) as(data[[i]], colClasses[i])
            else type.convert(data[[i]], as.is = as.is[i], dec = dec,
                              na.strings = character(0))
        ## as na.strings have already be converted to <NA>
    }

    ##	now determine row names

    if (missing(row.names)) {
	if (rlabp) {
	    row.names <- data[[1]]
	    data <- data[-1]
	}
	else row.names <- as.character(seq(len=nlines))
    } else if (is.null(row.names)) {
	row.names <- as.character(seq(len=nlines))
    } else if (is.character(row.names)) {
	if (length(row.names) == 1) {
	    rowvar <- (1:cols)[match(col.names, row.names, 0) == 1]
	    row.names <- data[[rowvar]]
	    data <- data[-rowvar]
	}
    } else if (is.numeric(row.names) && length(row.names) == 1) {
	rlabp <- row.names
	row.names <- data[[rlabp]]
	data <- data[-rlabp]
    } else stop("invalid row.names specification")

    ##	this is extremely underhanded
    ##	we should use the constructor function ...
    ##	don't try this at home kids

    class(data) <- "data.frame"
    row.names(data) <- row.names
    data
}

read.csv <-
    function (file, header = TRUE, sep = ",", quote="\"", dec=".",
              fill = TRUE, ...)
    read.table(file = file, header = header, sep = sep,
               quote = quote, dec = dec, fill = fill, ...)

read.csv2 <-
    function (file, header = TRUE, sep = ";", quote="\"", dec=",",
              fill = TRUE, ...)
    read.table(file = file, header = header, sep = sep,
               quote = quote, dec = dec, fill = fill, ...)

read.delim <-
    function (file, header = TRUE, sep = "\t", quote="\"", dec=".",
              fill = TRUE, ...)
    read.table(file = file, header = header, sep = sep,
               quote = quote, dec = dec, fill = fill, ...)

read.delim2 <-
    function (file, header = TRUE, sep = "\t", quote="\"", dec=",",
              fill = TRUE, ...)
    read.table(file = file, header = header, sep = sep,
               quote = quote, dec = dec, fill = fill, ...)

