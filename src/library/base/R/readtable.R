count.fields <- function(file, sep = "", quote = "\"'", skip = 0,
                         blank.lines.skip = TRUE)
{
    if(is.character(file)) {
        file <- file(file)
        on.exit(close(file))
    }
    if(!inherits(file, "connection"))
        stop("argument `file' must be a character string or connection")
    .Internal(count.fields(file, sep, quote, skip, blank.lines.skip))
}

read.table <-
    function (file, header = FALSE, sep = "", quote = "\"'", dec = ".",
              row.names, col.names, as.is = FALSE,
	      na.strings = "NA", skip = 0,
              check.names = TRUE, fill = !blank.lines.skip,
              strip.white = FALSE, blank.lines.skip = TRUE)
{
    type.convert <- function(x, na.strings = "NA",
                             as.is = FALSE, dec = ".")
	.Internal(type.convert(x, na.strings, as.is, dec))

    if(is.character(file)) {
        file <- file(file, "r")
        on.exit(close(file))
    }
    if(!inherits(file, "connection"))
        stop("argument `file' must be a character string or connection")

    if(isSeekable(file)) {
        if(skip > 0) readLines(file, skip)
        row.lens <- count.fields(file, sep, quote, 0, blank.lines.skip)
        seek(file, 0)
        if(skip > 0) readLines(file, skip)
        tfile <- file
    } else {
        warning("readLines on a non-seekable connection can be resource-intensive")
        if(skip > 0) readLines(file, skip)
        whole <- readLines(file)
        tfile <- textConnection(whole)
        row.lens <- count.fields(tfile, sep, quote, 0, blank.lines.skip)
        close(tfile)
        tfile <- textConnection(whole)
        on.exit(close(tfile), add = TRUE)
    }

    ##	basic column counting and header determination;
    ##	rlabp (logical) := it looks like we have column names

    nlines <- length(row.lens)
    rlabp <- nlines > 1 && (max(row.lens[-1]) - row.lens[1]) == 1
    if(rlabp && missing(header))
	header <- TRUE

    if (header) { # read in the header
        colnm <- scan(tfile, what = "", sep = sep, quote = quote, nlines = 1,
                      quiet = TRUE, skip = 0, strip.white = TRUE)
	row.lens <- row.lens[-1]
	nlines <- nlines - 1
        if(missing(col.names)) col.names <- colnm
    } else if (missing(col.names))
	col.names <- paste("V", 1:max(row.lens), sep = "")

    if(check.names) col.names <- make.names(col.names)

    ##	check that all rows have equal lengths unless fill == TRUE

    if ( !fill ) {
        cols <- unique(row.lens)
        if (length(cols) != 1) {
            cat("\nrow.lens=\n"); print(row.lens)
            stop("all rows must have the same length.")
        }
    } else {
        cols <- max(row.lens)
        if (header) {
            if (cols > length(col.names) + rlabp) {
                cat("\nrow.lens=\n"); print(row.lens)
                stop("Some rows have more fields than header implies")
            }
            cols <- length(col.names) + rlabp
        }
    }


    ##	set up for the scan of the file.
    ##	we read all values as character strings and convert later.

    what <- rep(list(""), cols)
    if (rlabp)
	col.names <- c("row.names", col.names)
    names(what) <- col.names
    data <- scan(file = tfile, what = what, sep = sep, quote = quote, skip = 0,
		 na.strings = na.strings, quiet = TRUE, fill = fill,
                 strip.white = strip.white, nmax = nlines,
                 blank.lines.skip = blank.lines.skip)

    if(!blank.lines.skip && row.lens[nlines] == 0) {
        ## we had a blank last line
        for (i in 1:cols)
            data[[i]] <- data[[i]][-nlines]
        nlines <- nlines - 1
    }
    ##	now we have the data;
    ##	convert to numeric or factor variables
    ##	(depending on the specifies value of "as.is").
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
    } else if (length(as.is) != cols)
	stop(paste("as.is has the wrong length",
		   length(as.is),"!= cols =", cols))
    for (i in 1:cols)
        data[[i]] <- type.convert(data[[i]], as.is = as.is[i], dec = dec)

    ##	now determine row names

    if (missing(row.names)) {
	if (rlabp) {
	    row.names <- data[[1]]
	    data <- data[-1]
	}
	else row.names <- as.character(1:nlines)
    } else if (is.null(row.names)) {
	row.names <- as.character(1:nlines)
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

