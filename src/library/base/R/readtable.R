count.fields <- function(file, sep = "", quote = "", skip = 0)
    .Internal(count.fields(file, sep, quote, skip))


read.table <-
    function (file, header=FALSE, sep="", quote="\"\'", dec=".",
              row.names, col.names, as.is=FALSE,
	      na.strings="NA", skip=0)
{
    type.convert <- function(x, na.strings = "NA",
                             as.is = FALSE, dec = ".")
	.Internal(type.convert(x, na.strings, as.is, dec))

    ##	basic column counting and header determination;
    ##	rlabp (logical) := it looks like we have column names

    row.lens <- count.fields(file, sep, quote, skip)
    nlines <- length(row.lens)
    rlabp <- nlines > 1 && (row.lens[2] - row.lens[1]) == 1
    if(rlabp && missing(header))
	header <- TRUE

    if (header) { # read in the header
	col.names <- scan(file, what="", sep=sep, quote=quote, nlines=1,
			  quiet=TRUE, skip=skip)
	skip <- skip + 1
	row.lens <- row.lens[-1]
	nlines <- nlines - 1
    } else if (missing(col.names))
	col.names <- paste("V", 1:row.lens[1], sep="")

    ##	check that all rows have equal lengths

    cols <- unique(row.lens)
    if (length(cols) != 1) {
	cat("\nrow.lens=\n"); print(row.lens)
	stop("all rows must have the same length.")
    }

    ##	set up for the scan of the file.
    ##	we read all values as character strings and convert later.

    what <- rep(list(""), cols)
    if (rlabp)
	col.names <- c("row.names", col.names)
    names(what) <- col.names
    data <- scan(file=file, what=what, sep=sep, quote=quote, skip=skip,
		 na.strings=na.strings, quiet=TRUE)

    ##	now we have the data;
    ##	convert to numeric or factor variables
    ##	(depending on the specifies value of "as.is").
    ##	we do this here so that columns match up

    if(cols != length(data)) { # this should never happen
	warning(paste("cols =",cols," != length(data) =", length(data)))
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
              row.names, col.names, as.is=FALSE, na.strings="", skip=0)
    read.table(file, header, sep, quote, dec, row.names, col.names,
               as.is, na.strings, skip)
read.csv2 <-
    function (file, header = TRUE, sep = ";", quote="\"", dec=",",
              row.names, col.names, as.is=FALSE, na.strings="", skip=0)
    read.table(file, header, sep, quote, dec, row.names, col.names,
               as.is, na.strings, skip)
