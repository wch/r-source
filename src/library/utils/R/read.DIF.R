read.DIF <- function(file, header = FALSE, dec = ".",
         row.names, col.names, as.is = !stringsAsFactors,
         na.strings = "NA", colClasses = NA,
         nrows = -1, skip = 0,
         check.names = TRUE,
         blank.lines.skip = TRUE,
         stringsAsFactors = default.stringsAsFactors())
{
    if (.Platform$OS.type == "windows" && identical(file, "clipboard")) {
	if (!(5 %in% getClipboardFormats()) ) stop("No DIF data on clipboard")
	lines <- readClipboard(5)
    } else
    {
	lines <- readLines(file)
    }
    topic <- ""
    nrow <- NA
    ncol <- NA
    i <- 1
    while (topic != "DATA") {
	topic <- lines[i]
	vnum <- lines[i+1]
	v <- as.numeric(sub(",.*$","",vnum))
	num <- as.numeric(sub("^.*,","",vnum))
	value <- lines[i+2]
 	i <- i + 3
	if (topic == "VECTORS") ncol <- num
	else if (topic == "TUPLES") nrow <- num
    }
    if (is.na(nrow) || is.na(ncol)) stop("row and column counts not found")

    data <- matrix("", nrow, ncol)
    types <- matrix(NA_character_, nrow, ncol)

    row <- 0
    while (i < length(lines)) {
	typenum <- lines[i]
	type <- as.numeric(sub(",.*$","",typenum))
	num <- as.numeric(sub("^.*,","",typenum))
	stringval <- lines[i+1]
	i <- i + 2
	if (type == -1) {
	    if (stringval == "BOT") {
		row <- row + 1
		col <- 0
	    } else if (stringval == "EOD") break
	    else stop("Unrecognized special data value")
	} else if (type == 0) {
	    col <- col + 1
	    types[row, col] <- "numeric"
	    if (stringval == "V") data[row, col] <- num
	    else if (stringval == "NA") data[row, col] <- NA
	    else if (stringval == "ERROR") data[row, col] <- NA
	    else if (stringval == "TRUE") {
		data[row, col] <- "TRUE"
		types[row, col] <- "logical"
	    }
	    else if (stringval == "FALSE") {
		data[row, col] <- "FALSE"
		types[row, col] <- "logical"
	    }
	    else stop("Unrecognized value indicator")
	} else if (type == 1) {
	    col <- col + 1
	    types[row, col] <- "character"
	    stringval <- sub("^\"", "", stringval)
	    stringval <- sub("\"$", "", stringval)
	    data[row, col] <- stringval
	}
    }

    if(skip > 0) data <- data[-(1:skip),]

    ## determine header, no of cols.
    nlines <- nrow(data)

    if (!nlines) {
        if (missing(col.names))
            stop("no lines available in input")
        else {
            tmp <- vector("list", length(col.names))
            names(tmp) <- col.names
            class(tmp) <- "data.frame"
            return(tmp)
        }
    }
    first <- data[1,]
    if (first[1] == "") first <- first[-1]

    col1 <- if(missing(col.names)) length(first) else length(col.names)
    cols <- ncol

    ##	basic column counting and header determination;
    ##	rlabp (logical) := it looks like we have column names
    rlabp <- all(types[1,][-1] == "character") && data[1,1] == ""
    if(rlabp && missing(header))
	header <- TRUE
    if(!header) rlabp <- FALSE

    if (header) {
    	data <- data[-1,] # skip over header
    	types <- types[-1,]
        if(missing(col.names)) col.names <- first
        else if(length(first) != length(col.names))
            warning("header and 'col.names' are of different lengths")

    } else if (missing(col.names))
	col.names <- paste("V", 1:cols, sep = "")
    if(length(col.names) + rlabp < cols)
        stop("more columns than column names")
    if(cols > 0 && length(col.names) > cols)
        stop("more column names than columns")
    if(cols == 0) stop("rows are empty: giving up")


    if(check.names) col.names <- make.names(col.names, unique = TRUE)
    if (rlabp) col.names <- c("row.names", col.names)

    nmColClasses <- names(colClasses)
    if(length(colClasses) < cols)
        if(is.null(nmColClasses)) {
            colClasses <- rep(colClasses, length.out=cols)
        } else {
            tmp <- rep(NA_character_, length.out=cols)
            names(tmp) <- col.names
            i <- match(nmColClasses, col.names, 0)
            if(any(i <= 0))
                warning("not all columns named in 'colClasses' exist")
            tmp[ i[i > 0] ] <- colClasses
            colClasses <- tmp
        }


    ##	set up as if we'll scan the file.

    colClasses[colClasses %in% c("real", "double")] <- "numeric"
    known <- colClasses %in%
                c("logical", "integer", "numeric", "complex", "character")
    keep <- !(colClasses %in% "NULL")

    if (blank.lines.skip) data <- data[apply(data, 1, function(x) !all(x == "")),]
    if (nrows > -1 && nrows < nrow(data)) data <- data[seq_len(nrows),]
    nlines <- nrow(data)

    data[data %in% na.strings] <- NA
    data <- as.data.frame(data, stringsAsFactors = FALSE)
    names(data) <- col.names

    ##	now we have the data;
    ##	convert to numeric or factor variables
    ##	(depending on the specified value of "as.is").
    ##	we do this here so that columns match up

    if(cols != length(data)) { # this should never happen
	warning("cols = ", cols, " != length(data) = ", length(data),
                domain = NA)
	cols <- length(data)
    }

    if(is.logical(as.is)) {
	as.is <- rep(as.is, length.out=cols)
    } else if(is.numeric(as.is)) {
	if(any(as.is < 1 | as.is > cols))
	    stop("invalid numeric 'as.is' expression")
	i <- rep.int(FALSE, cols)
	i[as.is] <- TRUE
	as.is <- i
    } else if(is.character(as.is)) {
        i <- match(as.is, col.names, 0)
        if(any(i <= 0))
            warning("not all columns named in 'as.is' exist")
        i <- i[i > 0]
        as.is <- rep.int(FALSE, cols)
        as.is[i] <- TRUE
    } else if (length(as.is) != cols)
	stop(gettextf("'as.is' has the wrong length %d  != cols = %d",
                     length(as.is), cols), domain = NA)

    do <- keep & !known # & !as.is
    if(rlabp) do[1] <- FALSE # don't convert "row.names"
    for (i in (1:cols)[do]) {
        data[[i]] <-
            if (is.na(colClasses[i])) {
            	if (!any(types[,i] == "character"))
                    type.convert(data[[i]], as.is = as.is[i], dec = dec,
                                 na.strings = character(0))
                else data[[i]]
            }
        ## as na.strings have already been converted to <NA>
            else if (colClasses[i] == "factor") as.factor(data[[i]])
            else if (colClasses[i] == "Date") as.Date(data[[i]])
            else if (colClasses[i] == "POSIXct") as.POSIXct(data[[i]])
            else methods::as(data[[i]], colClasses[i])
    }

    ##	now determine row names
    compactRN <- TRUE
    if (missing(row.names)) {
	if (rlabp) {
	    row.names <- data[[1]]
	    data <- data[-1]
            keep <- keep[-1]
            compactRN <- FALSE
	}
	else row.names <- .set_row_names(as.integer(nlines))
    } else if (is.null(row.names)) {
	row.names <- .set_row_names(as.integer(nlines))
    } else if (is.character(row.names)) {
        compactRN <- FALSE
	if (length(row.names) == 1) {
	    rowvar <- (1:cols)[match(col.names, row.names, 0) == 1]
	    row.names <- data[[rowvar]]
	    data <- data[-rowvar]
            keep <- keep[-rowvar]
	}
    } else if (is.numeric(row.names) && length(row.names) == 1) {
        compactRN <- FALSE
	rlabp <- row.names
	row.names <- data[[rlabp]]
	data <- data[-rlabp]
        keep <- keep[-rlabp]
    } else stop("invalid 'row.names' specification")
    data <- data[keep]

    ## rownames<- is interpreted, so avoid it for efficiency (it will copy)
    if(is.object(row.names) || !(is.integer(row.names)) )
        row.names <- as.character(row.names)
    if(!compactRN) {
        if (length(row.names) != nlines)
            stop("invalid 'row.names' length")
        if (any(duplicated(row.names)))
            stop("duplicate 'row.names' are not allowed")
        if (any(is.na(row.names)))
            stop("missing values in 'row.names' are not allowed")
    }

    ##	this is extremely underhanded
    ##	we should use the constructor function ...
    ##	don't try this at home kids

    attr(data, "row.names") <- row.names
    data
}
