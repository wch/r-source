#  File src/library/utils/R/read.DIF.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1995-2013 The R Core Team
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
#  http://www.r-project.org/Licenses/

read.DIF <- function(file, header = FALSE, dec = ".", exact = FALSE,
         row.names, col.names, as.is = !stringsAsFactors,
         na.strings = "NA", colClasses = NA,
         nrows = -1, skip = 0,
         check.names = TRUE,
         blank.lines.skip = TRUE,
         stringsAsFactors = default.stringsAsFactors(),
	 transpose = FALSE)
{
    if (.Platform$OS.type == "windows" && identical(file, "clipboard")) {
	if (!(5 %in% getClipboardFormats(numeric=TRUE)) ) stop("No DIF data on clipboard")
	lines <- readClipboard(5)
    } else {
	lines <- readLines(file)
    }
    if(length(lines) < 1L) stop("file had no lines")
    topic <- ""
    nrow <- NA
    ncol <- NA
    i <- 1L
    ## Read header info :
    while (topic != "DATA") {
	topic <- lines[i]
	vnum <- lines[i+1]
	num <- as.numeric(sub("^.*,","",vnum))
	## v <- as.numeric(sub(",.*$","",vnum))
	## value <- lines[i+2]
	i <- i + 3L
	if (topic == "VECTORS")
	    if(transpose) nrow <- num else ncol <- num
	else if (topic == "TUPLES")
	    if(transpose) ncol <- num else nrow <- num
    }
    if (is.na(nrow) || is.na(ncol)) stop("row and column counts not found")

    data <- matrix("", nrow, ncol)
    types <- matrix(NA_character_, nrow, ncol)

    row <- 0L
    while (i < length(lines)) {
	typenum <- lines[i]
	type <- as.numeric(sub(",.*$","",typenum))
	num <- as.numeric(sub("^.*,","",typenum))
	stringval <- lines[i+1]
	i <- i + 2L
	if (type == -1L) {
	    if (stringval == "BOT") {
		row <- row + 1L
                if(row > nrow)
                    stop("More rows than specified in header; maybe use 'transpose=TRUE'")
		col <- 0L
	    } else if (stringval == "EOD") break
	    else stop("Unrecognized special data value")
	} else {
	    col <- col + 1L
            if(col > ncol)
                stop("More columns than specified in header; maybe use 'transpose=TRUE'")
            if (type == 0L) {
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
            } else if (type == 1L) {
                types[row, col] <- "character"
                stringval <- sub("^\"", "", stringval)
                stringval <- sub("\"$", "", stringval)
                data[row, col] <- stringval
            }
        }
    }

    if(skip > 0L) data <- data[-(1L:skip),,drop=FALSE]

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
    first <- data[1L, ]
    if (first[1L] == "") first <- first[-1L]

    col1 <- if(missing(col.names)) length(first) else length(col.names)
    cols <- ncol

    ##	basic column counting and header determination;
    ##	rlabp (logical) := it looks like we have column names
    rlabp <- all(types[1L, ][-1L] == "character") && data[1L, 1L] == ""
    if(rlabp && missing(header))
	header <- TRUE
    if(!header) rlabp <- FALSE

    if (header) {
    	data <- data[-1L,,drop=FALSE] # skip over header
    	types <- types[-1L,,drop=FALSE]
        if(missing(col.names)) col.names <- first
        else if(length(first) != length(col.names))
            warning("header and 'col.names' are of different lengths")

    } else if (missing(col.names))
	col.names <- paste0("V", 1L:cols)
    if(length(col.names) + rlabp < cols)
        stop("more columns than column names")
    if(cols > 0L && length(col.names) > cols)
        stop("more column names than columns")
    if(cols == 0L) stop("rows are empty: giving up")


    if(check.names) col.names <- make.names(col.names, unique = TRUE)
    if (rlabp) col.names <- c("row.names", col.names)

    nmColClasses <- names(colClasses)
    if(length(colClasses) < cols)
        if(is.null(nmColClasses)) {
            colClasses <- rep_len(colClasses, cols)
        } else {
            tmp <- rep_len(NA_character_, cols)
            names(tmp) <- col.names
            i <- match(nmColClasses, col.names, 0L)
            if(any(i <= 0L))
                warning("not all columns named in 'colClasses' exist")
            tmp[ i[i > 0L] ] <- colClasses
            colClasses <- tmp
        }


    ##	set up as if we'll scan the file.

    colClasses[colClasses %in% c("real", "double")] <- "numeric"
    known <- colClasses %in%
                c("logical", "integer", "numeric", "complex", "character")
    keep <- !(colClasses %in% "NULL")

    if (blank.lines.skip) data <- data[apply(data, 1L, function(x) !all(x == "")),,drop=FALSE]
    if (nrows > -1 && nrows < nrow(data)) data <- data[seq_len(nrows),,drop=FALSE]
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
	as.is <- rep_len(as.is, cols)
    } else if(is.numeric(as.is)) {
	if(any(as.is < 1 | as.is > cols))
	    stop("invalid numeric 'as.is' expression")
	i <- rep.int(FALSE, cols)
	i[as.is] <- TRUE
	as.is <- i
    } else if(is.character(as.is)) {
        i <- match(as.is, col.names, 0L)
        if(any(i <= 0L))
            warning("not all columns named in 'as.is' exist")
        i <- i[i > 0L]
        as.is <- rep.int(FALSE, cols)
        as.is[i] <- TRUE
    } else if (length(as.is) != cols)
	stop(gettextf("'as.is' has the wrong length %d  != cols = %d",
                     length(as.is), cols), domain = NA)

    do <- keep & !known # & !as.is
    if(rlabp) do[1L] <- FALSE # don't convert "row.names"
    for (i in (1L:cols)[do]) {
        data[[i]] <-
	    if (is.na(colClasses[i])) {
	        if (any(types[,i] == "character")) {
	            if (stringsAsFactors && !as.is[i]) as.factor(data[[i]])
	            else data[[i]]
		} else
		    type.convert(data[[i]], as.is = as.is[i], dec = dec,
				 na.strings = character(0L), exact=exact)
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
	    row.names <- data[[1L]]
	    data <- data[-1L]
            keep <- keep[-1L]
            compactRN <- FALSE
	}
	else row.names <- .set_row_names(as.integer(nlines))
    } else if (is.null(row.names)) {
	row.names <- .set_row_names(as.integer(nlines))
    } else if (is.character(row.names)) {
        compactRN <- FALSE
	if (length(row.names) == 1L) {
	    rowvar <- (1L:cols)[match(col.names, row.names, 0L) == 1L]
	    row.names <- data[[rowvar]]
	    data <- data[-rowvar]
            keep <- keep[-rowvar]
	}
    } else if (is.numeric(row.names) && length(row.names) == 1L) {
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
        if (anyDuplicated(row.names))
            stop("duplicate 'row.names' are not allowed")
        if (anyNA(row.names))
            stop("missing values in 'row.names' are not allowed")
    }

    ##	this is extremely underhanded
    ##	we should use the constructor function ...
    ##	don't try this at home kids

    attr(data, "row.names") <- row.names
    data
}
