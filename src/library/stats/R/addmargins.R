#  File src/library/stats/R/addmargins.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 2004-2015 The R Core Team
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

addmargins <-
    function(A, margin = seq_along(dim(A)), FUN = sum, quiet = FALSE)
{
    ## The workhorse for this margin-expansion is the function
    ## expand.one, which is defined and called at the bottom.
    ##
    ## All this initial stuff is just to check consistency of
    ## specifications, and form maximally sensible margin names
    ##
    ## BxC, August 2003
    ##	 Sept	2003: Single margins caused crash. Fixed.
    ## Duncan Murdoch, Feb 2004: Machinery to derive functionnames
    ##			      from unnamed lists
    ##-------------------------------------------------------------

    if(is.null(dim(A))) stop("'A' must be an array or table")
    ## How many dimensions of A, and how many sides do we touch?
    n.sid <- length(margin)

    ## Check if FUN was specified
    ##
    miss.FUN <- missing(FUN)

    ## Check if FUN has the same length as margin, and if not, stop or
    ## expand a single function specification to a list of the same
    ## length as the margins vector.
    if (length(FUN) == 1 && !is.list(FUN)) {
	fname <- if (!miss.FUN) deparse(substitute(FUN)) else "Sum"
	FUN <- setNames(list(FUN), fname)
    }

    if (!miss.FUN) {
	## Recursive function to add names to unnamed list components
	add.names <- function(thelist)
	{
	    n <- names(thelist)
	    if (is.null(n)) n <- rep("", length(thelist))
	    for (i in seq_along(thelist)[-1L]) {
		if (!is.call(thelist[[i]])) {
		    if (n[i] == "") n[i] <- as.character(thelist[[i]])
		} else if (as.character(thelist[[i]][[1L]]) == "list")
		    thelist[[i]] <- add.names(thelist[[i]])
	    }
	    names(thelist) <- n
	    thelist
	}
	## this only makes sense if we were given an expression for FUN
	## which we can deparse.
	if(mode(substitute(FUN)) == "call")
	    FUN <- eval(add.names(substitute(FUN)))
	if (is.null(names(FUN))) names(FUN) <- rep("", length(FUN))
    }

    ## At this point FUN is a list with names wherever
    ## we could figure them out, empty strings otherwise

    if(length(FUN) != n.sid) {
	if(length(FUN) == 1L)
	    FUN <- rep(FUN, n.sid)
	else
	    stop(gettextf(
		"length of FUN, %d,\n does not match the length of the margins, %d",
			  length(FUN), n.sid), domain = NA)
    }

    ## If FUN is not given the default sum is put in the margin
    ## otherwise make a list to fill with names
    ##
    fnames <- vector("list", n.sid)

    ## Use the names from FUN and also possibly the names from
    ## sublists of FUN.	 Replace blanks with constructed names

    for(i in seq_along(FUN)) {
	fnames[[i]] <- names(FUN)[i]
	if (is.list(FUN[[i]])) {
	    topname <- fnames[[i]]
	    fnames[[i]] <- names(FUN[[i]])
	    blank <- fnames[[i]] == ""
	    fnames[[i]][blank] <- seq_along(blank)[blank]
	    if (topname == "") {
		fnames[[i]][blank] <-
		    paste0("Margin ", margin[i], ".", fnames[[i]][blank])
	    } else {
		fnames[[i]] <- paste0(topname, ".", fnames[[i]])
	    }
	} else if (fnames[[i]] == "")
            fnames[[i]] <- paste("Margin", margin[i])
    }

    ## So finally we have the relevant form of FUN and fnames to pass
    ## on to expand.one which expands over one factor at a time.

    expand.one <- function(A, margin, FUN, fnames)
    {
	## Function to expand a table with a set of margins over the
	## side <margin>, i.e. by a set of marginal tables classified by
	## all factors except <margin>.
	##
	## BxC, August 2003

	## Make sure that FUN is a list
	if(!inherits(FUN, "list")) FUN <- list(FUN)

	## Useful constants
	d <- dim(A)
	n.dim <- length(d)   # number of dimensions in the table
	n.mar <- length(FUN) # number of margins to be added

	## Define the dimensions of the new table with the margins
	newdim <- d
	newdim[margin] <- newdim[margin] + n.mar
	if(is.null(dnA <- dimnames(A))) dnA <- vector("list", n.dim)
	dnA[[margin]] <-
	    c(if(is.null(dnA[[margin]])) rep("", d[[margin]]) else dnA[[margin]],
	      fnames)

	## Number of elements in the expanded array
	n.new <- prod(newdim)

	## The positions in the vector-version of the new table
	## where the original table values goes, as a logical vector
	skip <- prod(d[1L:margin])
	runl <- skip / d[margin]
	apos <- rep(c(rep_len(TRUE, skip), rep_len(FALSE, n.mar*runl)),
		    n.new/(skip+n.mar*runl))

	## Define a vector to hold all the values of the new table
	values <- double(length(apos))

	## First fill in the body of the table
	values[apos] <- as.vector(A)

	## Then sucessively compute and fill in the required margins
	for(i in 1L:n.mar) {
	    mtab <- if(n.dim > 1)
			apply(A, (1L:n.dim)[-margin], FUN[[i]])
		    else
			FUN[[i]](A)
	    ## Vector the same length as the number of margins
	    select <- rep_len(FALSE, n.mar)
	    ## The position of the current margin
	    select[i] <- TRUE
	    ## Expand that to a vector the same length as the entire new matrix
	    mpos <- rep(c(rep_len(FALSE, skip), rep(select, each=runl)),
			prod(dim(A))/skip)
	    ## Fill the marginal table in there
	    values[mpos] <- as.vector(mtab)
	}

	## the new table with contents and margins
	array(values, dim=newdim, dimnames=dnA)
    }

    ## Once defined, we can use the expand.one function repeatedly
    new.A <- A
    for(i in 1L:n.sid)
	new.A <- expand.one(A = new.A, margin = margin[i], FUN = FUN[[i]],
			    fnames = fnames[[i]])
    if(inherits(A, "table")) # result shall be table, too
        class(new.A) <- c("table", class(new.A))

    ## Done! Now print it.
    ##
    if(!quiet && !miss.FUN && n.sid > 1) {
	cat("Margins computed over dimensions\nin the following order:\n")
        ## FIXME: what is paste(i) supposed to do?
	for(i in seq_len(n.sid))
	    cat(paste(i), ": ", names(dimnames(A))[margin[i]], "\n", sep = "")
    }
    new.A
}
