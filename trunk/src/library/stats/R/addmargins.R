addmargins <-
    function(A, margin = 1:length(dim(A)), FUN = sum, quiet = FALSE)
{
### The workhorse for this margin-expansion is the function
### expand.one, which is defined and called at the bottom.
###
### All this initial stuff is just to check consistency of
### specifications, and form maximally sensible margin names
###
### BxC, August 2003
###	 Sept	2003: Single margins caused crash. Fixed.
### Duncan Murdoch, Feb 2004: Machinery to derive functionnames
###			      from unnamed lists
###-------------------------------------------------------------

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
	FUN <- list(FUN)
	names(FUN) <- fname
    }

    if (!miss.FUN) {
	## Recursive function to add names to unnamed list components
	add.names <- function(thelist)
	{
	    n <- names(thelist)
	    if (is.null(n)) n <- rep("", length(thelist))
	    for (i in seq_along(thelist)[-1]) {
		if (!is.call(thelist[[i]])) {
		    if (n[i] == "") n[i] <- as.character(thelist[[i]])
		} else if (as.character(thelist[[i]][[1]]) == "list")
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
	if(length(FUN) == 1)
	    FUN <- rep(FUN, n.sid)
	else
	    stop(gettextf("length of FUN, %d,\n does not match the length of the margins, %d",
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
		    paste("Margin ", margin[i], ".", fnames[[i]][blank],
			  sep = "")
	    } else {
		fnames[[i]] <- paste(topname, ".", fnames[[i]], sep = "")
	    }
	} else
	    if (fnames[[i]] == "") fnames[[i]] <- paste("Margin", margin[i])
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
	if(class(FUN) != "list") FUN <- list(FUN)

	## Useful constants
	d <- dim(A)
	n.dim <- length(d)   # number of dimensions in the table
	n.mar <- length(FUN) # number of margins to be added

	## Define the dimensions of the new table with the margins
	newdim <- d
	newdim[margin] <- newdim[margin] + n.mar
	newdimnames <- dimnames(A)
	newdimnames[[margin]] <- c(newdimnames[[margin]], fnames)

	## Number of elements in the expanded array
	n.new <- prod(newdim)

	## The positions in the vector-version of the new table
	## where the original table values goes, as a logical vector
	skip <- prod(d[1:margin])
	runl <- skip / d[margin]
	apos <- rep(c(rep(TRUE, skip), rep(FALSE, n.mar*runl)),
		    n.new/(skip+n.mar*runl))

	## Define a vector to hold all the values of the new table
	values <- numeric(apos)

	## First fill in the body of the table
	values[apos] <- as.vector(A)

	## Then sucessively compute and fill in the required margins
	for(i in 1:n.mar) {
	    mtab <- if(n.dim > 1) {
		apply(A, (1:n.dim)[-margin], FUN[[i]])
	    } else FUN[[i]](A)
	    ## Vector the same length as the number of margins
	    select <- rep(FALSE, n.mar)
	    ## The position of the current margin
	    select[i] <- TRUE
	    ## Expand that to a vector the same length as the entire new matrix
	    mpos <- rep(c(rep(FALSE, skip), rep(select, each=runl)),
			prod(dim(A))/skip)
	    ## Fill the marginal table in there
	    values[mpos] <- as.vector(mtab)
	}

	## Then define the new table with contents and margins
	##
	new.A <- array(values, dim=newdim, dimnames=newdimnames)
	if(inherits(A, "table")) # result shall be table, too
	    class(new.A) <- c("table", class(new.A))
	new.A
    }

    ## Once defined, we can use the expand.one function repeatedly
    new.A <- A
    for(i in 1:n.sid)
	new.A <- expand.one(A = new.A, margin = margin[i], FUN = FUN[[i]],
			    fnames = fnames[[i]])

    ## Done! Now print it.
    ##
    if(!quiet && !miss.FUN  && n.sid > 1) {
	cat("Margins computed over dimensions\nin the following order:\n")
	for(i in 1:n.sid)
	    cat(paste(i), ": ", names(dimnames(A))[margin[i]], "\n", sep="")
    }
    new.A
}
