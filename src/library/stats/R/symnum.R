symnum <- function(x, cutpoints = c(  .3,  .6,	 .8,  .9, .95),
		   symbols =	 c(" ", ".", ",", "+", "*", "B"),
		   legend = length(symbols) >= 3,
		   na = "?", eps = 1e-5,
		   corr = missing(cutpoints),
		   show.max = if(corr) "1", show.min = NULL,
		   abbr.colnames = has.colnames,
		   lower.triangular = corr && is.numeric(x) && is.matrix(x),
		   diag.lower.tri = corr && !is.null(show.max))
{
    ## Martin Maechler, 21 Jan 1994; Dedicated to Benjamin Schaad, born that day

    ##--------------- Argument checking -----------------------------
    if(length(x) == 0)
	return(noquote(if(is.null(d <- dim(x)))character(0) else array("", dim=d)))
    has.na <- any(nax <- is.na(x))
    num.x <- is.numeric(x)## !is.logical(x)
    if(num.x) {
	force(corr) # missingness..
	cutpoints <- sort(cutpoints)
	if(corr) cutpoints <- c(0, cutpoints, 1)
	if(any(duplicated(cutpoints)) ||
	   (corr && (any(cutpoints > 1) || any(cutpoints < 0)) ))
	    stop("'cutpoints' must be unique ",
                 if(corr) "in 0 < cuts < 1", ", but are = ",
                 paste(format(cutpoints), collapse="|"))
	nc <- length(cutpoints)
	minc <- cutpoints[1]
	maxc <- cutpoints[nc]
	range.msg <- paste("'x' must be between",
			   if(corr) "-1" else format(minc),
			   " and", if(corr) "1" else format(maxc)," !")
	if(corr) x <- abs(x)
	else
	    if(any(x < minc - eps, na.rm=TRUE)) stop(range.msg)
	if (   any(x > maxc + eps, na.rm=TRUE)) stop(range.msg)

	ns <- length(symbols)
	symbols <- as.character(symbols)
	if(any(duplicated(symbols)))
	    stop("'symbols' must be unique, but are = ",
                 paste(symbols, collapse="|"))
	if(nc != ns+1)
	    stop("number of cutpoints must be  ONE ",
                 if(corr)"LESS" else "MORE", " than number of symbols")

	iS <- cut(x, breaks=cutpoints, include.lowest=TRUE, labels= FALSE)
	if(any(ii <- is.na(iS))) {
	    ##-- can get 0, if x[i]== minc  --- only case ?
	    iS[which(ii)[!is.na(x[ii]) & (abs(x[ii] - minc) < eps)]] <- 1#-> symbol[1]
	}
    }
    else if(!is.logical(x))
	stop("'x' must be numeric or logical")
    else  { ## logical x : no need for cut(points)
	if(missing(symbols))		# different default
	    symbols <- c(".","|")
	else if(length(symbols) != 2)
	    stop("must have 2 'symbols' for logical 'x' argument")
	iS <- x + 1 # F = 1,  T = 2
    }
    if(has.na) {
	ans <- character(length(iS))
	if((has.na <- is.character(na)))
	    ans[nax] <- na
	ans[!nax] <- symbols[iS[!nax]]
    } else ans <- symbols[iS]
    if(num.x) {
	if(!is.null(show.max)) ans[x >= maxc - eps] <-
	    if(is.character(show.max)) show.max else format(maxc, dig=1)
	if(!is.null(show.min)) ans[x <= minc + eps] <-
	    if(is.character(show.min)) show.min else format(minc, dig=1)
    }
    if(lower.triangular && is.matrix(x))
	ans[!lower.tri(x, diag = diag.lower.tri)] <- ""
    attributes(ans) <- attributes(x)
    if(is.array(ans)&& (rank <- length(dim(x))) >= 2) { # `fix' column names
	has.colnames <- !is.null(dimnames(ans))
	if(!has.colnames) {
	    dimnames(ans) <- vector("list",rank)
	} else {
	    has.colnames <- length(dimnames(ans)[[2]]) > 0
	}
	if((is.logical(abbr.colnames) || is.numeric(abbr.colnames))
	   && abbr.colnames) {
	    dimnames(ans)[[2]] <-
		abbreviate(dimnames(ans)[[2]], minlength= abbr.colnames)
	    ## dropped further abbrev. depending on getOption("width")
	}
	else if(is.null(abbr.colnames) || is.null(dimnames(ans)[[2]]))
	    dimnames(ans)[[2]] <- rep("", dim(ans)[2])
	else if(!is.logical(abbr.colnames)) stop("invalid 'abbr.colnames'")
    }
    if(legend) {
	legend <- c(rbind(sapply(cutpoints,format),
			  c(sQuote(symbols),"")),
		    if(has.na) paste("	    ## NA:", sQuote(na)))
	attr(ans,"legend") <- paste(legend[-2*(ns+1)], collapse=" ")
    }
    noquote(ans)
}
