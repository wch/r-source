symnum <- function(x, cutpoints = c(  .3,  .6,	 .8,  .9, .95),
		   symbols =	 c(" ", ".", ",", "+", "*", "B"),
		   legend = length(symbols) >= 3,
		   na = "?", eps = 1e-5,
		   corr = missing(cutpoints),
		   show.max = if(corr) "1", show.min = NULL,
		   lower.triangular = corr & is.matrix(x),
		   diag.lower.tri = corr & !is.null(show.max))
{
    ## Martin Maechler, 21 Jan 1994; Dedicated to Benjamin Schaad, born that day

    ##--------------- Argument checking -----------------------------
    if(length(x) == 0)
        return(noquote("()"))
    has.na <- any(nax <- is.na(x))
    num.x <- is.numeric(x)## !is.logical(x)
    if(num.x) {
        corr # eval missingness..
	cutpoints <- sort(cutpoints)
	if(corr) cutpoints <- c(0, cutpoints, 1)
	if(any(duplicated(cutpoints)) ||
	   (corr && (any(cutpoints > 1) || any(cutpoints < 0)) ))
	    stop(paste("'cutpoints' must be unique",
		       if(corr)"in 0 < cuts < 1", ", but are =",
		       paste(format(cutpoints), collapse="|")))
	nc <- length(cutpoints)
	minc <- cutpoints[1]
	maxc <- cutpoints[nc]
	range.msg <- paste("'x' must be between",
			   if(corr) "-1" else format(minc),
			   " and", if(corr) "1" else format(maxc)," !")
	if(corr) x <- abs(x)
	else
	    if(any(x < minc - eps, na.rm=TRUE)) stop(range.msg)
	if ( any(x > maxc + eps, na.rm=TRUE)) stop(range.msg)

	ns <- length(symbols)
	symbols <- as.character(symbols)
	if(any(duplicated(symbols)))
	    stop(paste("'symbols' must be unique, but are =",
		       paste(symbols, collapse="|")))
	if(nc != ns+1)
	    stop(paste("number of cutpoints must be  ONE",
		       if(corr)"LESS" else "MORE", "than number of symbols"))

	iS <- cut(x, breaks=cutpoints, include.lowest=TRUE, labels= FALSE)
	if(any(ii <- is.na(iS))) {
	    ##-- can get 0, if x[i]== minc  --- only case ?
	    iS[which(ii)[abs(x[ii] - minc) < eps]] <- 1#-> symbol[1]
	}
    }
    else if(!is.logical(x))
        stop("`x' must be numeric or logical")
    else  { ## logical x : no need for cut(points)
	if(missing(symbols))		# different default
	    symbols <- c(".","|")
	else if(length(symbols) != 2)
	    stop("must have 2 `symbols' for logical `x' argument")
	iS <- x + 1 # F = 1,  T = 2
    }
    if(has.na) {
	Scor <- character(length(iS))
	if((has.na <- is.character(na)))
	    Scor[nax] <- na
	Scor[!nax] <- symbols[iS[!nax]]
    } else Scor <- symbols[iS]
    if(num.x) {
	if(!is.null(show.max)) Scor[x >= maxc - eps] <-
	    if(is.character(show.max)) show.max else format(maxc, dig=1)
	if(!is.null(show.min)) Scor[x <= minc + eps] <-
	    if(is.character(show.min)) show.min else format(minc, dig=1)
    }
    if(lower.triangular && is.matrix(x))
	Scor[!lower.tri(x, diag = diag.lower.tri)] <- ""
    attributes(Scor) <- attributes(x)
    if(is.array(Scor)&& (rank <- length(dim(x))) >= 2) { # `fix' column names
	if(is.null(dimnames(Scor)))
	    dimnames(Scor) <- vector("list",rank)
	coln <- dimnames(Scor)[[2]]
	dimnames(Scor)[[2]] <-
	    if(length(coln)) {
		ch <- abbreviate(coln, minlength=1)
		if(sum(1+nchar(ch)) + max(nchar(coln))+ 1 > getOption("width"))
					#-- would not fit on one line
		    abbreviate(ch, minlength=2, use.classes=FALSE)
		else ch
	    }
	    else rep("", dim(Scor)[2])
    }
    if(legend) {
	legend <- c(rbind(sapply(cutpoints,format),
			  c(paste("`",symbols,"'",sep=""),"")),
		    if(has.na) paste("	    ## NA: `",na,"'",sep=""))
	attr(Scor,"legend") <- paste(legend[-2*(ns+1)], collapse=" ")
    }
    noquote(Scor)
}
