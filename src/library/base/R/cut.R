cut <- function(x, ...) UseMethod("cut")

cut.default <- function (x, breaks, labels=NULL, include.lowest = FALSE,
			 right=TRUE, dig.lab=3, ...)
{
    if (!is.numeric(x)) stop("'x' must be numeric")
    if (length(breaks) == 1) {
	if (is.na(breaks) | breaks < 2)
	    stop("invalid number of intervals")
	nb <- as.integer(breaks + 1)# one more than #{intervals}
	dx <- diff(rx <- range(x,na.rm=TRUE))
	if(dx==0) dx <- rx[1]
	breaks <- seq(rx[1] - dx/1000,
		      rx[2] + dx/1000, len=nb)
    } else nb <- length(breaks <- sort(breaks))
    if (any(duplicated(breaks))) stop("'breaks' are not unique")
    codes.only <- FALSE
    if (is.null(labels)) {#- try to construct nice ones ..
	for(dig in dig.lab:12) {
	    ch.br <- formatC(breaks, digits=dig, wid=1)
	    if(ok <- all(ch.br[-1]!=ch.br[-nb])) break
	}
	labels <-
	    if(ok) paste(if(right)"(" else "[",
			 ch.br[-nb], ",", ch.br[-1],
			 if(right)"]" else ")", sep='')
	    else paste("Range", 1:(nb - 1),sep="_")
        if (ok && include.lowest) {
            if (right)
                substr(labels[1], 1, 1) <- "[" # was "("
            else
                substring(labels[nb-1],
                          nchar(labels[nb-1], type="char")) <- "]" # was ")"
        }
    } else if (is.logical(labels) && !labels)
        codes.only <- TRUE
    else if (length(labels) != nb-1)
        stop("labels/breaks length conflict")
    code <- .C("bincode",
	       x =     	as.double(x),
	       n =	as.integer(length(x)),
	       breaks =	as.double(breaks),
               as.integer(nb),
	       code= 	integer(length(x)),
               right=	as.logical(right),
	       include= as.logical(include.lowest), naok = TRUE,
	       NAOK= TRUE, DUP = FALSE, PACKAGE = "base") $code
    ## NB this relies on passing NAOK in that position!
    if(codes.only) code
    else factor(code, seq(labels), labels)
}
