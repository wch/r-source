cut <- function(x, ...) UseMethod("cut")

cut.default <- function (x, breaks, labels=NULL, include.lowest = FALSE,
			 right=TRUE, dig.lab=3)
{
    if (!is.numeric(x)) stop("cut: x must be numeric")
    if (length(breaks) == 1) {
	if (is.na(breaks) | breaks < 2)
	    stop("invalid number of intervals")
	nb <- as.integer(breaks + 1)# one more than #{intervals}
	dx <- diff(rx <- range(x,na.rm=TRUE))
	if(dx==0) dx <- rx[1]
	breaks <- seq(rx[1] - dx/1000,
		      rx[2] + dx/1000, len=nb)
    } else nb <- length(breaks <- sort(breaks))
    if (any(duplicated(breaks))) stop("cut: breaks are not unique")
    codes.only <- FALSE
    if (is.null(labels)) {#- try to construct nice ones ..
	for(dig in dig.lab:12) {
	    ch.br <- formatC(breaks, dig=dig, wid=1)
	    if(ok <- all(ch.br[-1]!=ch.br[-nb])) break
	}
	labels <-
	    if(ok) paste(if(right)"(" else "[",
			 ch.br[-nb], ",", ch.br[-1],
			 if(right)"]" else ")", sep='')
	    else paste("Range", 1:(nb - 1),sep="_")
    } else if (is.logical(labels) && !labels)
        codes.only <- TRUE
    else if (length(labels) != nb-1)
        stop("labels/breaks length conflict")
    code <- .C("bincode",
	       x =     	as.double(x),
	       n =	length(x),
	       breaks =	as.double(breaks),
               nb,
	       code= 	integer(length(x)),
               right=	as.logical(right),
	       include= as.logical(include.lowest),
	       NAOK= TRUE, DUP = FALSE) $code
    if(codes.only) code
    else factor(code, seq(labels), labels)
}
