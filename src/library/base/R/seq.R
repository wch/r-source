seq <- function(x, ...) UseMethod("seq")

seq.default <-
function(from = 1, to = 1, by = ((to - from)/(length.out - 1)),
         length.out = NULL, along.with = NULL) {
	if(!missing(along.with))
		length.out <- length(along.with)
	else if(!missing(length.out))
		length.out <- ceiling(length.out)

	if(nargs() == 1 && !missing(from)) {
		if(mode(from) == "numeric" && length(from) == 1)
			1:from
		else seq(along.with = from)
	}
	else if(is.null(length.out))
		if(missing(by))
			from:to
		else {
			n <- (del <- to - from)/by
			if(is.na(n)) {
				if(by == 0 && del == 0)
					return(from)
				else stop("invalid (to - from)/by in seq(.)")
			} else if(abs(n) > .Machine$integer.max)
				stop("'by' argument is much too small")
			else if(n < 0)
				stop("Wrong sign in by= argument")

			eps <- .Machine$double.eps *
				max(1, max(abs(to),abs(from)) / abs(del))
			n <- as.integer(n * (1 + eps))
			if(eps*2*n >= 1)
				warning(paste("seq.default(f,t,by): n=",n,
					      ": possibly imprecise intervals"))
			if(by>0) while(from+ n*by > to) n <- n - 1
			else	 while(from+ n*by < to) n <- n - 1

			from + (0:n) * by
		}
	else if(length.out < 0)
		stop("Length cannot be negative")
	else if(length.out == 0)
		integer(0)
	else if(missing(by)) {
		if(from == to || length.out < 2)
			by <- 1
		if(missing(to))
			to <- from + length.out - 1
		if(missing(from))
			from <- to - length.out + 1
		if(length.out > 2)
			if(from == to)
				rep(from, length.out)
			else as.vector(c(from, from + (1:(length.out - 2)) *
					by, to))
		else as.vector(c(from, to))[1:length.out]
	}
	else if(missing(to))
		from + (0:(length.out - 1)) * by
	else if(missing(from))
		to - ((length.out - 1):0) * by
	else stop("Too many arguments")
}

sequence <- function(nvec)
{
	sequence <- NULL
	for(i in nvec) sequence<-c(sequence,seq(1:i))
	return(sequence)
}
