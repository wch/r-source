"apply"<-
function(X, MARGIN, FUN, ...)
{
	# ENSURE THAT FUN IS A FUNCTION

	if(is.character(FUN))
		FUN <- get(FUN, mode = "function")
	else if(mode(FUN) != "function") {
		f <- substitute(FUN)
		if(is.name(f))
			FUN <- get(as.character(f), mode = "function")
		else stop(paste("\"", f, "\" is not a function", sep = ""))
	}

	# ENSURE THAT X IS AN ARRAY OBJECT

	d <- dim(X)
	dl <- length(d)
	ds <- 1:length(d)
	if(dl == 0)
		stop("dim(X) must have a positive length")
	if(length(class(X)) > 0)
		X <- if(dl == 2) as.matrix(X) else as.array(X)
	dn <- dimnames(X)

	# EXTRACT THE MARGINS AND ASSOCIATED DIMNAMES

	s.call <- (1:length(d))[-MARGIN]
	s.ans <- (1:length(d))[MARGIN]
	d.call <- d[-MARGIN]
	d.ans <- d[MARGIN]
	dn.call <- dn[-MARGIN]
	dn.ans <- dn[MARGIN]
	# dimnames(X) <- NULL

	# DO THE CALLS

	newX <- aperm(X, c(s.call, s.ans))
	dim(newX) <- c(prod(d.call), prod(d.ans))
	d2 <- dim(newX)[2]
	ans <- vector("list", d2)
	for(i in 1:d2)
		ans[[i]] <- FUN(array(newX[,i], d.call, dn.call), ...)

	# ANSWER DIMS AND DIMNAMES

	ans.names <- names(ans[[1]])
	ans.list <- is.recursive(ans[[1]])
	ans.length <- length(ans[[1]])
	if(!ans.list)
		ans.list <- any(unlist(lapply(ans, length)) != ans.length)
	if(!ans.list)
		ans <- unlist(ans, recursive = F)
	if(length(MARGIN) == 1 && length(ans) == d2) {
		if(length(dn.ans[[1]]) > 0)
			names(ans) <- dn.ans[[1]]
		else names(ans) <- NULL
		return(ans)
	}
	else if(length(ans) == d2)
		return(array(ans, d.ans, dn.ans))
	else if(length(ans) > 0 && length(ans) %% d2 == 0) {
		if(is.null(dn.ans))
			return(array(ans, c(length(ans)/d2, d[MARGIN])))
		else return(array(ans, c(length(ans)/d2, d.ans),
				c(list(ans.names), dn.ans)))
	}
	else return(ans)
}
