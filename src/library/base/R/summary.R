summary <- function (object, ...) UseMethod("summary")

summary.default <-
    function(object, ..., digits = max(3, getOption("digits") - 3))
{
    if(is.factor(object))
	return(summary.factor(object, ...))
    else if(is.matrix(object))
	return(summary.matrix(object, digits = digits, ...))

    value <- if(is.numeric(object)) {
	nas <- is.na(object)
	object <- object[!nas]
	qq <- quantile(object)
	qq <- signif(c(qq[1:3], mean(object), qq[4:5]), digits)
	names(qq) <- c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max.")
	if(any(nas))
	    c(qq, "NA's" = sum(nas))
	else qq
    } else if(is.recursive(object) && !is.language(object) &&
	      (n <- length(object))) {
	sumry <- array("", c(n, 3), list(names(object),
					 c("Length", "Class", "Mode")))
	ll <- numeric(n)
	for(i in 1:n) {
	    ii <- object[[i]]
	    ll[i] <- length(ii)
	    cls <- class(ii)
	    sumry[i, 2] <- if(length(cls)>0) cls[1] else "-none-"
	    sumry[i, 3] <- mode(ii)
	}
	sumry[, 1] <- format(as.integer(ll))
	sumry
    }
    else c(Length= length(object), Class= class(object), Mode= mode(object))
    class(value) <- "table"
    value
}

summary.factor <- function(object, maxsum = 100, ...)
{
    nas <- is.na(object)
    ll <- levels(object)
    if(any(nas)) maxsum <- maxsum - 1
    tbl <- table(object)
    tt <- c(tbl) # names dropped ...
    names(tt) <- dimnames(tbl)[[1]]
    if(length(ll) > maxsum) {
	drop <- maxsum:length(ll)
	o <- sort.list(tt, decreasing = TRUE)
	tt <- c(tt[o[ - drop]], "(Other)" = sum(tt[o[drop]]))
    }
    if(any(nas)) c(tt, "NA's" = sum(nas)) else tt
}

summary.matrix <- function(object, ...)
    summary.data.frame(data.frame(object), ...)

summary.data.frame <-
    function(object, maxsum = 7, digits = max(3, getOption("digits") - 3), ...)
{
    # compute results to full precision.
    z <- lapply(as.list(object), summary, maxsum = maxsum, digits = 12, ...)
    nv <- length(object)
    nm <- names(object)
    lw <- numeric(nv)
    nr <- max(unlist(lapply(z, length)))
    for(i in 1:nv) {
	sms <- z[[i]]
	lbs <- format(names(sms))
	sms <- paste(lbs, ":", format(sms, digits = digits), "  ", sep = "")
	lw[i] <- nchar(lbs[1])
	length(sms) <- nr
	z[[i]] <- sms
    }
    z <- unlist(z, use.names=FALSE)
    dim(z) <- c(nr, nv)
    blanks <- paste(character(max(lw) + 2), collapse = " ")
    pad <- floor(lw-nchar(nm)/2)
    nm <- paste(substring(blanks, 1, pad), nm, sep = "")
    dimnames(z) <- list(rep("", nr), nm)
    attr(z, "class") <- c("table") #, "matrix")
    z
}
