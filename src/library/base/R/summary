summary <-
function (x, ...)
	UseMethod("summary")

summary.default <-
function(object, ..., digits = max(options()$digits - 3, 3))
{
	if(is.factor(object))
		return(summary.factor(object, ...))
	else if(is.matrix(object))
		return(summary.matrix(object, ...))
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
		class(sumry) <- "table"
		sumry
	}
	else c(Length = length(object), Class = class(object), Mode = mode(object))
	class(value) <- "table"
	value
}

summary.factor <-
function(x, maxsum = 100, ...)
{
	nas <- is.na(x)
	ll <- levels(x)
	if(any(nas)) maxsum <- maxsum - 1
	tbl <- table(x)
	tt <- c(tbl) # names dropped ...
	names(tt) <- dimnames(tbl)[[1]]
	if(length(ll) > maxsum) {
		drop <- maxsum:length(ll)
		o <- rev(order(tt))
		tt <- c(tt[o[ - drop]], "(Other)" = sum(tt[o[drop]]))
	}
	if(any(nas)) c(tt, "NA's" = sum(nas)) else tt
}

summary.matrix <-
function(x, ...)
	summary.data.frame(data.frame(x))

summary.data.frame <-
function(x, maxsum = 7, ...)
{
	z <- lapply(as.list(x), summary, maxsum = maxsum)
	nv <- length(x)
	nm <- names(x)
	lw <- numeric(nv)
	nr <- max(unlist(lapply(z, length)))
	for(i in 1:nv) {
		sms <- z[[i]]
		lbs <- format(names(sms))
		sms <- paste(lbs, ":", format(sms), "  ", sep = "")
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

print.table <-
function(x, digits= .Options$digits, quote = FALSE, na.print='', ...)
{
 print.default(unclass(x), digits=digits, quote=quote, na.print=na.print, ...)
}
