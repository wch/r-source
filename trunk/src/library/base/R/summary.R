summary <- function (object, ...) UseMethod("summary")

summary.default <-
    function(object, ..., digits = max(3, getOption("digits") - 3))
{
    if(is.factor(object))
	return(summary.factor(object, ...))
    else if(is.matrix(object))
	return(summary.matrix(object, digits = digits, ...))

    value <- if(is.logical(object))# scalar or array!
	c(Mode = "logical",
          {tb <- table(object, exclude=NULL)# incl. NA s
           if(!is.null(n <- dimnames(tb)[[1]]) && any(iN <- is.na(n)))
               dimnames(tb)[[1]][iN] <- "NA's"
           tb
           })
    else if(is.numeric(object)) {
	nas <- is.na(object)
	object <- object[!nas]
	qq <- stats::quantile(object)
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
	    cls <- oldClass(ii)
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

summary.matrix <- function(object, ...) {
    ## we do want this changed into separate columns, so use matrix method
    summary.data.frame(as.data.frame.matrix(object), ...)
}

## <FIXME> use encodeString here, and its justify options
summary.data.frame <-
    function(object, maxsum = 7, digits = max(3, getOption("digits") - 3), ...)
{
    # compute results to full precision.
    z <- lapply(as.list(object), summary, maxsum = maxsum, digits = 12, ...)
    nv <- length(object)
    nm <- names(object)
    lw <- numeric(nv)
    nr <- max(unlist(lapply(z, NROW)))
    for(i in 1:nv) {
        sms <- z[[i]]
        if(is.matrix(sms)) {
            ## need to produce a single column, so collapse matrix
            ## across rows
            cn <- paste(nm[i], gsub("^ +", "", colnames(sms)), sep=".")
            tmp <- format(sms)
            if(nrow(sms) < nr)
                tmp <- rbind(tmp, matrix("", nr - nrow(sms), ncol(sms)))
            sms <- apply(tmp, 1, function(x) paste(x, collapse="  "))
            ## produce a suitable colname: undoing padding
            wid <- sapply(tmp[1,], nchar, type="w")
            blanks <- paste(character(max(wid)), collapse = " ")
            pad0 <- floor((wid-nchar(cn, type="w"))/2)
            pad1 <- wid - nchar(cn, type="w") - pad0
            cn <- paste(substring(blanks, 1, pad0), cn,
                        substring(blanks, 1, pad1), sep = "")
            nm[i] <- paste(cn, collapse="  ")
            z[[i]] <- sms
        } else {
            lbs <- format(names(sms))
            sms <- paste(lbs, ":", format(sms, digits = digits), "  ",
                         sep = "")
            lw[i] <- nchar(lbs[1], type="w")
            length(sms) <- nr
            z[[i]] <- sms
        }
    }
    z <- unlist(z, use.names=TRUE)
    dim(z) <- c(nr, nv)
    blanks <- paste(character(max(lw) + 2), collapse = " ")
    pad <- floor(lw-nchar(nm, type="w")/2)
    nm <- paste(substring(blanks, 1, pad), nm, sep = "")
    dimnames(z) <- list(rep.int("", nr), nm)
    attr(z, "class") <- c("table") #, "matrix")
    z
}
