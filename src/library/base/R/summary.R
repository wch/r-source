#  File src/library/base/R/summary.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2013 The R Core Team
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  https://www.R-project.org/Licenses/

summary <- function (object, ...) UseMethod("summary")

summary.default <-
    function(object, ..., digits = max(3L, getOption("digits") - 3L))
{
    if(is.factor(object))
	return(summary.factor(object, ...))
    else if(is.matrix(object))
	return(summary.matrix(object, digits = digits, ...))

    value <- if(is.logical(object)) # scalar or array!
	c(Mode = "logical",
          {tb <- table(object, exclude = NULL) # incl. NA s
           if(!is.null(n <- dimnames(tb)[[1L]]) && any(iN <- is.na(n)))
               dimnames(tb)[[1L]][iN] <- "NA's"
           tb
           })
    else if(is.numeric(object)) {
	nas <- is.na(object)
	object <- object[!nas]
	qq <- stats::quantile(object)
	qq <- signif(c(qq[1L:3L], mean(object), qq[4L:5L]), digits)
	names(qq) <- c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max.")
	if(any(nas))
	    c(qq, "NA's" = sum(nas))
	else qq
    } else if(is.recursive(object) && !is.language(object) &&
	      (n <- length(object))) { # do not allow long dims
	sumry <- array("", c(n, 3L), list(names(object),
                                          c("Length", "Class", "Mode")))
	ll <- numeric(n)
	for(i in 1L:n) {
	    ii <- object[[i]]
	    ll[i] <- length(ii)
	    cls <- oldClass(ii)
	    sumry[i, 2L] <- if(length(cls)) cls[1L] else "-none-"
	    sumry[i, 3L] <- mode(ii)
	}
	sumry[, 1L] <- format(as.integer(ll))
	sumry
    }
    else c(Length = length(object), Class = class(object), Mode = mode(object))
    class(value) <- c("summaryDefault", "table")
    value
}

format.summaryDefault <- function(x, ...)
{
    xx <- x
    if(is.numeric(x) || is.complex(x)) {
      finite <- is.finite(x)
      xx[finite] <- zapsmall(x[finite])
    }
    class(xx) <- class(x)[-1]
    m <- match("NA's", names(x), 0)
    if(inherits(x, "Date") || inherits(x, "POSIXct")) {
        if(length(a <- attr(x, "NAs")))
            c(format(xx, ...), "NA's" = as.character(a))
        else format(xx)
    } else if(m && !is.character(x))
        xx <- c(format(xx[-m], ...), "NA's" = as.character(xx[m]))
    else format(xx, ...)
}

print.summaryDefault <- function(x, ...)
{
    xx <- x
    if(is.numeric(x) || is.complex(x)) {
      finite <- is.finite(x)
      xx[finite] <- zapsmall(x[finite])
    }
    class(xx) <- class(x)[-1] # for format
    m <- match("NA's", names(xx), 0)
    if(inherits(x, "Date") || inherits(x, "POSIXct")) {
        xx <- if(length(a <- attr(x, "NAs")))
            c(format(xx), "NA's" = as.character(a))
        else format(xx)
        print(xx, ...)
        return(invisible(x))
    } else if(m && !is.character(x))
        xx <- c(format(xx[-m]), "NA's" = as.character(xx[m]))
    print.table(xx, ...)
    invisible(x)
}

summary.factor <- function(object, maxsum = 100, ...)
{
    nas <- is.na(object)
    ll <- levels(object)
    if(any(nas)) maxsum <- maxsum - 1
    tbl <- table(object)
    tt <- c(tbl) # names dropped ...
    names(tt) <- dimnames(tbl)[[1L]]
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

summary.data.frame <-
    function(object, maxsum = 7L, digits = max(3L, getOption("digits") - 3L), ...)
{
    ncw <- function(x) {
        z <- nchar(x, type="w")
        if (any(na <- is.na(z))) {
            # FIXME: can we do better
            z[na] <- nchar(encodeString(z[na]), "b")
        }
        z
    }
    # compute results to full precision.
    z <- lapply(X = as.list(object), FUN = summary,
                maxsum = maxsum, digits = 12L, ...)
    nv <- length(object)
    nm <- names(object)
    lw <- numeric(nv)
    nr <- if (nv) max(unlist(lapply(z, NROW))) else 0
    for(i in seq_len(nv)) {
        sms <- z[[i]]
        if(is.matrix(sms)) {
            ## need to produce a single column, so collapse matrix
            ## across rows
            cn <- paste(nm[i], gsub("^ +", "", colnames(sms), useBytes = TRUE),
                        sep=".")
            tmp <- format(sms)
            if(nrow(sms) < nr)
                tmp <- rbind(tmp, matrix("", nr - nrow(sms), ncol(sms)))
            sms <- apply(tmp, 1L, function(x) paste(x, collapse="  "))
            ## produce a suitable colname: undoing padding
            wid <- sapply(tmp[1L, ], nchar, type="w") # might be NA
            blanks <- paste(character(max(wid)), collapse = " ")
            wcn <- ncw(cn)
            pad0 <- floor((wid - wcn)/2)
            pad1 <- wid - wcn - pad0
            cn <- paste0(substring(blanks, 1L, pad0), cn,
                         substring(blanks, 1L, pad1))
            nm[i] <- paste(cn, collapse="  ")
            z[[i]] <- sms
        } else {
            sms <- format(sms, digits = digits) # may add NAs row
            lbs <- format(names(sms))
            sms <- paste0(lbs, ":", sms, "  ")
            lw[i] <- ncw(lbs[1L])
            length(sms) <- nr
            z[[i]] <- sms
        }
    }
    if (nv) {
	z <- unlist(z, use.names=TRUE)
	dim(z) <- c(nr, nv)
	if(anyNA(lw))
	    warning("probably wrong encoding in names(.) of column ",
		paste(which(is.na(lw)), collapse = ", "))
	blanks <- paste(character(max(lw, na.rm=TRUE) + 2L), collapse = " ")
	pad <- floor(lw - ncw(nm)/2)
	nm <- paste0(substring(blanks, 1, pad), nm)
	dimnames(z) <- list(rep.int("", nr), nm)
    } else {
	z <- character()
	dim(z) <- c(nr, nv)
    }
    attr(z, "class") <- c("table") #, "matrix")
    z
}
