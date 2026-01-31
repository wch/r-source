#  File src/library/base/R/summary.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2026 The R Core Team
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

summary.default <- function(object, ..., digits, quantile.type = 7,
                            character.method = c("default", "factor"),
                            polar = TRUE)
{
    if(is.factor(object))
	return(summary.factor(object, ...))
    else if(is.matrix(object)) {
	if(missing(digits))
	    return(summary.matrix(object,                  quantile.type=quantile.type, ...))
	else
	    return(summary.matrix(object, digits = digits, quantile.type=quantile.type, ...))
    }

    value <- if(is.logical(object)) { # scalar or array!
        tb <- table(object, exclude = NULL, useNA = "ifany") # incl. NAs
        if(!is.null(n <- dimnames(tb)[[1L]]) && any(iN <- is.na(n)))
            dimnames(tb)[[1L]][iN] <- "NAs"
        c(Mode = "logical", tb)
    } else if(is.numeric(object) || is.raw(object)) {
        if(isRaw <- is.raw(object)) { # no NAs, no arithmetic -> as.int
            nas <- FALSE
            object <- as.integer(object)
            if(missing(quantile.type)) quantile.type <- 1L
            else if(!(quantile.type %in% c(1L, 3L)))
                warning("quantile.type not in {1, 3} for <raw>")
       } else {
	nas <- is.na(object)
	object <- object[!nas]
       }
	qq <- stats::quantile(object, names = FALSE, type = quantile.type)
        if(isRaw) {
            qq <- as.raw(qq)
        } else {
            qq <- c(qq[1L:3L], mean(object), qq[4L:5L])
            if(!missing(digits)) qq <- signif(qq, digits)
        }
	names(qq) <- c("Min.", "1st Qu.", "Median", if(!isRaw) "Mean", "3rd Qu.", "Max.")
	if(any(nas))
	    c(qq, "NAs" = sum(nas))
        else if(isRaw) c(Mode = "raw", qq)
	else qq
    } else if(is.character(object) && !is.null(character.method)) {
        character.method <- match.arg(character.method)
        if(character.method == "factor")
            return(summary.factor(factor(object), ...))
        nas <- is.na(object)
        object <- object[!nas]
        ncs <- nchar(object, allowNA = TRUE) # NA if "bytes"-encoded
        nna <- sum(nas)
        c(Length    = length(nas),
          N.unique  = length(unique(object)), # NA excluded
          N.blank   = length(grep("^[ \t\r\n]*$", object, perl = TRUE)), # trimws()
          Min.nchar = if(length(ncs)) min(ncs) else NA_integer_,
          Max.nchar = if(length(ncs)) max(ncs) else NA_integer_,
          NAs       = if(nna > 0) nna)
    } else if(is.complex(object)) {
	nas <- is.na(object)
	object <- object[!nas]
	qop <- function(op)
	    stats::quantile(op(object), probs = c(0, 0.5, 1),
	                    names = FALSE, type = quantile.type)
	if(polar) { qq <- c(qop(Mod), qop(Arg)); nm <- c("Mod", "Arg") }
	else      { qq <- c(qop(Re ), qop(Im )); nm <- c("Re" , "Im" ) }
	if(!missing(digits)) qq <- signif(qq, digits)
	names(qq) <- paste0(c("Min.", "Median.", "Max."), rep(nm, each = 3L))
	if(any(nas))
	    c(qq, "NAs" = sum(nas))
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
    } else # very basic/all-purpose summary
        c(Length = length(object), Class = class(object), Mode = mode(object))
    class(value) <- c("summaryDefault", "table")
    value
}

format.summaryDefault <-
function(x, digits = max(3L, getOption("digits") - 3L), zdigits = 4L, ...)
{
    if(is.character(x) || is.integer(x)) { # logical/basic || character summary
        NextMethod("format")
    } else if((iP <- inherits(x, "POSIXct")) || inherits(x, c("Date", "difftime"))) {
        c(NextMethod("format", digits = if(iP) 0L else digits),
          "NAs" = if(length(a <- attr(x, "NAs"))) as.character(a))
    } else { # currently always numeric -> zapsmall
        m <- match("NAs", names(x), 0L)
        nna <- x[m]
        if(m) x <- x[-m]
        finite <- is.finite(x)
        digs <- digits %||% eval(formals()$digits) # use <default> if NULL
        x[finite] <- zapsmall(x[finite], digits = digs + zdigits)
        xx <- NextMethod("format", digits = digits)
        if(m) c(xx, "NAs" = as.character(nna))
        else xx
    }
}

print.summaryDefault <-
function(x, digits = max(3L, getOption("digits") - 3L), zdigits = 4L, ...)
{
    if(is.character(x) || is.integer(x))
        return(NextMethod("print"))

    xx <- if(inherits(x, "difftime")) {
        cat("Time differences in ", attr(x, "units"), "\n", sep = "")
        format(x, digits = digits, with.units = FALSE)
    } else
        format(x, digits = digits, zdigits = zdigits)
    if(inherits(x, c("Date", "POSIXct")))
        print(xx, ...)
    else
        print.noquote(xx, ...)
    invisible(x)
}

summary.factor <- function(object, maxsum = 100L, ...)
{
    nas <- is.na(object)
    ll <- levels(object)
    if(ana <- any(nas)) maxsum <- maxsum - 1L
    tbl <- table(object)
    tt <- c(tbl) # names dropped ...
    names(tt) <- dimnames(tbl)[[1L]]
    if(length(ll) > maxsum) {
	drop <- maxsum:length(ll)
	o <- sort.list(tt, decreasing = TRUE)
	tt <- c(tt[o[ - drop]], "(Other)" = sum(tt[o[drop]]))
    }
    if(ana) c(tt, "NAs" = sum(nas)) else tt
}

summary.matrix <- function(object, ...) {
    ## we do want this changed into separate columns, so use data.frame method
    summary.data.frame(as.data.frame.matrix(object), ...)
}

summary.data.frame <-
    function(object, maxsum = 7L, digits = max(3L, getOption("digits") - 3L), ...)
{
    ncw <- function(x) {
        z <- nchar(x, type="w", allowNA=TRUE)
        if (any(na <- is.na(z))) {
            # FIXME: can we do better
            z[na] <- nchar(encodeString(z[na]), "b")
        }
        z
    }
    # compute results to "full" precision (FIXME: option full.digits = 12L )
    z <- lapply(X = as.list(object), FUN = summary,
                maxsum = maxsum, digits = 12L, ...)
    nv <- length(object)
    nm <- names(object)
    lw <- numeric(nv)
    nr <- if (nv)
	      max(vapply(z, function(x) NROW(x) + !is.null(attr(x, "NAs")), integer(1)))
	  else 0
    for(i in seq_len(nv)) {
        sms <- z[[i]]
        if(is.matrix(sms)) {
            ## need to produce a single column, so collapse matrix
            ## across rows
            cn <- paste(nm[i], gsub("^ +", "", colnames(sms), useBytes = TRUE),
                        sep=".")
	    tmp <- format(sms)# <- digits = ??  --currently take getOption("digits") !!!
            if(nrow(sms) < nr)
                tmp <- rbind(tmp, matrix("", nr - nrow(sms), ncol(sms)))
            sms <- apply(tmp, 1L, function(x) paste(x, collapse="  "))
            ## produce a suitable colname: undoing padding
            wid <- vapply(tmp[1L, ], nchar, 0, type = "w") # might be NA
            blanks <- paste(character(max(wid)), collapse = " ")
            wcn <- ncw(cn)
            pad0 <- floor((wid - wcn)/2)
            pad1 <- wid - wcn - pad0
            cn <- paste0(substring(blanks, 1L, pad0), cn,
                         substring(blanks, 1L, pad1))
            nm[i] <- paste(cn, collapse="  ")
        } else {
            sms <- format(sms, digits = digits) # may add NAs row
            lbs <- format(names(sms))
            sms <- paste0(lbs, ":", sms, "  ")
            lw[i] <- ncw(lbs[1L])
            length(sms) <- nr
        }
	z[[i]] <- sms
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
