#  File src/library/base/R/format.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1995-2015 The R Core Team
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
#  http://www.r-project.org/Licenses/

format <- function(x, ...) UseMethod("format")

format.default <-
    function(x, trim = FALSE, digits = NULL, nsmall = 0L,
	     justify = c("left", "right", "centre", "none"),
	     width = NULL, na.encode = TRUE, scientific = NA,
	     big.mark = "", big.interval = 3L,
	     small.mark = "", small.interval = 5L, decimal.mark = ".",
	     zero.print = NULL, drop0trailing = FALSE, ...)
{
    justify <- match.arg(justify)
    adj <- match(justify, c("left", "right", "centre", "none")) - 1L
    if(is.list(x)) {
	## do it this way to force evaluation of args
	if(missing(trim)) trim <- TRUE
	if(missing(justify)) justify <- "none"
	res <- lapply(X = x,
                      FUN = function(xx, ...) format.default(unlist(xx),...),
		      trim = trim, digits = digits, nsmall = nsmall,
		      justify = justify, width = width, na.encode = na.encode,
		      scientific = scientific,
		      big.mark = big.mark, big.interval = big.interval,
		      small.mark = small.mark, small.interval = small.interval,
		      decimal.mark = decimal.mark, zero.print = zero.print,
		      drop0trailing = drop0trailing, ...)
	vapply(res, paste, "", collapse = ", ")
    } else {
	switch(mode(x),
	       NULL = "NULL",
	       character = .Internal(format(x, trim, digits, nsmall, width,
					    adj, na.encode, scientific)),
	       call =, expression =, "function" =, "("  = deparse(x),
	       raw = as.character(x),
           {
	       ## else: logical, numeric, complex, .. :
	       prettyNum(.Internal(format(x, trim, digits, nsmall, width,
					  3L, na.encode, scientific)),
			 big.mark = big.mark, big.interval = big.interval,
			 small.mark = small.mark,
			 small.interval = small.interval,
			 decimal.mark = decimal.mark,
			 zero.print = zero.print, drop0trailing = drop0trailing,
			 is.cmplx = is.complex(x),
			 preserve.width = if (trim) "individual" else "common")
           })
    }
}

format.pval <- function(pv, digits = max(1L, getOption("digits") - 2L),
			eps = .Machine$double.eps, na.form = "NA", ...)
{
    ## Format  P values; auxiliary for print.summary.[g]lm(.)

    if((has.na <- any(ina <- is.na(pv)))) pv <- pv[!ina]
    ## Better than '0.0' for very small values `is0':
    r <- character(length(is0 <- pv < eps))
    if(any(!is0)) {
	rr <- pv <- pv[!is0]
	## be smart -- differ for fixp. and expon. display:
	expo <- floor(log10(ifelse(pv > 0, pv, 1e-50)))
	fixp <- expo >= -3 | (expo == -4 & digits>1)
	if(any( fixp)) rr[ fixp] <- format(pv[ fixp], digits = digits, ...)
	if(any(!fixp)) rr[!fixp] <- format(pv[!fixp], digits = digits, ...)
	r[!is0] <- rr
    }
    if(any(is0)) {
	digits <- max(1L, digits - 2L)
	if(any(!is0)) {
	    nc <- max(nchar(rr, type="w"))
	    if(digits > 1L && digits + 6L > nc)
		digits <- max(1L, nc - 7L)
	    sep <- if(digits == 1L && nc <= 6L) "" else " "
	} else sep <- if(digits == 1) "" else " "
	r[is0] <- paste("<", format(eps, digits = digits, ...), sep = sep)
    }
    if(has.na) { ## rarely
	rok <- r
	r <- character(length(ina))
	r[!ina] <- rok
	r[ina] <- na.form
    }
    r
}

## Martin Maechler <maechler@stat.math.ethz.ch> , 1994-1998,
## many corrections by R-core.
formatC <- function (x, digits = NULL, width = NULL,
		     format = NULL, flag = "", mode = NULL,
		     big.mark = "", big.interval = 3L,
		     small.mark = "", small.interval = 5L,
                     decimal.mark = getOption("OutDec"),
                     preserve.width = "individual", zero.print = NULL,
                     drop0trailing = FALSE)
{
    if(is.object(x)) {
        x <- unclass(x)
        warning("class of 'x' was discarded")
    }

    format.char <- function (x, width, flag)
    {
	if(is.null(width)) width <- 0L
	else if(width < 0L) { flag <- "-"; width <- -width }
	format.default(x, width=width,
		       justify = if(flag=="-") "left" else "right")
    }

    if (!(n <- length(x))) return("")
    if (is.null(mode))	  mode <- storage.mode(x)
    else if (any(mode == c("double", "real", "integer")))  {
      ## for .C call later on
	if(mode=="real") mode <- "double"
	storage.mode(x) <- mode
    }
    else if (mode != "character")
        stop("'mode' must be \"double\" (\"real\"), \"integer\" or \"character\"")
    if (mode == "character" || (!is.null(format) && format == "s")) {
	if (mode != "character") {
	    warning('coercing argument to "character" for format="s"')
	    x <- as.character(x)
	}
	return(format.char(x, width=width, flag=flag))
    }
    if (missing(format) || is.null(format))
	format <- if (mode == "integer") "d" else "g"
    else {
	if (any(format == c("f", "e", "E", "g", "G", "fg"))) {
	    if (mode == "integer") mode <- storage.mode(x) <- "double"
	}
	else if (format == "d") {
	    if (mode != "integer") mode <- storage.mode(x) <- "integer"
	}
	else stop('\'format\' must be one of {"f","e","E","g","G", "fg", "s"}')
    }
    some.special <- !all(Ok <- is.finite(x))
    if (some.special) {
	rQ <- as.character(x[!Ok])
	rQ[is.na(rQ)] <- "NA"
	x[!Ok] <- as.vector(0, mode = mode)
    }
    if(is.null(width) && is.null(digits))
	width <- 1L
    if (is.null(digits))
	digits <- if (mode == "integer") 2L else 4L
    else if(digits < 0L)
	digits <- 6L
    else {
	maxDigits <- if(format != "f") 50L else ceiling(-(.Machine$double.neg.ulp.digits + .Machine$double.min.exp) / log2(10))
	if (digits > maxDigits) {
            warning(gettextf("'digits' reduced to %d", maxDigits),
                    domain = NA)
	    digits <- maxDigits
	}
    }
    if(is.null(width))	width <- digits + 1L
    else if (width == 0L) width <- digits
    i.strlen <-
	pmax(abs(as.integer(width)),
	     if(format == "fg" || format == "f") {
		 xEx <- as.integer(floor(log10(abs(x+ifelse(x==0,1,0)))))
		 as.integer(x < 0 | flag!="") + digits +
		     if(format == "f") {
			 2L + pmax(xEx, 0L)
		     } else {# format == "fg"
			 pmax(xEx, digits, digits + (-xEx) + 1L) +
			     ifelse(flag != "", nchar(flag, "b"), 0L) + 1L
		     }
	     } else # format == "g" or "e":
	     rep.int(digits + 8L, n)
	     )
    ## sanity check for flags added 2.1.0
    flag <- as.character(flag)
    nf <- strsplit(flag, "")[[1L]]
    if(!all(nf %in% c("0", "+", "-", " ", "#")))
	stop("'flag' can contain only '0+- #'")
    if(digits > 0 && any(nf == "#"))
	digits <- -digits # C-code will notice "do not drop trailing zeros"

    attr(x, "Csingle") <- NULL	# avoid interpreting as.single
    r <- .Internal(formatC(x, as.character(mode), width, digits,
                           as.character(format), as.character(flag),
                           i.strlen))
    if (some.special) r[!Ok] <- format.char(rQ, width = width, flag = flag)

    if(big.mark != "" || small.mark != "" || decimal.mark != "." ||
       !is.null(zero.print) || drop0trailing)
	r <- prettyNum(r, big.mark = big.mark, big.interval = big.interval,
		       small.mark = small.mark, small.interval = small.interval,
		       decimal.mark = decimal.mark, preserve.width = preserve.width,
		       zero.print = zero.print, drop0trailing = drop0trailing,
		       is.cmplx = FALSE)

    if (!is.null(x.atr <- attributes(x)))
	attributes(r) <- x.atr
    r
}

format.factor <- function (x, ...)
    format(structure(as.character(x), names=names(x),
                     dim=dim(x), dimnames=dimnames(x)), ...)


format.data.frame <- function(x, ..., justify = "none")
{
    nr <- .row_names_info(x, 2L)
    nc <- length(x)
    rval <- vector("list", nc)
    for(i in seq_len(nc))
	rval[[i]] <- format(x[[i]], ..., justify = justify)
    lens <- vapply(rval, NROW, 1)
    if(any(lens != nr)) { # corrupt data frame, must have at least one column
	warning("corrupt data frame: columns will be truncated or padded with NAs")
	for(i in seq_len(nc)) {
	    len <- NROW(rval[[i]])
	    if(len == nr) next
	    if(length(dim(rval[[i]])) == 2L) {
		rval[[i]] <- if(len < nr)
		    rbind(rval[[i]], matrix(NA, nr-len, ncol(rval[[i]])))
		else rval[[i]][seq_len(nr),]
	    } else {
		rval[[i]] <- if(len < nr) c(rval[[i]], rep.int(NA, nr-len))
		else rval[[i]][seq_len(nr)]
	    }
	}
    }
    for(i in seq_len(nc)) {
	if(is.character(rval[[i]]) && inherits(rval[[i]], "character"))
	    oldClass(rval[[i]]) <- "AsIs"
    }
    cn <- names(x)
    m <- match(c("row.names", "check.rows", "check.names", ""), cn, 0L)
    if(any(m)) cn[m] <- paste0("..dfd.", cn[m])
    ## This requires valid symbols for the columns, so we need to
    ## truncate any of more than 256 bytes.
    long <- nchar(cn, "bytes") > 256L
    cn[long] <- paste(substr(cn[long], 1L, 250L), "...")
    names(rval) <- cn
    rval$check.names <- FALSE
    rval$row.names <- row.names(x)
    x <- do.call("data.frame", rval)
    ## x will have more cols than rval if there are matrix/data.frame cols
    if(any(m)) names(x) <- sub("^..dfd.", "", names(x))
    x
}

format.AsIs <- function(x, width = 12, ...)
{
    if(is.character(x)) return(format.default(x, ...))
    if(is.null(width)) width = 12L
    n <- length(x)
    rvec <- rep.int(NA_character_, n)
    for(i in seq_len(n)) {
        y <- x[[i]]
        ## need to remove class AsIs to avoid an infinite loop.
        cl <- oldClass(y)
        if(m <- match("AsIs", cl, 0L)) oldClass(y) <- cl[-m]
        rvec[i] <- toString(y, width = width, ...)
    }
    ## AsIs might be around a matrix, which is not a class.
    dim(rvec) <- dim(x)
    dimnames(rvec) <- dimnames(x)
    format.default(rvec, justify = "right")
}

prettyNum <-
    function(x,
	     big.mark = "", big.interval = 3L,
	     small.mark = "", small.interval = 5L,
	     decimal.mark = ".",
	     preserve.width = c("common", "individual", "none"),
	     zero.print = NULL, drop0trailing = FALSE, is.cmplx = NA, ...)
{
    if(!is.character(x)) {
        is.cmplx <- is.complex(x)
	x <- vapply(x, format, "", ...)
    }
    ## be fast in trivial case (when all options have their default):
    nMark <- big.mark== "" && small.mark== "" && decimal.mark== "."

    if (identical(big.mark, decimal.mark))
        warning(gettextf("'big.mark' and 'decimal.mark' are both '%s', which could be confusing",
                         big.mark), domain = NA)

    nZero <- is.null(zero.print) && !drop0trailing
    if(nMark && nZero)
	return(x)

    ## else
    if (!is.null(zero.print) && any(i0 <- {nx <- suppressWarnings(as.numeric(x))
					   nx == 0 & !is.na(nx)})) {
	## print zeros according to 'zero.print' (logical or string):
	if(length(zero.print) > 1L) stop("'zero.print' has length > 1")
	if(is.logical(zero.print))
	    zero.print <- if(zero.print) "0" else " "
	if(!is.character(zero.print))
	    stop("'zero.print' must be character, logical or NULL")
	blank.chars <- function(no) # as in formatC()
	    vapply(no+1L, function(n) paste(character(n), collapse=" "), "")
	nz <- nchar(zero.print, "c")
	nc <- nchar(x[i0], "c")
	ind0 <- regexpr("0", x[i0], fixed = TRUE)# first '0' in string
	substr(x[i0],ind0, (i1 <- ind0+nz-1L)) <- zero.print
	substr(x[i0],ind0+nz, nc) <- blank.chars(nc - i1)
    }
    if(nMark && !drop0trailing)# zero.print was only non-default
	return(x)

    ## else
    if(is.na(is.cmplx)) { ## find if 'x' is format from a *complex*
	ina <- is.na(x) | x == "NA"
	is.cmplx <-
	    if(all(ina)) FALSE
	    else length(grep("[0-9].*[-+][0-9].*i$", x)) > 0
    }
    if(is.cmplx) {
	## should be rare .. taking an easy route
	z.sp <- strsplit(sub("([0-9] *)([-+])( *[0-9])",
			     "\\1::\\2::\\3", x), "::", fixed=TRUE)
	## be careful, if x had an  "	NA":
	i3 <- vapply(z.sp, length, 0L) == 3L # those are re + im *i
	if(any(i3)) {
	    z.sp <- z.sp[i3]
	    z.im <- vapply(z.sp, `[[`, "", 3L)
	    ## drop ending 'i' (and later re-add it)
	    has.i <- grep("i$", z.im)
	    z.im[has.i] <- sub("i$", '', z.im[has.i])
	    r <- lapply(list(vapply(z.sp, `[[`, "", 1L), z.im),
			function(.)
			prettyNum(.,
				  big.mark=big.mark, big.interval=big.interval,
				  small.mark=small.mark, small.interval=small.interval,
				  decimal.mark=decimal.mark, preserve.width=preserve.width,
				  zero.print=zero.print, drop0trailing=drop0trailing,
				  is.cmplx=FALSE, ...))
	    r[[2]][has.i] <- paste0(r[[2]][has.i], "i")
	    x[i3] <- paste0(r[[1]], vapply(z.sp, `[[`, "", 2L), r[[2]])
	}
	return(x)
    }
    preserve.width <- match.arg(preserve.width)
    x.sp <- strsplit(x, ".", fixed=TRUE)
    revStr <- function(cc)
	vapply(lapply(strsplit(cc,NULL), rev), paste, "", collapse="")
    B. <- vapply(x.sp, `[`, "", 1L)	# Before "."
    A. <- vapply(x.sp, `[`, "", 2L)	# After  "." ; empty == NA
    if(any(iN <- is.na(A.))) A.[iN] <- ""

    if(nzchar(big.mark) &&
       length(i.big <- grep(paste0("[0-9]{", big.interval + 1L,",}"), B.))
       ) { ## add 'big.mark' in decimals before "." :
	B.[i.big] <-
	    revStr(gsub(paste0("([0-9]{",big.interval,"})\\B"),
			paste0("\\1",revStr(big.mark)), revStr(B.[i.big])))
    }
    if(nzchar(small.mark) &&
       length(i.sml <- grep(paste0("[0-9]{", small.interval + 1L,",}"), A.))
       ) { ## add 'small.mark' in decimals after "."  -- but *not* trailing
	A.[i.sml] <- gsub(paste0("([0-9]{",small.interval,"}\\B)"),
			  paste0("\\1",small.mark), A.[i.sml])
    }
    if(drop0trailing) {
	a <- A.[!iN]
	if(length(hasE <- grep("e", a, fixed=TRUE))) {
	    a[ hasE] <- sub("e[+-]0+$", '', a[ hasE]) # also drop "e+00"
	    a[-hasE] <- sub("0+$",	'', a[-hasE])
	} else a <- sub("0+$", '', a)
	A.[!iN] <- a
	## iN := TRUE for those A.[]  which are ""
	iN <- !nzchar(A.)
    }
    ## extraneous trailing dec.marks: paste(B., A., sep = decimal.mark)
    A. <- paste0(B., c(decimal.mark, "")[iN+ 1L], A.)
    if(preserve.width != "none") {
	nnc <- nchar(A., "c")
	d.len <- nnc - nchar(x, "c") # extra space added by 'marks' above
	if(any(ii <- d.len > 0L)) {
	    switch(preserve.width,
		   "individual" = {
		       ## drop initial blanks preserving original width
		       ## where possible:
		       A.[ii] <- vapply(which(ii), function(i)
					sub(sprintf("^ {1,%d}", d.len[i]), "",
					    A.[i]), "")
		   },
		   "common" = {
		       A. <- format(A., justify = "right")
		   })
	}
    }
    attributes(A.) <- attributes(x)
    class(A.) <- NULL
    A.
}
