#  File src/library/base/R/format.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2019 The R Core Team
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

format <- function(x, ...) UseMethod("format")

format.default <-
    function(x, trim = FALSE, digits = NULL, nsmall = 0L,
	     justify = c("left", "right", "centre", "none"),
	     width = NULL, na.encode = TRUE, scientific = NA,
	     big.mark = "", big.interval = 3L,
	     small.mark = "", small.interval = 5L,
	     decimal.mark = getOption("OutDec"),
	     zero.print = NULL, drop0trailing = FALSE, ...)
{
    justify <- match.arg(justify)
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
	       character = {
		   adj <- match(justify, c("left", "right", "centre", "none")) - 1L
		   .Internal(format(x, trim, digits, nsmall, width, adj,
				    na.encode, scientific, NA_character_))
	       },
	       call =, expression =, "function" =, "(" = deparse(x, backtick=TRUE),
	       raw = as.character(x),
	       S4 = {
		   cld <- methods::getClassDef(cl <- class(x))
		   pkg <- attr(cl, "package")
		   paste0("<S4 class ", sQuote(cl),
			  if(!is.null(pkg)) paste0(" [package ", dQuote(pkg), "]"),
			  if(!is.null(cld) && !is.null(sls <- cld@slots))
			      paste(" with", length(sls),
				    if(length(sls) == 1L) "slot" else "slots"), ">")
	       },
	       numeric =, logical =, complex =,
	       environment =
		   prettyNum(.Internal(format(x, trim, digits, nsmall, width, 3L,
					      na.encode, scientific, decimal.mark)),
			     big.mark = big.mark, big.interval = big.interval,
			     small.mark = small.mark,
			     small.interval = small.interval,
			     decimal.mark = decimal.mark, input.d.mark = decimal.mark,
			     zero.print = zero.print, drop0trailing = drop0trailing,
			     is.cmplx = is.complex(x),
			     preserve.width = if (trim) "individual" else "common"),
	       ## all others (for now):
	       stop(gettextf("Found no format() method for class \"%s\"",
			     class(x)), domain = NA))
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
## many corrections by R-core (incl MM).
formatC <- function (x, digits = NULL, width = NULL,
		     format = NULL, flag = "", mode = NULL,
		     big.mark = "", big.interval = 3L,
		     small.mark = "", small.interval = 5L,
                     decimal.mark = getOption("OutDec"),
                     preserve.width = "individual", zero.print = NULL,
		     replace.zero = TRUE,
                     drop0trailing = FALSE)
{
    if(is.object(x)) {
	if(!(is.atomic(x) || inherits(x, "vector")))
	    warning("class of 'x' was discarded")
        x <- unclass(x)
    }
    ## sanity check for flags added 2.1.0
    flag <- as.character(flag)
    if(length(flag) != 1) stop("'flag' must be a string, i.e., of length 1")
    nf <- strsplit(flag, "")[[1L]]
    if(!all(nf %in% c("0", "+", "-", " ", "#", "'", "I")))
	stop("'flag' should contain only characters from [0+- #'I]")

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
	if(mode == "real") mode <- "double"
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
	maxDigits <- if(format != "f") 50L else
	    ceiling(-(.Machine$double.neg.ulp.digits + .Machine$double.min.exp) / log2(10))
	if (digits > maxDigits) {
            warning(gettextf("'digits' reduced to %d", maxDigits), domain = NA)
	    digits <- maxDigits
	}
    }
    if(is.null(width))	width <- digits + 1L
    else if (width == 0L) width <- digits
    i.strlen <-
	pmax(abs(as.integer(width)),
	     if(format == "fg" || format == "f") {
		 xEx <- as.integer(floor(log10(abs(x + (x==0)))))
		 as.integer(x < 0 | flag!="") + digits +
		     if(format == "f") {
			 2L + pmax(xEx, 0L)
		     } else {# format == "fg"
			 1L + pmax(xEx, digits, digits + (-xEx) + 1L) +
			     length(nf) # == nchar(flag, "b")
		     }
	     } else # format == "g" or "e":
		 rep.int(digits + 8L, n)
	     )
    if(digits > 0 && any(nf == "#"))
	digits <- -digits # C-code will notice "do not drop trailing zeros"

    attr(x, "Csingle") <- NULL	# avoid interpreting as.single
    r <- .Internal(formatC(x, as.character(mode), width, digits,
			   as.character(format), flag, i.strlen))
    if (some.special) r[!Ok] <- format.char(rQ, width = width, flag = flag)

    if(nzchar(big.mark) || nzchar(small.mark) || decimal.mark != "." ||
       !is.null(zero.print) || drop0trailing)
	r <- prettyNum(r, big.mark = big.mark, big.interval = big.interval,
		       small.mark = small.mark, small.interval = small.interval,
		       decimal.mark = decimal.mark, input.d.mark = ".",
		       preserve.width = preserve.width, zero.print = zero.print,
		       replace.zero = replace.zero,
		       drop0trailing = drop0trailing, is.cmplx = FALSE)

    if (!is.null(x.atr <- attributes(x)))
	attributes(r) <- x.atr
    r
}

format.factor <- function (x, ...)
    format(structure(as.character(x), names=names(x),
                     dim=dim(x), dimnames=dimnames(x)), ...)


format.data.frame <- function(x, ..., justify = "none")
{
    nc <- length(x)
    if(!nc) return(x) # 0 columns: evade problems, notably for nrow() > 0
    nr <- .row_names_info(x, 2L)
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
    y <- as.data.frame.list(rval,
                            row.names = seq_len(nr),
                            col.names = names(x),
                            optional = TRUE, # <=> check.names = FALSE
                            fix.empty.names = FALSE, cut.names = TRUE)
    attr(y, "row.names") <- row.names(x)
    y
}

format.AsIs <- function(x, width = 12, ...)
{
    if(is.character(x) || (is.atomic(x) && is.matrix(x)))
	return(format.default(x, ...))
    if(is.null(width)) width <- 12L
    rvec <- vapply(x, function(y) {
        ## need to remove class AsIs to avoid an infinite loop.
        cl <- oldClass(y)
        if(m <- match("AsIs", cl, 0L)) oldClass(y) <- cl[-m]
        toString(y, width = width, ...)
    }, "")
    ## AsIs might be around a matrix, which is not a class.
    dim(rvec) <- dim(x)
    dimnames(rvec) <- dimnames(x)
    format.default(rvec, justify = "right")
}

.format.zeros <- function(x, zero.print, nx = suppressWarnings(as.numeric(x)),
                          replace = FALSE, warn.non.fitting = TRUE)
{
    if (!is.null(zero.print) && any(i0 <- nx == 0 & !is.na(nx))) {
	## print zeros according to 'zero.print' (logical or string):
	if(length(zero.print) > 1L) stop("'zero.print' has length > 1")
	if(is.logical(zero.print))
	    zero.print <- if(zero.print) "0" else " "
	if(!is.character(zero.print))
	    stop("'zero.print' must be character, logical or NULL")
	nz <- nchar(zero.print, "c")
	nc <- nchar(x[i0],      "c") # vector
	ind0 <- as.vector(regexpr("0", x[i0], fixed = TRUE))# first '0' in string
	if(replace) {
	    x[i0] <- zero.print
	} else { ## default -- the x[i0] strings should keep their width!
            if(any(nc < nz) && warn.non.fitting)
                warning("'zero.print' is truncated to fit into formatted zeros; consider 'replace=TRUE'")
            i2 <- pmin(nc, nz-1L+ind0)
            i1 <- pmax(1L, i2-nz+1L) # i2 == i1+k  means k+1 chars !
            substr(x[i0], i1, i2) <- zero.print
            if(any(P <- nc > i2))
                substr(x[i0][P], i2[P]+1L, nc[P]) <- strrep(" ", (nc - i2)[P])
        }
    }
    x
}

prettyNum <-
    function(x,
	     big.mark = "", big.interval = 3L,
	     small.mark = "", small.interval = 5L,
             decimal.mark = getOption("OutDec"), input.d.mark = decimal.mark,
	     preserve.width = c("common", "individual", "none"),
	     zero.print = NULL, replace.zero = FALSE,
	     drop0trailing = FALSE, is.cmplx = NA, ...)
{
    if(notChar <- !is.character(x)) {
	is.cmplx <- is.complex(x)
	x <- vapply(x, format, "",
		    big.mark=big.mark, big.interval=big.interval,
		    small.mark=small.mark, small.interval=small.interval,
		    decimal.mark=decimal.mark, zero.print=zero.print,
		    drop0trailing=drop0trailing, ...)
    }
    ## be fast in trivial case, when all options have their default, or "match"
    nMark <- big.mark == "" && small.mark == "" && (notChar || decimal.mark == input.d.mark)

    if (identical(big.mark, decimal.mark))
        warning(gettextf("'big.mark' and 'decimal.mark' are both '%s', which could be confusing",
                         big.mark), domain = NA)

    nZero <- is.null(zero.print) && !drop0trailing
    if(nMark && nZero)
	return(x)

    ## else
    if(nMark && !drop0trailing)# zero.print was only non-default
	return(.format.zeros(x, zero.print, replace=replace.zero))

    ## else
    if(is.na(is.cmplx)) { ## find if 'x' is format from a *complex*
	ina <- is.na(x) | x == "NA"
	is.cmplx <-
	    if(all(ina)) FALSE
	    else any(grepl("[0-9].*[-+][0-9].*i$", x))
    }
    preserve.width <- match.arg(preserve.width)
    if(is.cmplx) {
	## should be rare .. taking an easy route
        ## FIXME - or only at return(.) time ??
        x <- .format.zeros(x, zero.print, replace=replace.zero)
	z.sp <- strsplit(sub("([0-9] *)([-+])( *[0-9])",
			     "\\1::\\2::\\3", x), "::", fixed=TRUE)
	## be careful, if x had an  "	NA":
	i3 <- lengths(z.sp) == 3L # those are re + im *i
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
				  decimal.mark=decimal.mark, input.d.mark=input.d.mark,
				  preserve.width=preserve.width,
				  zero.print=zero.print, replace.zero=replace.zero,
				  drop0trailing=drop0trailing,
				  is.cmplx=FALSE, ...))
	    r[[2]][has.i] <- paste0(r[[2]][has.i], "i")
	    x[i3] <- paste0(r[[1]], vapply(z.sp, `[[`, "", 2L), r[[2]])
	}
	return(x)
    }
    if(nchar(input.d.mark) == 0)
        stop("'input.d.mark' has no characters")
    x.sp <- strsplit(x, input.d.mark, fixed=TRUE)
    ## can have "1.005.987" here, if all *.mark == "."
    if(any(lengths(x.sp) > 2)) { # partly more than two parts
	x.sp <- lapply(x.sp, function(xs) {
	    lx <- length(xs)
	    if(lx <= 2) xs else c(paste(xs[-lx], collapse=input.d.mark), xs[lx])
	})
    }
    B. <- vapply(x.sp, `[`, "", 1L)	# Before input.d.mark (".")
    A. <- vapply(x.sp, `[`, "", 2L)	# After  "." ; empty == NA
    if(any(iN <- is.na(A.))) A.[iN] <- ""

    if(nzchar(big.mark) &&
       length(i.big <- grep(paste0("[0-9]{", big.interval + 1L,",}"), B.))
       ) { ## add 'big.mark' in decimals before "." :
        revStr <- function(cc)
            vapply(lapply(strsplit(cc,NULL), rev), paste, "", collapse="")
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
    A. <- .format.zeros(paste0(B., c(decimal.mark, "")[iN+ 1L], A.),
			zero.print, replace=replace.zero)
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
