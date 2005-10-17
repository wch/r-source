format <- function(x, ...) UseMethod("format")

format.default <-
    function(x, trim = FALSE, digits = NULL, nsmall = 0,
             justify = c("left", "right", "centre", "none"),
             width = NULL, na.encode = TRUE, scientific = NA,
             big.mark = "", big.interval = 3,
             small.mark = "", small.interval = 5, decimal.mark = ".",
             ...)
{
    justify <- match.arg(justify)
    adj <- match(justify, c("left", "right", "centre", "none")) - 1
    if(is.list(x)) {
        ## do it this way to force evaluation of args
        if(missing(trim)) trim <- TRUE
        if(missing(justify)) justify <- "none"
        res <- lapply(x, FUN=function(xx, ...) format.default(unlist(xx),...),
                      trim = trim, digits = digits, nsmall = nsmall,
                      justify = justify, width = width, na.encode = na.encode,
                      scientific = scientific,
                      big.mark = big.mark, big.interval = big.interval,
                      small.mark = small.mark, small.interval = small.interval,
                      decimal.mark = decimal.mark, ...)
        sapply(res, paste, collapse = ", ")
    } else {
        switch(mode(x),
               NULL = "NULL",
               character = .Internal(format(x, trim, digits, nsmall, width,
                                            adj, na.encode, scientific)),
               call=, expression=, "function"=, "(" = deparse(x),
               raw = as.character(x),
               ## else: logical, numeric, complex, .. :
               prettyNum(.Internal(format(x, trim, digits, nsmall, width,
                                          3, na.encode, scientific)),
                         big.mark = big.mark, big.interval = big.interval,
                         small.mark = small.mark,
                         small.interval = small.interval,
                         decimal.mark = decimal.mark)
               )
    }
}

format.pval <- function(pv, digits = max(1, getOption("digits")-2),
			eps = .Machine$double.eps, na.form = "NA")
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
	if(any( fixp)) rr[ fixp] <- format(pv[ fixp], dig=digits)
	if(any(!fixp)) rr[!fixp] <- format(pv[!fixp], dig=digits)
	r[!is0]<- rr
    }
    if(any(is0)) {
	digits <- max(1,digits-2)
	if(any(!is0)) {
	    nc <- max(nchar(rr, type="w"))
	    if(digits > 1 && digits+6 > nc)
		digits <- max(1, nc - 7)
	    sep <- if(digits==1 && nc <= 6) "" else " "
	} else sep <- if(digits==1) "" else " "
	r[is0] <- paste("<", format(eps, digits=digits), sep = sep)
    }
    if(has.na) { ## rarely...
	rok <- r
	r <- character(length(ina))
	r[!ina] <- rok
	r[ina] <- na.form
    }
    r
}

## Martin Maechler <maechler@stat.math.ethz.ch> , 1994-1998 :
formatC <- function (x, digits = NULL, width = NULL,
		     format = NULL, flag = "", mode = NULL,
                     big.mark = "", big.interval = 3,
                     small.mark = "", small.interval = 5,
                     decimal.mark = ".")
{
    format.char <- function (x, width, flag)
    {
        if(is.null(width)) width <- 0
        else if(width < 0) { flag <- "-"; width <- -width }
        format.default(x, width=width,
                       justify = if(flag=="-") "left" else "right")
    }
    blank.chars <- function(no)
	sapply(no+1, function(n) paste(character(n), collapse=" "))

    if (!(n <- length(x))) return("")
    if (is.null(mode))	  mode <- storage.mode(x)
    else if (any(mode == c("double", "real", "integer")))  {
      ## for .C call later on
	if(mode=="real") mode <- "double"
	storage.mode(x) <- mode
    }
    else stop("'mode\' must be \"double\" (\"real\") or \"integer\"")
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
	width <- 1
    if (is.null(digits))
	digits <- if (mode == "integer") 2 else 4
    else if(digits < 0)
	digits <- 6
    if(is.null(width))	width <- digits + 1
    else if (width == 0)width <- digits
    i.strlen <-
	pmax(abs(width),
	     if(format == "fg"||format == "f") {
		 xEx <- as.integer(floor(log10(abs(x+ifelse(x==0,1,0)))))
		 as.integer(x < 0 | flag!="") + digits +
		     if(format == "f") {
			 2 + pmax(xEx,0)
		     } else {# format == "fg"
			 pmax(xEx, digits,digits+(-xEx)+1) +
			     ifelse(flag != "", nchar(flag), 0) + 1
		     }
	     } else # format == "g" or "e":
	     rep.int(digits+8, n)
	     )
    ## sanity check for flags added 2.1.0
    flag <- as.character(flag)
    nf <- strsplit(flag, "")[[1]]
    if(!all(nf %in% c("0", "+", "-", " ", "#")))
       stop("'flag' can contain only '0+- #'")
    attr(x, "Csingle") <- NULL  # avoid intepreting as.single
    r <- .C("str_signif",
	    x = x,
	    n = n,
	    mode   = as.character(mode),
	    width  = as.integer(width),
	    digits = as.integer(digits),
	    format = as.character(format),
	    flag   = as.character(flag),
	    result = blank.chars(i.strlen),
	    PACKAGE = "base")$result
    if (some.special)
	r[!Ok] <- format.char(rQ, width=width, flag=flag)

    if(big.mark != "" || small.mark != "" || decimal.mark != ".")
        r <- prettyNum(r, big.mark = big.mark, big.interval = big.interval,
                       small.mark = small.mark, small.interval = small.interval,
                       decimal.mark = decimal.mark)

    if (!is.null(x.atr <- attributes(x)))
	attributes(r) <- x.atr
    r
}

format.factor <- function(x, ...)
    format(as.character(x), ...)

format.data.frame <- function(x, ..., justify = "none")
{
    dims <- dim(x)
    nr <- dims[1]
    nc <- dims[2]
    rval <- vector("list", nc)
    for(i in 1:nc)
	rval[[i]] <- format(x[[i]], ..., justify = justify)
    lens <- sapply(rval, NROW)
    if(any(lens != nr)) { # corrupt data frame, must have at least one column
        warning("corrupt data frame: columns will be truncated or padded with NAs")
        for(i in 1:nc) {
            len <- NROW(rval[[i]])
            if(len == nr) next
            if(length(dim(rval[[i]])) == 2) {
                rval[[i]] <- if(len < nr)
                    rbind(rval[[i]], matrix(NA, nr-len, ncol(rval[[i]])))
                else rval[[i]][1:nr,]
            } else {
                rval[[i]] <- if(len < nr) c(rval[[i]], rep.int(NA, nr-len))
                else rval[[i]][1:nr]
            }
        }
    }
    for(i in 1:nc) {
        if(is.character(rval[[i]]) && class(rval[[i]]) == "character")
            oldClass(rval[[i]]) <- "AsIs"
    }
    dn <- dimnames(x)
    cn <- dn[[2]]
    m <- match(c("row.names", "check.rows", "check.names"), cn, 0)
    if(any(m > 0)) cn[m] <- paste("..dfd.", cn[m], sep="")
    names(rval) <- cn
    rval$check.names <- FALSE
    rval$row.names <- dn[[1]]
    x <- do.call("data.frame", rval)
    ## x will have more cols than rval if there are matrix/data.frame cols
    if(any(m > 0)) names(x) <- sub("^..dfd.", "", names(x))
    x
}

format.AsIs <- function(x, width = 12, ...)
{
    if(is.character(x)) return(format.default(x, ...))
    if(is.null(width)) width = 12
    n <- length(x)
    rvec <- rep.int(as.character(NA), n)
    for(i in 1:n) rvec[i] <- toString(x[[i]], width = width, ...)
    ## AsIs might be around a matrix, which is not a class.
    dim(rvec) <- dim(x)
    format.default(rvec, justify = "right")
}

prettyNum <-
    function(x,
             big.mark = "", big.interval = 3,
             small.mark = "", small.interval = 5,
             decimal.mark = ".", ...)
{
    ## be fast in trivial case:
    if(!is.character(x))
        x <- sapply(x, format, ...)
    if(big.mark == "" && small.mark == "" && decimal.mark == ".")
        return(x)
    ## else
    x.sp <- strsplit(x, ".", fixed=TRUE)
    P0 <- function(...) paste(..., sep="")
    revStr <- function(cc)
        sapply(lapply(strsplit(cc,NULL), rev), paste, collapse="")
    B. <- sapply(x.sp, "[", 1)      # Before "."
    A. <- sapply(x.sp, "[", 2)      # After  "." ; empty == NA
    if(any(iN <- is.na(A.))) A.[iN] <- ""
    if(nchar(big.mark) &&
       length(i.big <- grep(P0("[0-9]{", big.interval + 1,",}"), B.))
       ) { ## add `big.mark' in decimals before "." :
        B.[i.big] <-
            revStr(gsub(P0("([0-9]{",big.interval,"})\\B"),
                        P0("\\1",big.mark), revStr(B.[i.big])))
    }
    if(nchar(small.mark) &&
       length(i.sml <- grep(P0("[0-9]{", small.interval + 1,",}"), A.))
       ) { ## add `small.mark' in decimals after "." :
        A.[i.sml] <- gsub(P0("([0-9]{",small.interval,"})"),
                          P0("\\1",small.mark), A.[i.sml])
    }
    ## extraneous trailing dec.marks: paste(B., A., sep = decimal.mark)
    P0(B., c(decimal.mark, "")[iN+ 1:1], A.)
}
