format <- function(x, ...) UseMethod("format")

###	 -----
###----- FIXME ----- the digits handling should rather happen in
###	 -----       in .Internal(format(...))   in  ../../main/paste.c !
###--- also the 'names' should be kept INTERNALLY !
format.default <- function(x, trim=FALSE, digits=NULL)
{
        if(!is.null(digits)) {
                op <- options(digits=digits)
                on.exit(options(op))
        }
        switch(mode(x),
               NULL = "NULL",
               list = sapply(
                 lapply(x, function(x)
                        .Internal(format(unlist(x),trim=trim))),
                 paste, collapse=", "),
               ##else: numeric, complex, character, ??? :
               structure(.Internal(format(x, trim = trim)), names=names(x)))
}

##-- this should also happen in	C(.) :
##	.Internal(format(..) should work  with  'width =' and 'flag=.."
##		at least for the case of character arguments.
format.char <- function(x, width = NULL, flag = "-")
{
	## Purpose: Character formatting
	## --------------------------------------------------------------------
	## Arguments: x: character,  width: of field, flag: if "-" LEFT-justify
	## --------------------------------------------------------------------
	## Author: Martin Maechler <maechler@stat.math.ethz.ch>
	if (is.null(x)) return("")
	if(!is.character(x)) {
		warning("format.char: coercing 'x' to 'character'")
		x <- as.character(x)
	}
	if(is.null(width) && flag == "-")
	  return(format(x))		# Left justified; width= max.width
	## else
	at <- attributes(x)
	nc <- nchar(x)			#-- string lengths
	if(is.null(width)) width <- max(nc)
	else if(width<0) { flag <- "-"; width <- -width }
	pad <- sapply(pmax(0,width - nc),
		      function(no) paste(character(no+1), collapse =" "))
	r <- if(flag=="-") paste(x, pad, sep="")#-- LEFT  justified
		else	   paste(pad, x, sep="")#-- RIGHT justified
	if(!is.null(at)) attributes(r) <- at
	r
}


format.pval <- function(pv, digits = max(1, .Options$digits-2),
			eps = .Machine$double.eps, na.form = "NA")
{
	## Format  P values; auxiliary for print.summary.[g]lm(.)

	if((has.na <- any(ina <- is.na(pv)))) pv <- pv[!ina]
	## Better than '0.0' for very small values `is0':
	r <- character(length(is0 <- pv < eps))
	if(any(!is0)) {
		rr <- pv <- pv[!is0]
		## be smart -- differ for fixp. and expon. display:
		expo <- floor(log10(pv))
		fixp <- expo >= -3 | (expo == -4 & digits>1)
		if(any( fixp)) rr[ fixp] <- format(pv[ fixp], dig=digits)
		if(any(!fixp)) rr[!fixp] <- format(pv[!fixp], dig=digits)
		r[!is0]<- rr
	}
	if(any(is0)) {
		digits <- max(1,digits-2)
		if(any(!is0)) {
			nc <- max(nchar(rr))
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

## Martin Maechler <maechler@stat.math.ethz.ch> , 1994-1998
formatC <- function (x, digits = NULL, width = NULL,
		     format = NULL, flag = "", mode = NULL)
{
        blank.chars <- function(no)
        	sapply(no+1, function(n) paste(character(n), collapse=" "))

        if (!(n <- length(x))) return("")
	if (missing(mode))    mode <- storage.mode(x)
	else if (any(mode == c("double", "real", "integer")))
          storage.mode(x) <- if(mode=="real")"double" else mode
	else stop("\"mode\" must be \"double\" (\"real\") or \"integer\"")
	if (mode == "character" || (!is.null(format) && format == "s")) {
	 if (mode != "character") {
	  warning('should give "character" argument for format="s" -- COERCING')
	  x <- as.character(x)
	 }
	 return(format.char(x, width=width, flag=flag))
	}
	some.special <- !all(Ok <- is.finite(x))
	if (some.special) {
		rQ <- as.character(x[!Ok])
		x[!Ok] <- 0
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
	 else stop('"format" must be in {"f","e","E","g","G", "fg", "s"}')
	}
	if (missing(digits) || is.null(digits))
	  digits <- if (mode == "integer") 2 else 4
        else if(digits<0)
          digits <- 6
	if(is.null(width)) width <- digits + 1
	else if (width == 0) width <- digits##was stop("`width' must not be 0")
        i.strlen <-
          pmax(abs(width),
               if(format == "fg"||format == "f") {
                 xEx <- as.integer(floor(log10(abs(x+ifelse(x==0,1,0)))))
                 as.integer(x < 0 | flag!="") + digits +
                   if(format == "f") {
                     2 + pmax(xEx,0)
                   } else {# format == "fg"
                     pmax(xEx, digits,digits+(-xEx)+1) +
                       ifelse(flag!="",nchar(flag),0) + 1
                   }
               } else # format == "g" or "e":
               rep(digits+8, n)
               )
        ##Dbg if(format=="fg"||format == "f")
        ##Dbg   cat("formatC(,.): xEx=",xEx,"\n\t==> i.strlen=",i.strlen,"\n")
	r <- .C("str_signif",
		x = x,
		n = n,
		mode   = as.character(mode),
		width  = as.integer(width),
		digits = as.integer(digits),
		format = as.character(format),
		flag   = as.character(flag),
		result = blank.chars(i.strlen))$result
        ##Dbg if(any(ii <- (nc.res <- nchar(r)) > i.strlen)) {
        ##Dbg  cat("formatC: some  i.strlen[.] were too small:\n")
        ##Dbg  print(cbind(ii=which(ii), strlen=i.strlen[ii], nchar=nc.res[ii]))
        ##Dbg }
	if (some.special)
	  r[!Ok] <- format.char(rQ, width=width, flag=flag)
	if (!is.null(x.atr <- attributes(x)))
	  attributes(r) <- x.atr
	r
}
