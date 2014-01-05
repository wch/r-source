#  File src/library/graphics/R/hist.R
#  Part of the R package, http://www.R-project.org
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
#  http://www.r-project.org/Licenses/

hist <- function(x, ...) UseMethod("hist")

hist.default <-
    function (x, breaks = "Sturges", freq = NULL,
	      probability = !freq, include.lowest= TRUE,
	      right = TRUE, density = NULL, angle = 45,
	      col = NULL, border = NULL,
	      main = paste("Histogram of" , xname),
	      xlim = range(breaks), ylim = NULL,
	      xlab = xname, ylab,
	      axes = TRUE, plot = TRUE, labels = FALSE, nclass = NULL,
	      warn.unused = TRUE, ...)
{
    if (!is.numeric(x))
	stop("'x' must be numeric")
    xname <- paste(deparse(substitute(x), 500), collapse="\n")
    n <- length(x <- x[is.finite(x)])
    n <- as.integer(n)
    if(is.na(n)) stop("invalid length(x)")
    use.br <- !missing(breaks)
    if(use.br) {
	if(!missing(nclass))
	    warning("'nclass' not used when 'breaks' is specified")
    }
    else if(!is.null(nclass) && length(nclass) == 1L)
	breaks <- nclass
    use.br <- use.br && (nB <- length(breaks)) > 1L
    if(use.br)
	breaks <- sort(breaks)
    else {				# construct vector of breaks
	if(!include.lowest) {
	    include.lowest <- TRUE
	    warning("'include.lowest' ignored as 'breaks' is not a vector")
	}
	if(is.character(breaks)) {
	    breaks <- match.arg(tolower(breaks),
				c("sturges", "fd",
				  "freedman-diaconis", "scott"))
	    breaks <- switch(breaks,
			     sturges = nclass.Sturges(x),
			     "freedman-diaconis" =,
			     fd = nclass.FD(x),
			     scott = nclass.scott(x),
			     stop("unknown 'breaks' algorithm"))
	} else if(is.function(breaks)) {
	    breaks <- breaks(x)
	}
        ## if(!is.numeric(breaks) || !is.finite(breaks) || breaks < 1L)
        ##     stop("invalid number of 'breaks'")
        ## breaks <- pretty (range(x), n = breaks, min.n = 1)
        ## nB <- length(breaks)
        ## if(nB <= 1) ##-- Impossible !
        ##     stop(gettextf("hist.default: pretty() error, breaks=%s",
        ##                   format(breaks)), domain = NA)
        if (length(breaks) == 1) {
            if(!is.numeric(breaks) || !is.finite(breaks) || breaks < 1L)
                stop("invalid number of 'breaks'")
            breaks <- pretty (range(x), n = breaks, min.n = 1)
            nB <- length(breaks)
            if(nB <= 1) ##-- Impossible !
                stop(gettextf("hist.default: pretty() error, breaks=%s",
                              format(breaks)), domain = NA)
        }
        else {
            if(!is.numeric(breaks) || length(breaks) <= 1)
                stop(gettextf("Invalid breakpoints produced by 'breaks(x)': %s",
                              format(breaks)), domain = NA)
            breaks <- sort(breaks)
            nB <- length(breaks)
            use.br <- TRUE # To allow equidist=FALSE below (FIXME: Find better way?)
        }
    }
    nB <- as.integer(nB)
    if(is.na(nB)) stop("invalid length(breaks)")

    ## Do this *before* adding fuzz or logic breaks down...

    h <- diff(breaks)
    equidist <- !use.br || diff(range(h)) < 1e-7 * mean(h)
    if (!use.br && any(h <= 0))
	stop("'breaks' are not strictly increasing")
    freq1 <- freq # we want to do missing(freq) later
    if (is.null(freq)) {
	freq1 <- if(!missing(probability)) !as.logical(probability) else equidist
    } else if(!missing(probability) && any(probability == freq))
	stop("'probability' is an alias for '!freq', however they differ.")

    ## Fuzz to handle cases where points are "effectively on"
    ## the boundaries
    ## As one break point could be very much larger than the others,
    ## as from 1.9.1 we no longer use the range. (PR#6931)
    ## diddle <- 1e-7 * max(abs(range(breaks)))
    diddle <- 1e-7 * stats::median(diff(breaks))
    fuzz <- if(right)
	c(if(include.lowest) - diddle else diddle,
          rep.int(diddle, length(breaks) - 1))
    else
	c(rep.int(-diddle, length(breaks) - 1),
          if(include.lowest) diddle else -diddle)

    fuzzybreaks <- breaks + fuzz
    h <- diff(fuzzybreaks)

    ## With the fuzz adjustment above, the "right" and "include"
    ## arguments are often irrelevant (but not with integer data!)
    counts <- .Call(C_BinCount, x, fuzzybreaks, right, include.lowest)
    if (any(counts < 0L))
	stop("negative 'counts'. Internal Error.", domain = NA)
    if (sum(counts) < n)
	stop("some 'x' not counted; maybe 'breaks' do not span range of 'x'")
    dens <- counts/(n*diff(breaks)) # use un-fuzzed intervals
    mids <- 0.5 * (breaks[-1L] + breaks[-nB])
    r <- structure(list(breaks = breaks, counts = counts,
			density = dens, mids = mids,
			xname = xname, equidist = equidist),
		   class = "histogram")
    if (plot) {
	plot(r, freq = freq1, col = col, border = border,
	     angle = angle, density = density,
	     main = main, xlim = xlim, ylim = ylim, xlab = xlab, ylab = ylab,
	     axes = axes, labels = labels, ...)
	invisible(r)
    }
    else { ## plot is FALSE
    	if (warn.unused) {
	    ## make an effort to warn about "non sensical" arguments:
	    nf <- names(formals()) ## all formals but those:
	    nf <- nf[is.na(match(nf, c("x", "breaks", "nclass", "plot",
				       "include.lowest", "right")))]
	    missE <- lapply(nf, function(n)
			    substitute(missing(.), list(. = as.name(n))))
	    not.miss <- ! sapply(missE, eval, envir = environment())
	    if(any(not.miss))
		warning(sprintf(ngettext(sum(not.miss),
					 "argument %s is not made use of",
					 "arguments %s are not made use of"),
				paste(sQuote(nf[not.miss]), collapse=", ")),
			domain = NA)
	}
        r
    }
}

plot.histogram <-
    function (x, freq = equidist, density = NULL, angle = 45,
	      col = NULL, border = par("fg"), lty = NULL,
	      main = paste("Histogram of", paste(x$xname, collapse="\n")),
              sub = NULL,
	      xlab = x$xname, ylab,
	      xlim = range(x$breaks), ylim = NULL,
	      axes = TRUE, labels = FALSE, add = FALSE, ann = TRUE, ...)
{
    equidist <-
	if(is.logical(x$equidist)) x$equidist
	else { h <- diff(x$breaks) ; diff(range(h)) < 1e-7 * mean(h) }
    if(freq && !equidist)
	warning("the AREAS in the plot are wrong -- rather use 'freq = FALSE'")

    y <- if (freq) x$counts else x$density
    nB <- length(x$breaks)
    if(is.null(y) || 0L == nB) stop("'x' is wrongly structured")

    dev.hold(); on.exit(dev.flush())
    if(!add) {
	if(is.null(ylim))
	    ylim <- range(y, 0)
	if (missing(ylab))
	    ylab <- if (!freq) "Density" else "Frequency"
	plot.new()
	plot.window(xlim, ylim, "")	#-> ylim's default from 'y'
	if(ann) title(main = main, sub = sub, xlab = xlab, ylab = ylab, ...)
	if(axes) {
	    axis(1, ...)
	    axis(2, ...)
	}
    }
    rect(x$breaks[-nB], 0, x$breaks[-1L], y,
	 col = col, border = border,
	 angle = angle, density = density, lty = lty)
    if((logl <- is.logical(labels) && labels) || is.character(labels))
	text(x$mids, y,
	     labels = if(logl) {
		 if(freq) x$counts else round(x$density,3)
	     } else labels,
	     adj = c(0.5, -0.5))
    invisible()
}

lines.histogram <- function(x, ...) plot.histogram(x, ..., add = TRUE)

