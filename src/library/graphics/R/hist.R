hist <- function(x, ...) UseMethod("hist")

hist.default <-
    function (x, breaks = "Sturges", freq = NULL,
	      probability = !freq, include.lowest= TRUE,
	      right= TRUE, density = NULL, angle = 45,
	      col = NULL, border = NULL,
	      main = paste("Histogram of" , xname),
	      xlim = range(breaks), ylim = NULL,
	      xlab = xname, ylab,
	      axes = TRUE, plot = TRUE, labels = FALSE, nclass = NULL, ...)
{
    if (!is.numeric(x))
	stop("'x' must be numeric")
    xname <- paste(deparse(substitute(x), 500), collapse="\n")
    n <- length(x <- x[is.finite(x)])
    use.br <- !missing(breaks)
    if(use.br) {
	if(!missing(nclass))
	    warning("'nclass' not used when 'breaks' is specified")
    }
    else if(!is.null(nclass) && length(nclass) == 1)
	breaks <- nclass
    use.br <- use.br && (nB <- length(breaks)) > 1
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
			     stop("unknown breaks algorithm"))
	} else if(is.function(breaks)) {
	    breaks <- breaks(x)
	}
	if(!is.numeric(breaks) || is.na(breaks) || breaks < 2)
	    stop("invalid number of breaks")
	breaks <- pretty (range(x), n = breaks, min.n = 1)
	nB <- length(breaks)
	if(nB <= 1) ##-- Impossible !
	    stop("hist.default: pretty() error, breaks=",
		       format(breaks))
    }

    ## Do this *before* adding fuzz or logic breaks down...

    h <- diff(breaks)
    equidist <- !use.br || diff(range(h)) < 1e-7 * mean(h)
    if (!use.br && any(h <= 0))
	stop("'breaks' are not strictly increasing")
    if (is.null(freq)) {
	freq <- if(!missing(probability)) !as.logical(probability) else equidist
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

    storage.mode(x) <- "double"
    storage.mode(fuzzybreaks) <- "double"
    ## With the fuzz adjustment above, the "right" and "include"
    ## arguments are really irrelevant
    counts <- .C("bincount",
		 x,
		 as.integer(n),
		 fuzzybreaks,
		 as.integer(nB),
		 counts = integer(nB - 1),
		 right = as.logical(right),
		 include= as.logical(include.lowest), naok = FALSE,
		 NAOK = FALSE, DUP = FALSE, PACKAGE = "base") $counts
    if (any(counts < 0))
	stop("negative 'counts'. Internal Error in C-code for \"bincount\"")
    if (sum(counts) < n)
	stop("some 'x' not counted; maybe 'breaks' do not span range of 'x'")
    dens <- counts/(n*h)
    mids <- 0.5 * (breaks[-1] + breaks[-nB])
    r <- structure(list(breaks = breaks, counts = counts,
			intensities = dens,
			density = dens, mids = mids,
			xname = xname, equidist = equidist),
		   class="histogram")
    if (plot) {
	plot(r, freq = freq, col = col, border = border,
	     angle = angle, density = density,
	     main = main, xlim = xlim, ylim = ylim, xlab = xlab, ylab = ylab,
	     axes = axes, labels = labels, ...)
	invisible(r)
    }
    else r
}

plot.histogram <-
    function (x, freq = equidist, density = NULL, angle = 45,
	      col = NULL, border = par("fg"), lty = NULL,
	      main = paste("Histogram of", paste(x$xname, collapse="\n")),
              sub = NULL,
	      xlab = x$xname, ylab,
	      xlim = range(x$breaks), ylim = NULL,
	      axes = TRUE, labels = FALSE, add = FALSE, ...)
{
    equidist <-
	if(is.logical(x$equidist)) x$equidist
	else { h <- diff(x$breaks) ; diff(range(h)) < 1e-7 * mean(h) }
    if(freq && !equidist)
	warning("the AREAS in the plot are wrong -- rather use freq=FALSE")

    y <- if (freq) x$counts else { ## x$density -- would be enough, but
	## for back compatibility
	y <- x$density; if(is.null(y)) x$intensities else y}
    nB <- length(x$breaks)
    if(is.null(y) || 0 == nB) stop("'x' is wrongly structured")

    if(!add) {
	if(is.null(ylim))
	    ylim <- range(y, 0)
	if (missing(ylab))
	    ylab <- if (!freq) "Density" else "Frequency"
	plot.new()
	plot.window(xlim, ylim, "")	#-> ylim's default from 'y'
	title(main = main, sub = sub, xlab = xlab, ylab = ylab, ...)
	if(axes) {
	    axis(1, ...)
	    axis(2, ...)
	}
    }
    rect(x$breaks[-nB], 0, x$breaks[-1], y,
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

nclass.Sturges <- function(x) ceiling(log2(length(x)) + 1)

nclass.scott <- function(x)
{
    h <- 3.5 * sqrt(var(x)) * length(x)^(-1/3)
    ceiling(diff(range(x))/h)
}

nclass.FD <- function(x)
{
    r <- as.vector(quantile(x, c(0.25, 0.75)))
    h <- 2 * (r[2] - r[1]) * length(x)^(-1/3)
    ceiling(diff(range(x))/h)
}
