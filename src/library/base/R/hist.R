hist <- function(x, ...) UseMethod("hist")

hist.default <-
    function (x, breaks, freq= NULL, probability = !freq, include.lowest= TRUE,
	      right=TRUE, col = NULL, border = par("fg"),
	      main = paste("Histogram of" , deparse(substitute(x))),
	      xlim = range(breaks), ylim = range(y, 0),
	      xlab = deparse(substitute(x)), ylab,
	      axes = TRUE, plot = TRUE, labels = FALSE, ...)
{
    if (!is.numeric(x))
	stop("hist: x must be numeric")
    main # defeat lazy eval
    xlab
    n <- length(x <- x[!is.na(x)])
    use.br <- !missing(breaks) && (nB <- length(breaks)) > 1
    if(use.br)
	breaks <- sort(breaks)
    else {
	dx <- diff(rx <- range(x))
	nnb <-
	    if(missing(breaks)) 1 + log2(n)
	    else { # breaks = `nclass'
		if (is.na(breaks) | breaks < 2)
		    stop("invalid number of breaks")
		breaks
	    }
	breaks <- pretty (rx, n = nnb, min.n=1, eps.corr = 2)
	nB <- length(breaks)
	if(nB <= 1) { ##-- Impossible !
	    stop(paste("hist.default: pretty() error, breaks=",format(breaks)))
	}
    }
    storage.mode(x) <- "double"
    storage.mode(breaks) <- "double"
    counts <- .C("bincount",
		 x,
		 n,
		 breaks,
		 nB,
		 counts = integer(nB - 1),
		 right	= as.logical(right),
		 include= as.logical(include.lowest),
		 NAOK = FALSE, DUP = FALSE) $counts
    if (any(counts < 0))
	stop("negative `counts'. Internal Error in C-code for \"bincount\"")
    if (sum(counts) < n)
	stop("some `x' not counted; maybe `breaks' do not span range of `x'")
    h <- diff(breaks)
    if (!use.br && any(h <= 0))
	stop("not strictly increasing `breaks'.")
    if (is.null(freq)) {
	freq <- if(!missing(probability))
	    !as.logical(probability)
	else if(use.br) {
	    ##-- Do frequencies if breaks are evenly spaced
	    max(h)-min(h) < 1e-7 * mean(h)
	} else TRUE
    } else if(!missing(probability) && any(probability == freq))
	stop("`probability is an alias for `!freq', however they differ.")
    intensities <- counts/(n*h)
    mids <- 0.5 * (breaks[-1] + breaks[-nB])
    y <- if (freq) counts else intensities
    r <- list(breaks = breaks, counts = counts,
	      intensities = intensities, mids = mids)
    if (plot) {
	plot.new()
	plot.window(xlim, ylim, "") #-> ylim's default from 'y'
	if (missing(ylab))
	    ylab <- paste(if(!freq)"Relative ", "Frequency", sep="")
	if(freq && use.br && max(h)-min(h) > 1e-7 * mean(h))
	    warning("the AREAS in the plot are wrong -- maybe use `freq=FALSE'")
	title(main = main, xlab = xlab, ylab = ylab, ...)
	if(axes) {
	    axis(1, ...)
	    axis(2, ...)
	}
	rect(breaks[-nB], 0, breaks[-1], y,
	     col = col, border = border)
	if(labels)
	    text(mids, y,
		 labels = if(freq) counts else round(intensities,3),
		 adj = c(0.5, -0.5))
	invisible(r)
    }
    else r
}
