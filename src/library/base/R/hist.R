hist <- function(x, ...) UseMethod("hist")

hist.default <-
function (x, breaks, freq = NULL, probability = !freq, include.lowest = TRUE,
	col = NULL, border = par("fg"),
	main = paste("Histogram of" , deparse(substitute(x))),
	xlim = range(breaks), ylim = range(y, 0),
	xlab = deparse(substitute(x)), ylab,
	axes = TRUE, plot = TRUE, labels = FALSE, ...)
{
	if (!is.numeric(x))
		stop("hist: x must be numeric")
	eval(main)
	eval(xlab)
	n <- length(x <- x[!is.na(x)])
	use.br <- !missing(breaks) && length(breaks) > 1
	breaks <-
	  if(use.br) sort(breaks)
	  else {
		rx <- range(x)
		pretty (rx + c(0, diff(rx)/1000),
			n = if(missing(breaks)) 1 + log2(n)
			else { # breaks = `nclass'
				if (is.na(breaks) | breaks < 2)
				  stop("invalid number of breaks")
				breaks
			})
	  }
	nB <- length(breaks)
	counts <- .C("bincount",
		as.double(x),
		n,
		as.double(breaks),
		nB,
		counts = integer(nB - 1),
		include= as.logical(include.lowest),
		NAOK = FALSE) $counts
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
	if (plot) {
		plot.new()
		plot.window(xlim, ylim, "") #-> ylim's default from 'y'
          	if (missing(ylab))
                	ylab <- paste(if(!freq)"Relative ", "Frequency", sep="")
                if(freq && use.br && max(h)-min(h) > 1e-7 * mean(h))
                	warning("the AREAS in the plot are wrong -- maybe use `freq=F'")
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
	}
	invisible(list(breaks = breaks, counts = counts,
		intensities = intensities, mids = mids))
}
