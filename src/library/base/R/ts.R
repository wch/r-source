start	<- function(x, ...) UseMethod("start")
end	<- function(x, ...) UseMethod("end")
frequency <- function(x, ...) UseMethod("frequency")
time	<- function(x, ...) UseMethod("time")
window	<- function(x, ...) UseMethod("window")

# The first 2 as requested by  <la-jassine@aix.pacwan.net>
start.default	<- function (x) start(ts(x))
end.default	<- function (x)	end(ts(x))
frequency.default<-function (x) frequency(ts(x))
time.default	<- function (x)	time(ts(x))
window.default	<- function (x)	window(ts(x))


options(ts.eps = 1e-5)#- default as S

ts <- function(data=NA, start=1, end=numeric(0), frequency=1, deltat=1,
	 ts.eps = .Options$ts.eps)
{
	if(is.matrix(data)) {
		nseries <- ncol(data)
		ndata <- nrow(data)
	} else {
		nseries <- 1
		ndata <- length(data)
	}

	if(missing(frequency)) frequency <- 1/deltat
	if(missing(deltat)) deltat <- 1/deltat

	if(frequency > 1 && abs(frequency - round(frequency)) < ts.eps)
		frequency <- round(frequency)

	if(length(start) > 1) {
		if(start[2] > frequency) stop("invalid start")
		start <- start[1] + (start[2] - 1)/frequency
	}
	if(length(end) > 1) {
		if(end[2] > frequency) stop("invalid end")
		end <- end[1] + (end[2] - 1)/frequency
	}
	if(missing(end))
		end <- start + (ndata - 1)/frequency
	else if(missing(start))
		start <- end - (ndata - 1)/frequency

	nobs <- floor((end - start) * frequency + 1.01)

	if(nobs != ndata)
	  data <-
	    if(nseries == 1) {
	      if(ndata < nobs) rep(data, length=nobs)
	      else if(nobs > ndata) data[1:nobs]
	    } else {
	      if(ndata < nobs) data[rep(1:ndata, length=nobs)]
	      else if(nobs > ndata) data[1:nobs,]
	    }
	attr(data, "tsp") <- c(start, end, frequency)#-- order is fix !
	attr(data, "class") <- "ts"
	data
}

tsp <- function(x) attr(x, "tsp")

"tsp<-" <- function(x, value)
{
	attr(x,"tsp") <- value
	class(x) <- "ts"
	x
}

is.ts <-function (x) inherits(x, "ts")

as.ts <-function (x) if (is.ts(x)) x else ts(x)

start.ts <- function(x)
{
	ts.eps <- .Options$ts.eps
	##if(is.null(ts.eps)) ts.eps <- 1.e-5
	tsp <- attr(as.ts(x), "tsp")
	is <- tsp[1]*tsp[3]
	if(abs(is-round(is)) < ts.eps) {
		is <- floor(tsp[1])
		fs <- floor(tsp[3]*(tsp[1] - is)+0.001)
		c(is,fs+1)
	}
	else tsp[1]
}

end.ts <- function(x)
{
	ts.eps <- .Options$ts.eps
	##if(is.null(ts.eps)) ts.eps <- 1.e-5
	tsp <- attr(as.ts(x), "tsp")
	is <- tsp[2]*tsp[3]
	if(abs(is-round(is)) < ts.eps) {
		is <- floor(tsp[2])
		fs <- floor(tsp[3]*(tsp[2] - is)+0.001)
		c(is, fs+1)
	}
	else tsp[2]
}

frequency.ts <- function(x) { attr(as.ts(x), "tsp")[3] }

time.ts <- function (x)
{
	x <- as.ts(x)
	n <- if(is.matrix(x)) nrow(x) else length(x)
	xtsp <- attr(x, "tsp")
	ts(seq(xtsp[1], xtsp[2], length=n),
		start=start(x), end=end(x), frequency=frequency(x))
}

print.ts <- function(x, calendar, ...)
{
	fr.x <- frequency(x)
	if(missing(calendar))
		calendar <- any(fr.x==c(4,12))
	if(!is.matrix(x) && calendar) {
		if(fr.x > 1) {
			start.pad <- start(x)[2] - 1
			end.pad <- fr.x - end(x)[2]
			dn1 <- start(x)[1]:end(x)[1]
			dn2 <-
			  if(fr.x == 12)  month.abb
			  else if(fr.x == 4) {
				  dn1 <- paste(dn1, ":" , sep="")
				  c("Qtr1", "Qtr2", "Qtr3", "Qtr4")
			  } else paste("p", 1:fr.x, sep="")
			x <- matrix(c(rep(NA, start.pad), x,
				rep(NA, end.pad)), nc= fr.x, byrow=TRUE,
				dimnames = list(dn1, dn2))
		} else { ## fr.x == 1
			tx <- time(x)
			attributes(x) <- NULL
			names(x) <- tx
		}
	}
	else { ##-- no 'calendar' --
		cat("Time-Series:\nStart =", deparse(start(x)),
		    "\nEnd =", deparse(end(x)),
		    "\nFrequency =", deparse(fr.x), "\n")
		tx <- time(x)
		attr(x, "tsp") <- NULL
		attr(x, "class") <- NULL
		##>> something like this is needed here
		##---  if(is.matrix(x)) rownames(data) <- tx
	}
	NextMethod("print", ...)
	invisible(x)
}

plot.ts <-
function (x, type="l", xlim=NULL, ylim=NULL, xlab = "Time", ylab, log="",
	col=par("col"), bg=NA, pch=par("pch"), lty=par("lty"),
	axes = TRUE, frame.plot = axes, ann = par("ann"), main = NULL, ...)
{
	time.x <- time(x)
	if(is.null(xlim)) xlim <- range(time.x)
	if(is.null(ylim)) ylim <- range(x, finite=TRUE)
	if(missing(ylab)) ylab <- deparse(substitute(x))
	plot.new()
	plot.window(xlim, ylim, log)
	if(is.matrix(x)) {
		for(i in 1:ncol(x))
			lines.default(time.x, x[,i],
				col=col[(i-1)%%length(col) + 1],
				lty=lty[(i-1)%%length(lty) + 1],
				bg = bg[(i-1)%%length(bg) + 1],
				pch=pch[(i-1)%%length(pch) + 1],
				type=type)
	}
	else {
		lines.default(time.x, x, col=col[1], bg=bg, lty=lty[1],
			pch=pch[1], type=type)
	}

	pars <- list(...)
	if (ann)
		title(main = main, xlab = xlab, ylab = ylab, pars = pars)
	if (axes) {
		axis(1, pars = pars)
		axis(2, pars = pars)
	}
        if (frame.plot) box(...)
}

window.ts <- function(x, start, end)
{
 x <- as.ts(x)
 xtsp <- tsp(x)
 freq <- xtsp[3]
 xtime <- time(x)
 ts.eps <- .Options$ts.eps

 start <- if(missing(start))
		xtsp[1]
	  else switch(length(start),
		start,
		start[1] + (start[2] - 1)/freq,
		stop("Bad value for start"))
 if(start < xtsp[1]) {
	start <- xtsp[1]
	warning("start value not changed")
 }

 end <- if(missing(end))
		xtsp[2]
	else switch(length(end),
		end,
		end[1] + (end[2] - 1)/freq,
		stop("Bad value for end"))
 if(end > xtsp[2]) {
	end <- xtsp[2]
	warning("end value not changed")
 }

 if(start > end)
	stop("start cannot be after end")

 if(all(abs(start - xtime) > abs(start) * ts.eps)) {
	start <- xtime[(xtime > start) & ((start + 1/freq) > xtime)]
 }
 if(all(abs(end - xtime) > abs(end) * ts.eps)) {
	end <- xtime[(xtime < end) & ((end - 1/freq) < xtime)]
 }
 i <- trunc((start - xtsp[1]) * freq + 1.5):
      trunc(( end  - xtsp[1]) * freq + 1.5)
 x <- if(is.matrix(x)) x[i, , drop = FALSE] else x[i]
 tsp(x) <- c(start, end, freq)
 x
}

"[.ts" <- function (x, i, j, drop = TRUE)
{
 y <- NextMethod("[")
 if (missing(i))
        ts(y, start = start(x), freq = frequency(x))
 else {
        n <- if (is.matrix(x)) nrow(x) else length(x)
        li <- length(ind <- (1:n)[i])
        if(li > 1) delta <- unique(ind[-1] - ind[-li])
        if (li <= 1 || length(delta) != 1) {
                warning("Not returning a time series object")
        } else {
                xtsp <- tsp(x)
                xtimes <- seq(from = xtsp[1], to = xtsp[2], by = 1/xtsp[3])
                ytsp <- xtimes[range(ind)]
                tsp(y) <- c(ytsp, (li - 1)/(ytsp[2] - ytsp[1]))
        }
        y
 }
}
