start	  <- function(x, ...) UseMethod("start")
end	  <- function(x, ...) UseMethod("end")
frequency <- function(x, ...) UseMethod("frequency")
time	  <- function(x, ...) UseMethod("time")
window	  <- function(x, ...) UseMethod("window")
cycle     <- function(x, ...) UseMethod("cycle")
deltat    <- function(x, ...) UseMethod("deltat")

options(ts.eps = 1e-5)   # default as S

ts <- function(data = NA, start = 1, end = numeric(0), frequency = 1,
	       deltat = 1, ts.eps  =  .Options$ts.eps,
               class = if(nseries > 1) c("mts", "ts") else "ts")
{
    if(is.matrix(data) || is.data.frame(data)) {
	nseries <- ncol(data)
	ndata <- nrow(data)
    } else {
	nseries <- 1
	ndata <- length(data)
    }

    if(missing(frequency)) frequency <- 1/deltat
    else if(missing(deltat)) deltat <- 1/frequency

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

    if(start > end) stop("start cannot be after end")
    nobs <- floor((end - start) * frequency + 1.01)

    if(nobs != ndata)
	data <-
	    if(NCOL(data) == 1) {
		if(ndata < nobs) rep(data, length = nobs)
		else if(ndata > nobs) data[1:nobs]
	    } else {
		if(ndata < nobs) data[rep(1:ndata, length = nobs), ]
		else if(ndata > nobs) data[1:nobs, ]
	    }
    attr(data, "tsp") <- c(start, end, frequency) #-- order is fixed
    if(!is.null(class) && class != "none") attr(data, "class") <- class
    data
}

tsp <- function(x) attr(x, "tsp")

"tsp<-" <- function(x, value)
{
    cl <- class(x)
    attr(x, "tsp") <- value # does error-checking internally
    if (inherits(x, "ts") && is.null(value))
        class(x) <- cl["ts" != cl]
    x
}

hasTsp <- function(x)
{
    if(is.null(attr(x, "tsp")))
        attr(x, "tsp") <- c(1, NROW(x), 1)
    x
}

is.ts <- function (x) inherits(x, "ts")

as.ts <- function (x)
{
    if (is.ts(x)) x
    else if(!is.null(xtsp <- tsp(x))) ts(x, xtsp[1], xtsp[2], xtsp[3])
    else ts(x)
}

start.default <- function(x, ...)
{
    ts.eps <- .Options$ts.eps
    tsp <- attr(hasTsp(x), "tsp")
    is <- tsp[1]*tsp[3]
    if(abs(is-round(is)) < ts.eps) {
	is <- floor(tsp[1]+ts.eps)
	fs <- floor(tsp[3]*(tsp[1] - is)+0.001)
	c(is,fs+1)
    }
    else tsp[1]
}

end.default <- function(x, ...)
{
    ts.eps <- .Options$ts.eps
    tsp <- attr(hasTsp(x), "tsp")
    is <- tsp[2]*tsp[3]
    if(abs(is-round(is)) < ts.eps) {
	is <- floor(tsp[2]+ts.eps)
	fs <- floor(tsp[3]*(tsp[2] - is)+0.001)
	c(is, fs+1)
    }
    else tsp[2]
}

frequency.default <- function(x, ...)
    if(!is.null(xtsp <- attr(x, "tsp"))) xtsp[3] else 1

deltat.default <- function(x)
    if(!is.null(xtsp <- attr(x, "tsp"))) 1/xtsp[3] else 1

time.default <- function (x, offset = 0, ...)
{
    n <- if(is.matrix(x)) nrow(x) else length(x)
    xtsp <- attr(hasTsp(x), "tsp")
    y <- seq(xtsp[1], xtsp[2], length = n) + offset/xtsp[3]
    tsp(y) <- xtsp
    y
}

time.ts <- function (x, ...) as.ts(time.default(x, ...))

cycle.default <- function(x)
{
    p <- tsp(hasTsp(x))
    m <- floor((p[1] %% 1) * p[3])
    x <- (1:NROW(x) + m - 1) %% p[3] + 1
    tsp(x) <- p
    x
}

cycle.ts <- function (x, ...) as.ts(cycle.default(x, ...))

print.ts <- function(x, calendar, ...)
{
    header <- function(x) {
        if((fr.x <- frequency(x))!= 1)
            cat("Time Series:\nStart =", deparse(start(x)),
                "\nEnd =", deparse(end(x)),
                "\nFrequency =", deparse(fr.x), "\n")
        else
            cat("Time Series:\nStart =", format(tsp(x)[1]),
                "\nEnd =", format(tsp(x)[2]),
                "\nFrequency =", deparse(fr.x), "\n")
    }
    x.orig <- x
    x <- as.ts(x)
    fr.x <- frequency(x)
    if(missing(calendar))
	calendar <- any(fr.x==c(4,12))
    if(NCOL(x) == 1) { # could be 1-col matrix
        if(calendar) {
            if(fr.x > 1) {
                dn2 <-
                    if(fr.x == 12) month.abb
                    else if(fr.x == 4) {
                        c("Qtr1", "Qtr2", "Qtr3", "Qtr4")
                    } else paste("p", 1:fr.x, sep = "")
                if(NROW(x) <= fr.x) { # not more than one period
                    dn1 <- start(x)[1]
                    dn2 <- dn2[1 + (start(x)[2] - 2 + seq(along=x))%%fr.x]
                    x <- matrix(format(x, ...), nrow = 1 , byrow = TRUE,
                                dimnames = list(dn1, dn2))
                } else { # more than one period
                    start.pad <- start(x)[2] - 1
                    end.pad <- fr.x - end(x)[2]
                    dn1 <- start(x)[1]:end(x)[1]
                    x <- matrix(c(rep("", start.pad), format(x, ...),
                                  rep("", end.pad)), nc =  fr.x, byrow = TRUE,
                                dimnames = list(dn1, dn2))
                }
            } else { ## fr.x == 1
                tx <- time(x)
                attributes(x) <- NULL
                names(x) <- tx
            }
        } else { ##-- no `calendar' --
            header(x)
            attr(x, "class") <- attr(x, "tsp") <- NULL
        }
    } else { # multi-column matrix
        if(calendar && fr.x > 1) {
            tm <- time(x)
            t2 <- 1 + floor(fr.x*(tm %%1))
            p1 <- format(floor(tm))
            if(fr.x == 12) {
                p2 <- month.abb[t2]
                rownames(x) <- paste(p2, p1, sep=" ")
            } else {
                if(fr.x == 4)
                    p2 <- c("Q1", "Q2", "Q3", "Q4")[t2]
                else p2 <- format(t2)
                rownames(x) <- paste(p1, p2, sep=" ")
            }
        } else {
            if(!calendar) header(x)
            rownames(x) <- format(time(x))
        }
        attr(x, "class") <- attr(x, "tsp") <- NULL
    }
    NextMethod("print", x, quote = FALSE, right = TRUE, ...)
    invisible(x.orig)
}

plot.ts <-
function (x, y = NULL, type = "l", xlim = NULL, ylim = NULL, xlab =
          "Time", ylab, log = "", col = par("col"), bg = NA, pch =
          par("pch"), cex = par("cex"), lty = par("lty"), lwd =
          par("lwd"), axes = TRUE, frame.plot = axes, ann = par("ann"),
          main = NULL, plot.type = c("multiple", "single"), ...)
{
    xlabel <- if (!missing(x)) deparse(substitute(x)) else NULL
    ylabel <- if (!missing(y)) deparse(substitute(y)) else NULL
    plot.type <- match.arg(plot.type)
    if(plot.type == "multiple" && NCOL(x) > 1) {
        m <- match.call()
        m[[1]] <- as.name("plot.mts")
        return(eval(m, parent.frame()))
    }
    x <- as.ts(x)
    if(!is.null(y)) {
	## want ("scatter") plot of y ~ x
	y <- hasTsp(y)
        if(NCOL(x) > 1 || NCOL(y) > 1)
            stop("scatter plots only for univariate time series")
        if(is.ts(x) && is.ts(y)){
            xy <- ts.intersect(x, y)
            xy <- xy.coords(xy[,1], xy[,2], xlabel, ylabel, log)
        } else
            xy <- xy.coords(x, y, xlabel, ylabel, log)
	xlab <- xy$xlab
	ylab <- if (missing(ylab)) xy$ylab else ylab
	xlim <- if (is.null(xlim)) range(xy$x[is.finite(xy$x)]) else xlim
	ylim <- if (is.null(ylim)) range(xy$y[is.finite(xy$y)]) else ylim
	plot.default(xy, type = "n", xlab = xlab, ylab = ylab, xlim =
                     xlim, ylim = ylim, log = log, col = col, bg = bg,
                     pch = pch, axes = axes, frame.plot = frame.plot,
                     ann = ann, main = main, ...)
	text(xy, labels =
             if(all(tsp(x)==tsp(y))) formatC(time(x), wid = 1)
             else seq(along = x),
	     col = col, cex = cex)
	lines(xy, col = col, lty = lty, lwd = lwd)
	return(invisible())
    }
    if(missing(ylab)) ylab <- xlabel
    time.x <- time(x)
    if(is.null(xlim)) xlim <- range(time.x)
    if(is.null(ylim)) ylim <- range(x[is.finite(x)])
    plot.new()
    plot.window(xlim, ylim, log, ...)
    if(is.matrix(x)) {
	for(i in 1:ncol(x))
	    lines.default(time.x, x[,i],
			  col = col[(i-1) %% length(col) + 1],
			  lty = lty[(i-1) %% length(lty) + 1],
			  lwd = lwd[(i-1) %% length(lwd) + 1],
			  bg  =  bg[(i-1) %% length(bg)  + 1],
			  pch = pch[(i-1) %% length(pch) + 1],
			  type = type)
    }
    else {
	lines.default(time.x, x, col = col[1], bg = bg, lty = lty[1],
                      lwd = lwd[1], pch = pch[1], type = type)
    }
    if (ann)
	title(main = main, xlab = xlab, ylab = ylab, ...)
    if (axes) {
	axis(1, ...)
	axis(2, ...)
    }
    if (frame.plot) box(...)
}

lines.ts <- function(x, ...)
    lines.default(time(as.ts(x)), x, ...)

plot.mts <- function (x, plot.type = c("multiple", "single"),
                      log = "", col = par("col"),  bg = NA, pch = par("pch"),
                      cex = par("cex"), lty = par("lty"), lwd = par("lwd"),
                      ann = par("ann"),  xlab = "Time", main=NULL,
                      oma=c(6, 0, 5, 0),...)
{
    addmain <- function(main, cex.main=par("cex.main"),
                        font.main=par("font.main"),
                        col.main=par("col.main"), ...)
    {
            mtext(main, 3, 3, cex=cex.main, font=font.main, col=col.main, ...)
    }
    plot.type <- match.arg(plot.type)
    nser <- NCOL(x)
    if(plot.type == "single" || nser == 1) {
        m <- match.call()
        m[[1]] <- as.name("plot.ts")
        m$plot.type <- "single"
        return(eval(m, parent.frame()))
    }
    if(nser > 10) stop("Can't plot more than 10 series")
    if(is.null(main)) main <- deparse(substitute(x))
    nm <- colnames(x)
    if(is.null(nm)) nm <- paste("Series", 1:nser)
    nc <- if(nser >  4) 2 else 1
    oldpar <- par("mar", "oma", "mfcol")
    on.exit(par(oldpar))
    par(mar = c(0, 5.1, 0, 2.1), oma = oma)
    nr <- ceiling(nser %/% nc)
    par(mfcol = c(nr, nc))
    for(i in 1:nser) {
        plot(x[, i], axes = F, xlab="", ylab="",
             log = log, col = col, bg = bg, pch = pch, ann = ann,
             ...)
        box()
        axis(2, xpd=NA)
        mtext(nm[i], 2, 3)
        if(i%%nr==0 || i==nser) axis(1, xpd=NA)
    }
    if(ann) {
        mtext(xlab, 1, 3, ...)
        if(!is.null(main)) {
            par(mfcol=c(1,1))
            addmain(main, ...)
        }
    }
    invisible()
}

window.default <- function(x, start = NULL, end = NULL,
                           frequency = NULL, deltat = NULL, ...)
{
    x <- hasTsp(x)
    xtsp <- tsp(x)
    xfreq <- xtsp[3]
    xtime <- time(x)
    ts.eps <- .Options$ts.eps

    if(!is.null(frequency) && !is.null(deltat) &&
       abs(frequency*deltat - 1) > ts.eps)
        stop("frequency and deltat are both supplied and are inconsistent")
    if (is.null(frequency) && is.null(deltat)) yfreq <- xfreq
    else if (is.null(deltat)) yfreq <- frequency
    else if (is.null(frequency)) yfreq <- 1/deltat
    if (yfreq > 0 && xfreq%%yfreq < ts.eps) {
        thin <- round(xfreq/yfreq)
        yfreq <- xfreq/thin
    } else {
        thin <- 1
        yfreq <- xfreq
        warning("Frequency not changed")
    }
    start <- if(is.null(start))
	xtsp[1]
    else switch(length(start),
		start,
		start[1] + (start[2] - 1)/xfreq,
		stop("Bad value for start"))
    if(start < xtsp[1]) {
	start <- xtsp[1]
	warning("start value not changed")
    }

    end <- if(is.null(end))
	xtsp[2]
    else switch(length(end),
		end,
		end[1] + (end[2] - 1)/xfreq,
		stop("Bad value for end"))
    if(end > xtsp[2]) {
	end <- xtsp[2]
	warning("end value not changed")
    }

    if(start > end)
	stop("start cannot be after end")

    if(all(abs(start - xtime) > abs(start) * ts.eps))
	start <- xtime[(xtime > start) & ((start + 1/xfreq) > xtime)]

    if(all(abs(end - xtime) > abs(end) * ts.eps))
	end <- xtime[(xtime < end) & ((end - 1/xfreq) < xtime)]

    i <- seq(trunc((start - xtsp[1]) * xfreq + 1.5), trunc((end -
        xtsp[1]) * xfreq + 1.5), by = thin)
    y <- if(is.matrix(x)) x[i, , drop = FALSE] else x[i]
    ystart <- xtime[i[1]]
    yend <- xtime[i[length(i)]]
    attr(y, "tsp") <- c(ystart, yend, yfreq)
    y
}

window.ts <- function (x, ...) as.ts(window.default(x, ...))

"[.ts" <- function (x, i, j, drop = TRUE) {
    y <- NextMethod("[")
    if (missing(i))
	ts(y, start = start(x), freq = frequency(x))
#     else {
#         if(is.matrix(i)) return(y)
# 	n <- if (is.matrix(x)) nrow(x) else length(x)
# 	li <- length(ind <- (1:n)[i])
#         if(li == 0) return(numeric(0))
#         if(li == 1) {
#             tsp(y) <- c(start(x), start(x), frequency(x))
#             class(y) <- class(x)
#             return(y)
#         }
# 	if (length(unique(ind[-1] - ind[-li])) != 1) {
# 	    warning("Not returning a time series object")
# 	} else {
# 	    xtsp <- tsp(x)
# 	    xtimes <- seq(from = xtsp[1], to = xtsp[2], by = 1 / xtsp[3])
# 	    ytsp <- xtimes[range(ind)]
# 	    tsp(y) <- c(ytsp, (li - 1) / (ytsp[2] - ytsp[1]))
#             class(y) <- class(x)
# 	}
# 	y
#     }
    else y
}
