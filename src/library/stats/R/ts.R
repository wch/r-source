start	  <- function(x, ...) UseMethod("start")
end	  <- function(x, ...) UseMethod("end")
frequency <- function(x, ...) UseMethod("frequency")
time	  <- function(x, ...) UseMethod("time")
window	  <- function(x, ...) UseMethod("window")
cycle     <- function(x, ...) UseMethod("cycle")
deltat    <- function(x, ...) UseMethod("deltat")

ts <- function(data = NA, start = 1, end = numeric(0), frequency = 1,
	       deltat = 1, ts.eps  =  getOption("ts.eps"),
               class = if(nseries > 1) c("mts", "ts") else "ts",
               names = if(!is.null(dimnames(data))) colnames(data)
               else paste("Series", seq(nseries))
               )
{
    if(is.data.frame(data)) data <- data.matrix(data)
#   if(!is.numeric(data)) stop("'data'  must be a numeric vector or matrix")
    if(is.matrix(data)) {
	nseries <- ncol(data)
	ndata <- nrow(data)
        dimnames(data) <- list(NULL, names)
    } else {
	nseries <- 1
	ndata <- length(data)
    }
    if(ndata == 0) stop("'ts' object must have one or more observations")

    if(missing(frequency)) frequency <- 1/deltat
    else if(missing(deltat)) deltat <- 1/frequency

    if(frequency > 1 && abs(frequency - round(frequency)) < ts.eps)
	frequency <- round(frequency)

    if(length(start) > 1) {
## strange: this never checked for < 1!  commented for 1.7.0
##	if(start[2] > frequency) stop("invalid start")
	start <- start[1] + (start[2] - 1)/frequency
    }
    if(length(end) > 1) {
##	if(end[2] > frequency) stop("invalid end")
	end <- end[1] + (end[2] - 1)/frequency
    }
    if(missing(end))
	end <- start + (ndata - 1)/frequency
    else if(missing(start))
	start <- end - (ndata - 1)/frequency

    if(start > end) stop("'start' cannot be after 'end'")
    nobs <- floor((end - start) * frequency + 1.01)

    if(nobs != ndata)
	data <-
	    if(NCOL(data) == 1) {
		if(ndata < nobs) rep(data, length.out = nobs)
		else if(ndata > nobs) data[1:nobs]
	    } else {
		if(ndata < nobs) data[rep(1:ndata, length.out = nobs), ]
		else if(ndata > nobs) data[1:nobs, ]
	    }
    ## FIXME: The following "attr<-"() calls C tspgets() which uses a
    ##  	fixed equivalent of ts.eps := 1e-5
    attr(data, "tsp") <- c(start, end, frequency) #-- order is fixed
    if(!is.null(class) && class != "none") attr(data, "class") <- class
    ## if you alter the return structure, you also need to alter
    ## newBasic in methods/R/RClassUtils.R.  So please don't.
    data
}

tsp <- function(x) attr(x, "tsp")

"tsp<-" <- function(x, value)
{
    cl <- oldClass(x)
    attr(x, "tsp") <- value # does error-checking internally
    if (inherits(x, "ts") && is.null(value))
        class(x) <- if(!identical(cl,"ts")) cl["ts" != cl]
    else if (inherits(x, "mts") && is.null(value))
        class(x) <- if(!identical(cl,"mts")) cl["mts" != cl]
    x
}

hasTsp <- function(x)
{
    if(is.null(attr(x, "tsp")))
        attr(x, "tsp") <- c(1, NROW(x), 1)
    x
}

is.ts <- function(x) inherits(x, "ts") && length(x)

as.ts <- function(x, ...) UseMethod("as.ts")

as.ts.default <- function(x, ...)
{
    if (is.ts(x)) x
    else if(!is.null(xtsp <- tsp(x))) ts(x, xtsp[1], xtsp[2], xtsp[3])
    else ts(x)
}

.cbind.ts <- function(sers, nmsers, dframe = FALSE, union = TRUE)
{
    nulls <- sapply(sers, is.null)
    sers <- sers[!nulls]
    nser <- length(sers)
    if(nser == 0) return(NULL)
    if(nser == 1)
        if(dframe) return(as.data.frame(sers[[1]])) else return(sers[[1]])
    tsser <-  sapply(sers, function(x) length(tsp(x)) > 0)
    if(!any(tsser))
        stop("no time series supplied")
    sers <- lapply(sers, as.ts)
    nsers <- sapply(sers, NCOL)
    tsps <- sapply(sers[tsser], tsp)
    freq <- mean(tsps[3,])
    if(max(abs(tsps[3,] - freq)) > getOption("ts.eps")) {
        stop("not all series have the same frequency")
    }
    if(union) {
        st <- min(tsps[1,])
        en <- max(tsps[2,])
    } else {
        st <- max(tsps[1,])
        en <- min(tsps[2,])
        if(st > en) {
            warning("non-intersecting series")
            return(NULL)
        }
    }
    p <- c(st, en, freq)
    n <- round(freq * (en - st) + 1)
    if(any(!tsser)) {
        ln <- sapply(sers[!tsser], NROW)
        if(any(ln != 1 && ln != n))
            stop("non-time series not of the correct length")
        for(i in (1:nser)[!tsser]) {
            sers[[i]] <- ts(sers[[i]], start=st, end=en, frequency=freq)
        }
        tsps <- sapply(sers, tsp)
    }
    if(dframe) {
        x <- vector("list", nser)
        names(x) <- nmsers
    } else {
        ns <- sum(nsers)
        x <- matrix(, n, ns)
        cs <- c(0, cumsum(nsers))
        nm <- character(ns)
        for(i in 1:nser)
            if(nsers[i] > 1) {
                cn <- colnames(sers[[i]])
                if(is.null(cn)) cn <- 1:nsers[i]
                nm[(1+cs[i]):cs[i+1]] <- paste(nmsers[i], cn, sep=".")
            } else nm[cs[i+1]] <- nmsers[i]
        dimnames(x) <- list(NULL, nm)
    }
    for(i in 1:nser) {
        if(union) {
            xx <-
                if(nsers[i] > 1)
                    rbind(matrix(NA, round(freq * (tsps[1,i] - st)), nsers[i]),
                          sers[[i]],
                          matrix(NA, round(freq * (en - tsps[2,i])), nsers[i]))
                else
                    c(rep.int(NA, round(freq * (tsps[1,i] - st))), sers[[i]],
                      rep.int(NA, round(freq * (en - tsps[2,i]))))
        } else {
            xx <- window(sers[[i]], st, en)
        }
        if(dframe) x[[i]] <- structure(xx, tsp=p, class="ts")
        else x[, (1+cs[i]):cs[i+1]] <- xx
    }
    if(dframe) as.data.frame(x)
    else ts(x, start=st, freq=freq)
}

.makeNamesTs <- function(...)
{
    l <- as.list(substitute(list(...)))[-1]
    nm <- names(l)
    fixup <- if(is.null(nm)) seq(along = l) else nm == ""
    ## <NOTE>
    dep <- sapply(l[fixup], function(x) deparse(x)[1])
    ## We could add support for 'deparse.level' here by creating dep
    ## as in list.names() inside table().  But there is a catch: we
    ## need deparse.level = 2 to get the 'usual' deparsing when the
    ## method is invoked by the generic ...
    ## </NOTE>
    if(is.null(nm)) return(dep)
    if(any(fixup)) nm[fixup] <- dep
    nm
}

Ops.ts <- function(e1, e2)
{
    if(missing(e2)) {
        ## univariate operator
        NextMethod(.Generic)
    } else if(any(nchar(.Method) == 0)) {
        ## one operand is not a ts
        NextMethod(.Generic)
    } else {
        nc1 <- NCOL(e1)
        nc2 <- NCOL(e2)
        ## use ts.intersect to align e1 and e2
        e12 <- .cbind.ts(list(e1, e2),
                         c(deparse(substitute(e1))[1],
                           deparse(substitute(e2))[1]),
                         union = FALSE)
        e1 <- if(is.matrix(e1)) e12[, 1:nc1, drop = FALSE] else e12[, 1]
        e2 <- if(is.matrix(e2)) e12[, nc1 + (1:nc2), drop = FALSE]
        else e12[, nc1 + 1]
        NextMethod(.Generic)
    }
}

cbind.ts <- function(..., deparse.level = 1) {
    if(deparse.level != 1) .NotYetUsed("deparse.level != 1")
    .cbind.ts(list(...), .makeNamesTs(...), dframe = FALSE, union = TRUE)
}

ts.union <- function(..., dframe = FALSE)
    .cbind.ts(list(...), .makeNamesTs(...), dframe = dframe, union = TRUE)

ts.intersect <- function(..., dframe = FALSE)
    .cbind.ts(list(...), .makeNamesTs(...), dframe = dframe, union = FALSE)

diff.ts <- function (x, lag = 1, differences = 1, ...)
{
    if (lag < 1 | differences < 1)
        stop("bad value for 'lag' or 'differences'")
    if (lag * differences >= NROW(x)) return(x[0])
    ## <FIXME>
    ## lag() and its default method are defined in package ts, so we
    ## need to provide our own implementation.
    tsLag <- function(x, k = 1) {
        p <- tsp(x)
        tsp(x) <- p - (k/p[3]) * c(1, 1, 0)
        x
    }
    r <- x
    for (i in 1:differences) {
        r <- r - tsLag(r, -lag)
    }
    xtsp <- attr(x, "tsp")
    if(is.matrix(x)) colnames(r) <- colnames(x)
    ts(r, end = xtsp[2], freq = xtsp[3])
}

na.omit.ts <- function(object, ...)
{
    tm <- time(object)
    xfreq <- frequency(object)
    ## drop initial and final NAs
    if(is.matrix(object))
        good <- which(apply(!is.na(object), 1, all))
    else  good <- which(!is.na(object))
    if(!length(good)) stop("all times contain an NA")
    omit <- integer(0)
    n <- NROW(object)
    st <- min(good)
    if(st > 1) omit <- c(omit, 1:(st-1))
    en <- max(good)
    if(en < n) omit <- c(omit, (en+1):n)
    cl <- attr(object, "class")
    if(length(omit)) {
        object <- if(is.matrix(object)) object[st:en,] else object[st:en]
        attr(omit, "class") <- "omit"
        attr(object, "na.action") <- omit
        tsp(object) <- c(tm[st], tm[en], xfreq)
        if(!is.null(cl)) class(object) <- cl
    }
    if(any(is.na(object))) stop("time series contains internal NAs")
    object
}

is.mts <- function (x) inherits(x, "mts")

start.default <- function(x, ...)
{
    ts.eps <- getOption("ts.eps")
    tsp <- attr(hasTsp(x), "tsp")
    is <- tsp[1]*tsp[3]
    if(abs(tsp[3] - round(tsp[3])) < ts.eps &&
       abs(is - round(is)) < ts.eps) {
	is <- floor(tsp[1]+ts.eps)
	fs <- floor(tsp[3]*(tsp[1] - is)+0.001)
	c(is,fs+1)
    }
    else tsp[1]
}

end.default <- function(x, ...)
{
    ts.eps <- getOption("ts.eps")
    tsp <- attr(hasTsp(x), "tsp")
    is <- tsp[2]*tsp[3]
    if(abs(tsp[3] - round(tsp[3])) < ts.eps &&
       abs(is - round(is)) < ts.eps) {
	is <- floor(tsp[2]+ts.eps)
	fs <- floor(tsp[3]*(tsp[2] - is)+0.001)
	c(is, fs+1)
    }
    else tsp[2]
}

frequency.default <- function(x, ...)
    if(!is.null(xtsp <- attr(x, "tsp"))) xtsp[3] else 1

deltat.default <- function(x, ...)
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

cycle.default <- function(x, ...)
{
    p <- tsp(hasTsp(x))
    m <- round((p[1] %% 1) * p[3])
    x <- (1:NROW(x) + m - 1) %% p[3] + 1
    tsp(x) <- p
    x
}

cycle.ts <- function (x, ...) as.ts(cycle.default(x, ...))

print.ts <- function(x, calendar, ...)
{
    x.orig <- x
    x <- as.ts(x)
    fr.x <- frequency(x)
    if(missing(calendar))
	calendar <- any(fr.x == c(4,12))
    if(!calendar)
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
    if(NCOL(x) == 1) { # could be 1-col matrix
        if(calendar) {
            if(fr.x > 1) {
                dn2 <-
                    if(fr.x == 12) month.abb
                    else if(fr.x == 4) {
                        c("Qtr1", "Qtr2", "Qtr3", "Qtr4")
                    } else paste("p", 1:fr.x, sep = "")
                if(NROW(x) <= fr.x && start(x)[1] == end(x)[1]) {
                    ## not more than one period
                    dn1 <- start(x)[1]
                    dn2 <- dn2[1 + (start(x)[2] - 2 + seq(along=x))%%fr.x]
                    x <- matrix(format(x, ...), nrow = 1 , byrow = TRUE,
                                dimnames = list(dn1, dn2))
                } else { # more than one period
                    start.pad <- start(x)[2] - 1
                    end.pad <- fr.x - end(x)[2]
                    dn1 <- start(x)[1]:end(x)[1]
                    x <- matrix(c(rep.int("", start.pad), format(x, ...),
                                  rep.int("", end.pad)), nc =  fr.x, byrow = TRUE,
                                dimnames = list(dn1, dn2))
                }
            } else { ## fr.x == 1
                tx <- time(x)
                attributes(x) <- NULL
                names(x) <- tx
            }
        } else { ##-- no 'calendar' --
            header(x)
            attr(x, "class") <- attr(x, "tsp") <- attr(x, "na.action") <- NULL
        }
    } else { # multi-column matrix
	if(calendar && fr.x > 1) {
	    tm <- time(x)
	    t2 <- 1 + round(fr.x*((tm+0.001) %%1))
	    p1 <- format(floor(zapsmall(tm)))# yr
	    rownames(x) <-
		if(fr.x == 12)
		    paste(month.abb[t2], p1, sep=" ")
		else
		    paste(p1, if(fr.x == 4) c("Q1", "Q2", "Q3", "Q4")[t2]
			      else format(t2),
			  sep=" ")
        } else {
            if(!calendar) header(x)
            rownames(x) <- format(time(x))
        }
        attr(x, "class") <- attr(x, "tsp") <- attr(x, "na.action") <- NULL
    }
    NextMethod("print", x, quote = FALSE, right = TRUE, ...)
    invisible(x.orig)
}

plot.ts <-
    function (x, y = NULL, plot.type = c("multiple", "single"),
	      xy.labels, xy.lines, panel = lines, nc, yax.flip = FALSE,
	      mar.multi = c(0, 5.1, 0, if(yax.flip) 5.1 else 2.1),
	      oma.multi = c(6, 0, 5, 0), axes = TRUE, ...)
{
    plotts <-
	function (x, y = NULL, plot.type = c("multiple", "single"),
		  xy.labels, xy.lines, panel = lines, nc,
		  xlabel, ylabel, type = "l", xlim = NULL, ylim = NULL,
		  xlab = "Time", ylab, log = "", col = par("col"), bg = NA,
		  pch = par("pch"), cex = par("cex"),
		  lty = par("lty"), lwd = par("lwd"),
		  axes = TRUE, frame.plot = axes, ann = par("ann"),
		  main = NULL, ...)
    {
	plot.type <- match.arg(plot.type)
	nser <- NCOL(x)

	if(plot.type == "multiple" && nser > 1) {
	    addmain <- function(main, cex.main=par("cex.main"),
				font.main=par("font.main"),
				col.main=par("col.main"), ...)
		## pass 'cex.main' etc	via "..." from main function
		mtext(main, side=3, line=3,
		      cex=cex.main, font=font.main, col=col.main, ...)
	    panel <- match.fun(panel)
	    nser <- NCOL(x)
	    if(nser > 10) stop("cannot plot more than 10 series as \"multiple\"")
	    if(is.null(main)) main <- xlabel
	    nm <- colnames(x)
	    if(is.null(nm)) nm <- paste("Series", 1:nser)
	    if(missing(nc)) nc <- if(nser > 4) 2 else 1
	    nr <- ceiling(nser/nc)

	    oldpar <- par(mar = mar.multi, oma = oma.multi, mfcol = c(nr, nc))
	    on.exit(par(oldpar))
	    for(i in 1:nser) {
		plot.default(x[, i], axes = FALSE, xlab="", ylab="",
		     log = log, col = col, bg = bg, pch = pch, ann = ann,
		     type = "n", ...)
		panel(x[, i], col = col, bg = bg, pch = pch, type=type, ...)
		if(frame.plot) box(...)
		y.side <- if (i %% 2 || !yax.flip) 2 else 4
		do.xax <- i %% nr == 0 || i == nser
		if(axes) {
		    axis(y.side, xpd = NA)
		    if(do.xax)
			axis(1, xpd = NA)
		}
		if(ann) {
		    mtext(nm[i], y.side, line=3, ...)
		    if(do.xax)
			mtext(xlab, side=1, line=3, ...)
		}
	    }
	    if(ann && !is.null(main)) {
		par(mfcol=c(1,1))
		addmain(main, ...)
	    }
	    return(invisible())
	}
	## end of multiple plot section

	x <- as.ts(x)
	if(!is.null(y)) {
	    ## want ("scatter") plot of y ~ x
	    y <- hasTsp(y)
	    if(NCOL(x) > 1 || NCOL(y) > 1)
		stop("scatter plots only for univariate time series")
	    if (is.ts(x) && is.ts(y)) {
		xy <- ts.intersect(x, y)
		xy <- xy.coords(xy[,1], xy[,2], xlabel, ylabel, log)
	    } else
	    xy <- xy.coords(x, y, xlabel, ylabel, log)
	    xlab <- if (missing(xlab)) xy$xlab else xlab
	    ylab <- if (missing(ylab)) xy$ylab else ylab
	    xlim <- if (is.null(xlim)) range(xy$x[is.finite(xy$x)]) else xlim
	    ylim <- if (is.null(ylim)) range(xy$y[is.finite(xy$y)]) else ylim
	    n <- length(xy $ x)		  #-> default for xy.l(ines|abels)
	    if(missing(xy.labels)) xy.labels <- (n <= 150)
	    if(!is.logical(xy.labels)) {
		if(!is.character(xy.labels))
		    stop("'xy.labels' must be logical or character")
		do.lab <- TRUE
	    } else do.lab <- xy.labels

	    ptype <-
		if(do.lab) "n" else if(missing(type)) "p" else type
	    plot.default(xy, type = ptype,
			 xlab = xlab, ylab = ylab,
			 xlim = xlim, ylim = ylim, log = log, col = col, bg = bg,
			 pch = pch, axes = axes, frame.plot = frame.plot,
			 ann = ann, main = main, ...)
	    if(missing(xy.lines)) xy.lines <- do.lab
	    if(do.lab)
		text(xy, labels =
		     if(is.character(xy.labels)) xy.labels
		     else if(all(tsp(x) == tsp(y))) formatC(time(x), wid = 1)
		     else seq(along = x),
		     col = col, cex = cex)
	    if(xy.lines)
		lines(xy, col = col, lty = lty, lwd = lwd,
		      type = if(do.lab) "c" else "l")
	    return(invisible())
	}
	## Else : no y, only x

	if(missing(ylab)) {
	    ylab <- colnames(x)
	    if(length(ylab) != 1)
		ylab <- xlabel
	}
	## using xy.coords() mainly for the log treatment
	if(is.matrix(x)) {
	    k <- ncol(x)
	    tx <- time(x)
	    xy <- xy.coords(x = matrix(rep.int(tx, k), ncol = k),
			    y = x, log=log)
	    xy$x <- tx
	}
	else xy <- xy.coords(x, NULL, log=log)
	if(is.null(xlim)) xlim <- range(xy$x)
	if(is.null(ylim)) ylim <- range(xy$y[is.finite(xy$y)])
	plot.new()
	plot.window(xlim, ylim, log, ...)
	if(is.matrix(x)) {
	    for(i in seq(length=k))
		lines.default(xy$x, x[,i],
			      col = col[(i-1) %% length(col) + 1],
			      lty = lty[(i-1) %% length(lty) + 1],
			      lwd = lwd[(i-1) %% length(lwd) + 1],
			      bg  = bg [(i-1) %% length(bg) + 1],
			      pch = pch[(i-1) %% length(pch) + 1],
			      type = type)
	}
	else {
	    lines.default(xy$x, x, col = col[1], bg = bg, lty = lty[1],
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
    xlabel <- if (!missing(x)) deparse(substitute(x))# else NULL
    ylabel <- if (!missing(y)) deparse(substitute(y))
    plotts(x = x, y = y, plot.type = plot.type,
	   xy.labels = xy.labels, xy.lines = xy.lines,
	   panel = panel, nc = nc, xlabel = xlabel, ylabel = ylabel,
           axes = axes, ...)
}

lines.ts <- function(x, ...)
    lines.default(time(as.ts(x)), x, ...)


window.default <- function(x, start = NULL, end = NULL,
                           frequency = NULL, deltat = NULL,
                           extend = FALSE, ...)
{
    x <- hasTsp(x)
    xtsp <- tsp(x)
    xfreq <- xtsp[3]
    xtime <- time(x)
    ts.eps <- getOption("ts.eps")

    if(!is.null(frequency) && !is.null(deltat) &&
       abs(frequency*deltat - 1) > ts.eps)
        stop("'frequency' and 'deltat' are both supplied and are inconsistent")
    if (is.null(frequency) && is.null(deltat)) yfreq <- xfreq
    else if (is.null(deltat)) yfreq <- frequency
    else if (is.null(frequency)) yfreq <- 1/deltat
    thin <- round(xfreq/yfreq)
    if (yfreq > 0 && abs(xfreq/yfreq -thin) < ts.eps) {
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
		stop("bad value for 'start'"))
    if(start < xtsp[1] && !extend) {
	start <- xtsp[1]
	warning("'start' value not changed")
    }

    end <- if(is.null(end))
	xtsp[2]
    else switch(length(end),
		end,
		end[1] + (end[2] - 1)/xfreq,
		stop("bad value for 'end'"))
    if(end > xtsp[2] && !extend) {
	end <- xtsp[2]
	warning("'end' value not changed")
    }

    if(start > end)
	stop("'start' cannot be after 'end'")

    if(!extend) {
        if(all(abs(start - xtime) > abs(start) * ts.eps))
            start <- xtime[(xtime > start) & ((start + 1/xfreq) > xtime)]

        if(all(abs(end - xtime) > abs(end) * ts.eps))
            end <- xtime[(xtime < end) & ((end - 1/xfreq) < xtime)]

        i <- seq(trunc((start - xtsp[1]) * xfreq + 1.5),
                 trunc((end - xtsp[1]) * xfreq + 1.5), by = thin)
        y <- if(is.matrix(x)) x[i, , drop = FALSE] else x[i]
        ystart <- xtime[i[1]]
        yend <- xtime[i[length(i)]]
        attr(y, "tsp") <- c(ystart, yend, yfreq)
    } else {
        ## first adjust start and end to the time base
        stoff <- ceiling((start - xtsp[1]) * xfreq - ts.eps)
        ystart <- xtsp[1] + stoff/xfreq
        enoff <- floor((end - xtsp[2]) * xfreq + ts.eps)
        yend <- xtsp[2] + enoff/xfreq
        nold <- round(xfreq*(xtsp[2] - xtsp[1])) + 1
        ## both start and end could be outside time base
        i0 <- 1+max(0, stoff); i1 <- nold + min(0, enoff)
        i <- c(rep.int(nold+1, max(0, -stoff)),
                   if(i0 <= i1) i0:i1,
                   rep.int(nold+1, max(0, enoff)))
        y <- if(is.matrix(x)) rbind(x, NA)[i, , drop = FALSE] else c(x, NA)[i]
        attr(y, "tsp") <- c(ystart, yend, xfreq)
        if(yfreq != xfreq) y <- Recall(y, frequency = yfreq)
    }
    y
}

window.ts <- function (x, ...) as.ts(window.default(x, ...))

"window<-" <- function(x, ..., value) UseMethod("window<-")

"window<-.ts" <- function(x, start, end, frequency, deltat, ..., value)
{
    xtsp <- tsp(x)
    m <- match.call(expand.dots = FALSE)
    m$value <- NULL
    m$extend <- TRUE
    m[[1]] <- as.name("window")
    xx <- eval.parent(m)
    xxtsp <- tsp(xx)
    start <- xxtsp[1]; end <- xxtsp[2]
    if(start > end) stop("'start' > 'end'")
    if (start < xtsp[1] || end > xtsp[2]) {
        warning("extending time series when replacing values", call. = FALSE)
        x <- window(x, min(start, xtsp[1]), max(end, xtsp[2]), extend = TRUE)
    }
    xfreq <- xtsp[3]
    xtimes <- round(xfreq*time(x))
    xxtimes <- round(xfreq * time(xx))

    ind <- match(xxtimes, xtimes)
    if(any(is.na(ind))) stop("times to be replaced do not match")

    len <- length(ind)
    val_len <- length(value)
    if(!val_len) stop("no replacement values supplied")
    if(val_len > len) stop("too many replacement values supplied")
    if(val_len > 1 && (len %% val_len))
        stop("number of values supplied is not a sub-multiple of the number of values to be replaced")
    if(NCOL(x) == 1) x[ind] <- value else x[ind, ] <- value
    x
}

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

t.ts <- function(x) {
    cl <- oldClass(x)
    other <- !(cl %in% c("ts","mts"))
    class(x) <- if(any(other)) cl[other]
    attr(x, "tsp") <- NULL
    t(x)
}

ts.plot <- function(..., gpars = list())
{
    dots <- list(...)
    pars <- c("xlab", "ylab", "xlim", "ylim", "col", "lty", "lwd",
              "type", "main", "sub", "log")
    m <- names(dots) %in% pars
    if(length(m)) {
        gpars <- c(gpars, dots[m])
        dots <- dots[!m]
    }
    sers <- do.call("ts.union", dots)
    if(is.null(gpars$ylab))
        gpars$ylab <- if(NCOL(sers) > 1) "" else deparse(substitute(...))
    do.call("plot.ts", c(list(sers, plot.type = "single"), gpars))
}

arima.sim <- function(model, n, rand.gen = rnorm,
                      innov = rand.gen(n, ...), n.start = NA, ...)
{
    if(!is.list(model)) stop("'model' must be list")
    p <- length(model$ar)
    if(p) {
        minroots <- min(Mod(polyroot(c(1, -model$ar))))
        if(minroots <= 1) stop("'ar' part of model is not stationary")
    }
    q <- length(model$ma)
    if(is.na(n.start)) n.start <- p + q +
        ifelse(p > 0, ceiling(6/log(minroots)), 0)
    if(n.start < p + q) stop("burn-in 'n.start' must be as long as 'ar + ma'")
    d <- 0
    if(!is.null(ord <- model$order)) {
        if(length(ord) != 3) stop("'model$order' must be of length 3")
        if(p != ord[1]) stop("inconsistent specification of 'ar' order")
        if(q != ord[3]) stop("inconsistent specification of 'ma' order")
        d <- ord[2]
        if(d != round(d) || d < 0)
            stop("number of differences must be a positive integer")
    }
    x <- ts(c(rand.gen(n.start, ...), innov[1:n]), start = 1 - n.start)
    if(length(model$ma)) x <- filter(x, c(1, model$ma), sides = 1)
    if(length(model$ar)) x <- filter(x, model$ar, method = "recursive")
    if(n.start > 0) x <- x[-(1:n.start)]
    if(d > 0) x <- diffinv(x, differences = d)
    as.ts(x)
}
