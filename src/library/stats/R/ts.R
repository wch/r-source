#  File src/library/stats/R/ts.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2016 The R Core Team
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

start	  <- function(x, ...) UseMethod("start")
end	  <- function(x, ...) UseMethod("end")
frequency <- function(x, ...) UseMethod("frequency")
time	  <- function(x, ...) UseMethod("time")
window	  <- function(x, ...) UseMethod("window")
cycle     <- function(x, ...) UseMethod("cycle")
deltat    <- function(x, ...) UseMethod("deltat")

ts <- function(data = NA, start = 1, end = numeric(), frequency = 1,
	       deltat = 1, ts.eps  =  getOption("ts.eps"),
	       class = if(nseries > 1) c("mts", "ts", "matrix") else "ts",
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

    if(length(start) > 1L) {
## strange: this never checked for < 1!  commented for 1.7.0
##	if(start[2L] > frequency) stop("invalid start")
	start <- start[1L] + (start[2L] - 1)/frequency
    }
    if(length(end) > 1L) {
##	if(end[2L] > frequency) stop("invalid end")
	end <- end[1L] + (end[2L] - 1)/frequency
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
		if(ndata < nobs) rep_len(data, nobs)
		else if(ndata > nobs) data[1L:nobs]
	    } else {
		if(ndata < nobs) data[rep_len(1L:ndata, nobs), ]
		else if(ndata > nobs) data[1L:nobs, ]
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

`tsp<-` <- function(x, value)
{
    cl <- oldClass(x)
    attr(x, "tsp") <- value # does error-checking internally
    if (is.null(value)) {
        if (inherits(x, "ts"))
	    cl <- cl["ts" != cl]
        if (inherits(x, "mts"))
	    cl <- cl["mts" != cl]
        class(x) <- cl
    }
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
    else if(!is.null(xtsp <- tsp(x))) ts(x, xtsp[1L], xtsp[2L], xtsp[3L])
    else ts(x)
}

.cbind.ts <- function(sers, nmsers, dframe = FALSE, union = TRUE)
{
    nulls <- vapply(sers, is.null, NA)
    sers <- sers[!nulls]
    nser <- length(sers)
    if(nser == 0L) return(NULL)
    if(nser == 1L)
        if(dframe) return(as.data.frame(sers[[1L]])) else return(sers[[1L]])
    tsser <- vapply(sers, function(x) length(tsp(x)) > 0L, NA)
    if(!any(tsser))
        stop("no time series supplied")
    sers <- lapply(sers, as.ts)
    nsers <- vapply(sers, NCOL, 1)
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
        ln <- vapply(sers[!tsser], NROW, 1)
        if(any(ln != 1 && ln != n))
            stop("non-time series not of the correct length")
        for(i in (1L:nser)[!tsser]) {
            sers[[i]] <- ts(sers[[i]], start=st, end=en, frequency=freq)
        }
        tsps <- sapply(sers, tsp)
    }
    if(dframe) {
	x <- setNames(vector("list", nser), nmsers)
    } else {
        ns <- sum(nsers)
        x <- matrix(, n, ns)
        cs <- c(0, cumsum(nsers))
        nm <- character(ns)
        for(i in 1L:nser)
            if(nsers[i] > 1) {
                cn <- colnames(sers[[i]])
                if(is.null(cn)) cn <- 1L:nsers[i]
                nm[(1+cs[i]):cs[i+1]] <- paste(nmsers[i], cn, sep=".")
            } else nm[cs[i+1]] <- nmsers[i]
        dimnames(x) <- list(NULL, nm)
    }
    for(i in 1L:nser) {
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
    else ts(x, start=st, frequency=freq)
}

.makeNamesTs <- function(...)
{
    l <- as.list(substitute(list(...)))[-1L]
    nm <- names(l)
    fixup <- if(is.null(nm)) seq_along(l) else nm == ""
    ## <NOTE>
    dep <- sapply(l[fixup], function(x) deparse(x)[1L])
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
    } else if(any(!nzchar(.Method))) {
        ## one operand is not a ts
        NextMethod(.Generic)
    } else {
        nc1 <- NCOL(e1)
        nc2 <- NCOL(e2)
        ## use ts.intersect to align e1 and e2
        e12 <- .cbind.ts(list(e1, e2),
                         c(deparse(substitute(e1))[1L],
                           deparse(substitute(e2))[1L]),
                         union = FALSE)
        e1 <- if(is.matrix(e1)) e12[, 1L:nc1, drop = FALSE] else e12[, 1]
        e2 <- if(is.matrix(e2)) e12[, nc1 + (1L:nc2), drop = FALSE]
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
    if (lag * differences >= NROW(x)) return(x[0L])
    ## <FIXME>
    ## lag() and its default method are defined in package ts, so we
    ## need to provide our own implementation.
    tsLag <- function(x, k = 1) {
        p <- tsp(x)
        tsp(x) <- p - (k/p[3L]) * c(1, 1, 0)
        x
    }
    r <- x
    for (i in 1L:differences) {
        r <- r - tsLag(r, -lag)
    }
    xtsp <- attr(x, "tsp")
    if(is.matrix(x)) colnames(r) <- colnames(x)
    ts(r, end = xtsp[2L], frequency = xtsp[3L])
}

na.omit.ts <- function(object, ...)
{
    tm <- time(object)
    xfreq <- frequency(object)
    ## drop initial and final NAs
    if(is.matrix(object))
        good <- which(apply(!is.na(object), 1L, all))
    else  good <- which(!is.na(object))
    if(!length(good)) stop("all times contain an NA")
    omit <- integer()
    n <- NROW(object)
    st <- min(good)
    if(st > 1) omit <- c(omit, 1L:(st-1))
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
    if(anyNA(object)) stop("time series contains internal NAs")
    object
}

is.mts <- function (x) inherits(x, "mts")

start.default <- function(x, ...)
{
    ts.eps <- getOption("ts.eps")
    tsp <- attr(hasTsp(x), "tsp")
    is <- tsp[1L]*tsp[3L]
    if(abs(tsp[3L] - round(tsp[3L])) < ts.eps &&
       abs(is - round(is)) < ts.eps) {
	is <- floor(tsp[1L]+ts.eps)
	fs <- floor(tsp[3L]*(tsp[1L] - is)+0.001)
	c(is,fs+1)
    }
    else tsp[1L]
}

end.default <- function(x, ...)
{
    ts.eps <- getOption("ts.eps")
    tsp <- attr(hasTsp(x), "tsp")
    is <- tsp[2L]*tsp[3L]
    if(abs(tsp[3L] - round(tsp[3L])) < ts.eps &&
       abs(is - round(is)) < ts.eps) {
	is <- floor(tsp[2L]+ts.eps)
	fs <- floor(tsp[3L]*(tsp[2L] - is)+0.001)
	c(is, fs+1)
    }
    else tsp[2L]
}

frequency.default <- function(x, ...)
    if(!is.null(xtsp <- attr(x, "tsp"))) xtsp[3L] else 1

deltat.default <- function(x, ...)
    if(!is.null(xtsp <- attr(x, "tsp"))) 1/xtsp[3L] else 1

time.default <- function (x, offset = 0, ...)
{
    n <- if(is.matrix(x)) nrow(x) else length(x)
    xtsp <- attr(hasTsp(x), "tsp")
    y <- seq.int(xtsp[1L], xtsp[2L], length.out = n) + offset/xtsp[3L]
    tsp(y) <- xtsp
    y
}

time.ts <- function (x, ...) as.ts(time.default(x, ...))

cycle.default <- function(x, ...)
{
    p <- tsp(hasTsp(x))
    m <- round((p[1L] %% 1) * p[3L])
    x <- (1L:NROW(x) + m - 1) %% p[3L] + 1
    tsp(x) <- p
    x
}

cycle.ts <- function (x, ...) as.ts(cycle.default(x, ...))

print.ts <- function(x, calendar, ...)
{
    x <- as.ts(x)
    ## sanity check
    Tsp <- tsp(x)
    if(is.null(Tsp)) {
	warning("series is corrupt, with no 'tsp' attribute")
	print(unclass(x), ...)
	return(invisible(x))
    }
    nn <- 1 + round((Tsp[2L] - Tsp[1L]) * Tsp[3L])
    if(NROW(x) != nn) {
        warning(gettextf("series is corrupt: length %d with 'tsp' implying %d",
                         NROW(x), nn), domain=NA, call.=FALSE)
        calendar <- FALSE
    }
    fr.x <- frequency(x)
    if(missing(calendar))
	calendar <- any(fr.x == c(4,12)) && length(start(x)) == 2L
    if(!calendar) {
        if(fr.x != 1)
            cat("Time Series:\nStart =", deparse(start(x)),
                "\nEnd =", deparse(end(x)),
                "\nFrequency =", deparse(fr.x), "\n")
        else
            cat("Time Series:\nStart =", format(tsp(x)[1L]),
                "\nEnd =", format(tsp(x)[2L]),
                "\nFrequency =", deparse(fr.x), "\n")
    }
    print(.preformat.ts(x, calendar, ...), quote = FALSE, right = TRUE, ...)
    invisible(x)
}

## To be used in a  format.ts():
.preformat.ts <- function(x, calendar, ...)
{
    fr.x <- frequency(x)
    if(missing(calendar))
	calendar <- any(fr.x == c(4,12)) && length(start(x)) == 2L
    ## sanity check
    Tsp <- tsp(x)
    if(is.null(Tsp)) stop("series is corrupt, with no 'tsp' attribute")
    nn <- 1 + round((Tsp[2L] - Tsp[1L]) * Tsp[3L])
    if(NROW(x) != nn) {
        warning(gettextf("series is corrupt: length %d with 'tsp' implying %d",
                         NROW(x), nn), domain=NA, call.=FALSE)
        calendar <- FALSE
    }
    if(NCOL(x) == 1) { # could be 1-col matrix
        if(calendar) {
            if(fr.x > 1) {
                dn2 <-
                    if(fr.x == 12) month.abb
                    else if(fr.x == 4) {
                        c("Qtr1", "Qtr2", "Qtr3", "Qtr4")
                    } else paste0("p", 1L:fr.x)
                if(NROW(x) <= fr.x && start(x)[1L] == end(x)[1L]) {
                    ## not more than one period
                    dn1 <- start(x)[1L]
                    dn2 <- dn2[1 + (start(x)[2L] - 2 + seq_along(x))%%fr.x]
                    x <- matrix(format(x, ...), nrow = 1L , byrow = TRUE,
                                dimnames = list(dn1, dn2))
                } else { # more than one period
                    start.pad <- start(x)[2L] - 1
                    end.pad <- fr.x - end(x)[2L]
                    dn1 <- start(x)[1L]:end(x)[1L]
                    x <- matrix(c(rep.int("", start.pad), format(x, ...),
                                  rep.int("", end.pad)), ncol =  fr.x,
                                byrow = TRUE, dimnames = list(dn1, dn2))
                }
            } else { ## fr.x == 1
                tx <- time(x)
                attributes(x) <- NULL
                names(x) <- tx
            }
        } else { ##-- no 'calendar' --
            attr(x, "class") <- attr(x, "tsp") <- attr(x, "na.action") <- NULL
        }
    } else { # multi-column matrix
        rownames(x) <-
	    if(calendar && fr.x > 1) {
		tm <- time(x)
		t2 <- 1 + round(fr.x*((tm+0.001) %%1))
                ## protect people against themselves if they set options(digits=2)
                p1 <- format(floor(zapsmall(tm, digits = 7))) # yr
		if(fr.x == 12)
		    paste(month.abb[t2], p1)
		else
		    paste(p1, if(fr.x == 4) c("Q1", "Q2", "Q3", "Q4")[t2]
			  else format(t2))
	    } else
		format(time(x))
        attr(x, "class") <- attr(x, "tsp") <- attr(x, "na.action") <- NULL
    }
    x
}## {.preformat.ts}

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
                  cex.lab = par("cex.lab"), col.lab = par("col.lab"),
                  font.lab = par("font.lab"), cex.axis = par("cex.axis"),
                  col.axis = par("col.axis"), font.axis = par("font.axis"),
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
	    if(is.null(nm)) nm <- paste("Series", 1L:nser)
	    if(missing(nc)) nc <- if(nser > 4) 2 else 1
	    nr <- ceiling(nser/nc)

	    oldpar <- par(mar = mar.multi, oma = oma.multi, mfcol = c(nr, nc))
	    on.exit(par(oldpar))
	    for(i in 1L:nser) {
		plot.default(x[, i], axes = FALSE, xlab="", ylab="",
		     log = log, col = col, bg = bg, pch = pch, ann = ann,
		     type = "n", ...)
		panel(x[, i], col = col, bg = bg, pch = pch, type=type, ...)
		if(frame.plot) box(...)
		y.side <- if (i %% 2 || !yax.flip) 2 else 4
		do.xax <- i %% nr == 0 || i == nser
		if(axes) {
		    axis(y.side, xpd = NA, cex.axis = cex.axis,
			 col.axis = col.axis, font.axis = font.axis, ...)
		    if(do.xax)
			axis(1, xpd = NA, cex.axis = cex.axis,
			     col.axis = col.axis, font.axis = font.axis, ...)
		}
		if(ann) {
		    mtext(nm[i], y.side, line=3, cex=cex.lab, col=col.lab,
                          font=font.lab, ...)
		    if(do.xax)
			mtext(xlab, side=1, line=3, cex=cex.lab, col=col.lab,
                          font=font.lab, ...)
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

            dev.hold(); on.exit(dev.flush())
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
		     else if(all(tsp(x) == tsp(y)))
                         formatC(unclass(time(x)), width = 1)
		     else seq_along(xy$x),
		     col = col, cex = cex)
	    if(xy.lines)
		lines(xy, col = col, lty = lty, lwd = lwd,
		      type = if(do.lab) "c" else "l")
	    return(invisible())
	}
	## Else : no y, only x

	if(missing(ylab)) {
	    ylab <- colnames(x)
	    if(length(ylab) != 1L)
		ylab <- xlabel
	}
	## using xy.coords() mainly for the log treatment
	if(is.matrix(x)) {
	    k <- ncol(x)
	    tx <- time(x)
	    xy <- xy.coords(x = matrix(rep.int(tx, k), ncol = k),
			    y = x, log = log, setLab = FALSE)
	    xy$x <- tx
	}
	else xy <- xy.coords(x, NULL, log = log, setLab = FALSE)
	if(is.null(xlim)) xlim <- range(xy$x)
	if(is.null(ylim)) ylim <- range(xy$y[is.finite(xy$y)])
	plot.new()
	plot.window(xlim, ylim, log, ...)
	if(is.matrix(x)) {
	    for(i in seq_len(k))
		lines.default(xy$x, x[,i],
			      col = col[(i-1L) %% length(col) + 1L],
			      lty = lty[(i-1L) %% length(lty) + 1L],
			      lwd = lwd[(i-1L) %% length(lwd) + 1L],
			      bg  = bg [(i-1L) %% length(bg) + 1L],
			      pch = pch[(i-1L) %% length(pch) + 1L],
			      type = type)
	}
	else {
	    lines.default(xy$x, x, col = col[1L], bg = bg, lty = lty[1L],
			  lwd = lwd[1L], pch = pch[1L], type = type)
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
    xfreq <- xtsp[3L]
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
        warning("'frequency' not changed")
    }
    start <- if(is.null(start))
	xtsp[1L]
    else switch(length(start),
		start,
		start[1L] + (start[2L] - 1)/xfreq,
		stop("bad value for 'start'"))
    if(start < xtsp[1L]-ts.eps/xfreq && !extend) {
	start <- xtsp[1L]
	warning("'start' value not changed")
    }

    end <- if(is.null(end))
	xtsp[2L]
    else switch(length(end),
		end,
		end[1L] + (end[2L] - 1)/xfreq,
		stop("bad value for 'end'"))
    if(end > xtsp[2L]+ts.eps/xfreq && !extend) {
	end <- xtsp[2L]
	warning("'end' value not changed")
    }

    if(start > end)
	stop("'start' cannot be after 'end'")

    if(!extend) {
        if(all(abs(start - xtime) > ts.eps/xfreq))
            start <- xtime[(xtime > start) & ((start + 1/xfreq) > xtime)]

        if(all(abs(end - xtime) > ts.eps/xfreq))
            end <- xtime[(xtime < end) & ((end - 1/xfreq) < xtime)]

        i <- seq.int(trunc((start - xtsp[1L]) * xfreq + 1.5),
                     trunc((end - xtsp[1L]) * xfreq + 1.5), by = thin)
        y <- if(is.matrix(x)) x[i, , drop = FALSE] else x[i]
        ystart <- xtime[i[1L]]
        yend <- xtime[i[length(i)]]
        attr(y, "tsp") <- c(ystart, yend, yfreq)
    } else {
        ## first adjust start and end to the time base
        ## try to ensure that they are exactly n/xfreq
        stoff <- ceiling((start - xtsp[1L]) * xfreq - ts.eps)
        ystart <- (round(xtsp[1L]*xfreq) + stoff)/xfreq
        enoff <- floor((end - xtsp[2L]) * xfreq + ts.eps)
        yend <- (round(xtsp[2L]*xfreq) + enoff)/xfreq
        nold <- round(xfreq*(xtsp[2L] - xtsp[1L])) + 1
        ## both start and end could be outside time base
        ## and indeed the new ad old ranges might not intersect.
        i <- if(start > xtsp[2L]+ts.eps/xfreq || end < xtsp[1L] - ts.eps/xfreq)
            rep(nold+1, floor(1+(end-start)*xfreq + ts.eps))
        else {
            i0 <- 1+max(0, stoff); i1 <- nold + min(0, enoff)
            c(rep.int(nold+1, max(0, -stoff)),
              if(i0 <= i1) i0:i1,
              rep.int(nold+1, max(0, enoff)))
        }
        y <- if(is.matrix(x)) rbind(x, NA)[i, , drop = FALSE] else c(x, NA)[i]
        attr(y, "tsp") <- c(ystart, yend, xfreq)
        if(yfreq != xfreq) y <- Recall(y, frequency = yfreq)
    }
    y
}

window.ts <- function (x, ...) as.ts(window.default(x, ...))

`window<-` <- function(x, ..., value) UseMethod("window<-")

`window<-.ts` <- function(x, start, end, frequency, deltat, ..., value)
{
    xtsp <- tsp(x)
    m <- match.call(expand.dots = FALSE)
    m$value <- NULL
    m$extend <- TRUE
    ## need stats:: for non-standard evaluation
    m[[1L]] <- quote(stats::window)
    xx <- eval.parent(m)
    xxtsp <- tsp(xx)
    start <- xxtsp[1L]; end <- xxtsp[2L]
    if(start > end) stop("'start' > 'end'")
    if (start < xtsp[1L] || end > xtsp[2L]) {
        warning("extending time series when replacing values", call. = FALSE)
        x <- window(x, min(start, xtsp[1L]), max(end, xtsp[2L]), extend = TRUE)
    }
    xfreq <- xtsp[3L]
    xtimes <- round(xfreq*time(x))
    xxtimes <- round(xfreq * time(xx))

    ind <- match(xxtimes, xtimes)
    if(anyNA(ind)) stop("times to be replaced do not match")

    len <- length(ind)
    val_len <- NROW(value)
    if(!val_len) stop("no replacement values supplied")
    if(val_len > len) stop("too many replacement values supplied")
    if(val_len > 1L && (len %% val_len))
        stop("number of values supplied is not a sub-multiple of the number of values to be replaced")
    if(NCOL(x) == 1L) x[ind] <- value else x[ind, ] <- value
    x
}

`[.ts` <- function (x, i, j, drop = TRUE) {
    y <- NextMethod("[")
    if (missing(i))
	ts(y, start = start(x), frequency = frequency(x))
    else y
}

`[<-.ts` <- function (x, i, j, value) {
    y <- NextMethod("[<-")
    if (NROW(y) != NROW(x)) stop("only replacement of elements is allowed")
    y
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
                      innov = rand.gen(n, ...), n.start = NA,
                      start.innov = rand.gen(n.start, ...), ...)
{
    if(!is.list(model)) stop("'model' must be list")
    if(n <= 0L) stop("'n' must be strictly positive")
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
        if(length(ord) != 3L) stop("'model$order' must be of length 3")
        if(p != ord[1L]) stop("inconsistent specification of 'ar' order")
        if(q != ord[3L]) stop("inconsistent specification of 'ma' order")
        d <- ord[2L]
        if(d != round(d) || d < 0)
            stop("number of differences must be a positive integer")
    }
    if(!missing(start.innov) && length(start.innov) < n.start)
        stop(sprintf(ngettext(n.start,
                              "'start.innov' is too short: need %d point",
                              "'start.innov' is too short: need %d points"),
                     n.start), domain = NA)
    x <- ts(c(start.innov[seq_len(n.start)], innov[1L:n]), start = 1 - n.start)
    if(length(model$ma)) {
        x <- filter(x, c(1, model$ma), sides = 1L)
        x[seq_along(model$ma)] <- 0 # rather than NA
    }
    if(length(model$ar)) x <- filter(x, model$ar, method = "recursive")
    if(n.start > 0) x <- x[-(seq_len(n.start))]
    if(d > 0) x <- diffinv(x, differences = d)
    as.ts(x)
}
