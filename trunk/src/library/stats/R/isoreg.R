### Isotonic Regression --- original code is simplification of MASS' Shepard():
##
isoreg <- function(x, y=NULL)
{
    xy <- xy.coords(x,y)
    x <- xy$x
    isOrd <- (!is.null(xy$xlab) && xy$xlab == "Index") || !is.unsorted(x)
    if(!isOrd) {
        ord <- order(x)
	yo <- xy$y[ord]
    }
    z <- .Call("R_isoreg", if(isOrd)xy$y else yo, PACKAGE = "stats")
    structure(c(xy[c("x","y")], z[c("yf","yc","iKnots")],
                list(isOrd = isOrd, ord = if(!isOrd) ord,
                     call = match.call())),
	      class = "isoreg")
}

fitted.isoreg <- function(object, ...)
{
    if(object$isOrd) object$yf
    else object$yf[order(object$ord)]
}

residuals.isoreg <- function(object, ...) object$y - fitted(object)

print.isoreg <- function(x, digits = getOption("digits"), ...)
{
  cat("Isotonic regression from ", deparse(x$call), ",\n", sep="")
  cat("  with", length(x$iKnots), "knots / breaks at obs.nr.", x$iKnots, ";\n")
  if(x$isOrd) cat("  initially ordered 'x'\n")
  else { cat("  x ordering:"); str(x$ord) }
  cat("  and further components ")
  str(x[1:4], digits.d = 3 + max(0,digits - 7))
  invisible(x)
}

lines.isoreg <- function(x, col = "red", lwd = 1.5,
			 do.points = FALSE, cex = 1.5, pch = 13, ...)
{
    xx <- if(x$isOrd) x$x else x$x[x$ord]
    lines (xx, x$yf, col = col, lwd = lwd, type = "S")
    if(do.points)
	points(xx[x$iKnots], x$yf[x$iKnots], col = col, cex = cex, pch = pch)
    invisible()
}

plot.isoreg <-
    function(x, plot.type = c("single", "row.wise", "col.wise"),
	     main = paste("Isotonic regression", deparse(x$call)),
	     main2= "Cumulative Data and Convex Minorant",
	     xlab = "x0", ylab = "x$y",
	     par.fit = list(col = "red", cex = 1.5, pch = 13, lwd = 1.5),
	     mar = if(both) .1 + c(3.5,2.5,1,1) else par("mar"),
	     mgp = if(both) c(1.6, 0.7, 0) else par("mgp"),
	     grid = length(x$x) < 12,
	     ...)
{
    plot.type <- match.arg(plot.type)
    both <- plot.type != "single"
    if(both) {
	col.wise <- plot.type == "col.wise"
	if(!is.null(main)) main.wid <- 2
	op <- par(mfcol = if(col.wise) 1:2 else 2:1,
		  oma = c(0,0, main.wid, 0), mar = mar, mgp = mgp)
    } else
	op <- par(mar = mar, mgp = mgp)

    on.exit(par(op))

    xx <- if(x$isOrd) x$x else x$x[x$ord]
    x0 <- c(xx[1] - mean(diff(xx)), xx)# 1 pt left
    cy <- x$yc # = cumsum(c(0, x$y[ordered]))
    cf <- cumsum(c(0, x$yf))

    ##Dbg i <- abs(cy - cf) < 1e-10 * abs(cy + cf)## cy == cf
    ##Dbg if(!identical(which(i[-1]), x$iKnots))
    ##Dbg    warning("x$iKnots differs from which(i[-1]) ..")

    ## Plot of "Data" + Fit
    plot(x0, c(NA,x$y), ..., xlab = xlab, ylab = ylab, main = if(!both) main)
    lines (xx, x$yf, col = par.fit$col, lwd = par.fit$lwd, type = "S")
    points(xx[x$iKnots], x$yf[x$iKnots], col = par.fit$col,
           cex = par.fit$cex, pch = par.fit$pch)
    if(grid) grid()
    if(both) { ## Cumulative Plot
	plot (x0, cy, type = "n", xlab = xlab,
	      ylab = paste("cumsum(", ylab, ")", sep=""), ylim = range(cy, cf),
              ...)
        i <- 1L + x$iKnots
        lines(x0, cf, col = par.fit$col, lwd = par.fit$lwd)
        points(x0[i], cy[i], col = par.fit$col, cex = par.fit$cex,
               pch = par.fit$pch)
	if(grid) {
	    Agrid <- formals("grid")
	    abline(v = x0[i], col = Agrid$col, lty = Agrid$lty,
                   xpd = !col.wise)
	}
	points(x0[-1], cy[-1])# over draw
	if(!is.null(main2))
	    mtext(main2, cex = par("cex.main"),
		  col = par("col.main"), font = par("font.main"))
	if(!is.null(main))
	    mtext(main, side = 3, outer = TRUE, cex = par("cex.main"),
		  col = par("col.main"), font = par("font.main"))
    }
    invisible()
}
