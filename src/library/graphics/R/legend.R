legend <-
function(x, y = NULL, legend, fill=NULL, col = "black", lty, lwd, pch,
	 angle = 45, density = NULL, bty = "o", bg = par("bg"),
         pt.bg = NA, cex = 1, pt.cex = cex, pt.lwd = lwd,
	 xjust = 0, yjust = 1, x.intersp = 1, y.intersp = 1, adj = c(0, 0.5),
	 text.width = NULL, text.col = par("col"),
         merge = do.lines && has.pch, trace = FALSE,
	 plot = TRUE, ncol = 1, horiz = FALSE)
{
    ## the 2nd arg may really be `legend'
    if(missing(legend) && !missing(y) &&
       (is.character(y) || is.expression(y))) {
        legend <- y
        y <- NULL
    }
    mfill <- !missing(fill) || !missing(density)

    xy <- xy.coords(x, y); x <- xy$x; y <- xy$y
    nx <- length(x)
    if (nx < 1 || nx > 2) stop("invalid coordinate lengths")

    xlog <- par("xlog")
    ylog <- par("ylog")

    rect2 <- function(left, top, dx, dy, density = NULL, angle, ...) {
	r <- left + dx; if(xlog) { left <- 10^left; r <- 10^r }
	b <- top  - dy; if(ylog) {  top <- 10^top;  b <- 10^b }
	rect(left, top, r, b, angle = angle, density = density, ...)
    }
    segments2 <- function(x1, y1, dx, dy, ...) {
	x2 <- x1 + dx; if(xlog) { x1 <- 10^x1; x2 <- 10^x2 }
	y2 <- y1 + dy; if(ylog) { y1 <- 10^y1; y2 <- 10^y2 }
	segments(x1, y1, x2, y2, ...)
    }
    points2 <- function(x, y, ...) {
	if(xlog) x <- 10^x
	if(ylog) y <- 10^y
	points(x, y, ...)
    }
    text2 <- function(x, y, ...) {
	##--- need to adjust  adj == c(xadj, yadj) ?? --
	if(xlog) x <- 10^x
	if(ylog) y <- 10^y
	text(x, y, ...)
    }
    if(trace)
	catn <- function(...)
	    do.call("cat", c(lapply(list(...),formatC), list("\n")))

    cin <- par("cin")
    Cex <- cex * par("cex")		# = the `effective' cex for text

    if(is.null(text.width))
	text.width <- max(strwidth(legend, units="user", cex=cex))
    else if(!is.numeric(text.width) || text.width < 0)
	stop("text.width must be numeric, >= 0")

    xc <- Cex * xinch(cin[1], warn.log=FALSE)# [uses par("usr") and "pin"]
    yc <- Cex * yinch(cin[2], warn.log=FALSE)

    xchar  <- xc
    yextra <- yc * (y.intersp - 1)
    ymax   <- max(yc, strheight(legend, units="user", cex=cex))
    ychar <- yextra + ymax
    if(trace) catn("  xchar=", xchar, "; (yextra,ychar)=", c(yextra,ychar))

    if(mfill) {
	##= sizes of filled boxes.
	xbox <- xc * 0.8
	ybox <- yc * 0.5
	dx.fill <- xbox ## + x.intersp*xchar
    }
    do.lines <- (!missing(lty) && (is.character(lty) || any(lty > 0))
		 ) || !missing(lwd)
    n.leg <- if(is.call(legend)) 1 else length(legend)

    ## legends per column:
    n.legpercol <-
	if(horiz) {
	    if(ncol != 1)
		warning(
		    "horizontal specification overrides: Number of columns := ",
			n.leg)
	    ncol <- n.leg
	    1
	} else ceiling(n.leg / ncol)

    if(has.pch <- !missing(pch) && length(pch) > 0) {
	if(is.character(pch) && !is.na(pch[1]) && nchar(pch[1]) > 1) {
	    if(length(pch) > 1)
		warning("Not using pch[2..] since pch[1] has multiple chars")
	    np <- nchar(pch[1])
	    pch <- substr(rep.int(pch[1], np), 1:np, 1:np)
	}
	if(!merge) dx.pch <- x.intersp/2 * xchar
    }
    x.off <- if(merge) -0.7 else 0

    ##- Adjust (x,y) :
    if (xlog) x <- log10(x)
    if (ylog) y <- log10(y)

    if(nx == 2) {
	## (x,y) are specifiying OPPOSITE corners of the box
	x <- sort(x)
	y <- sort(y)
	left <- x[1]
	top  <- y[2]
	w <- diff(x)# width
	h <- diff(y)# height
	w0 <- w/ncol # column width

	x <- mean(x)
	y <- mean(y)
	if(missing(xjust)) xjust <- 0.5
	if(missing(yjust)) yjust <- 0.5

    }
    else {## nx == 1
	## -- (w,h) := (width,height) of the box to draw -- computed in steps
	h <- n.legpercol * ychar + yc
	w0 <- text.width + (x.intersp + 1) * xchar
	if(mfill)	w0 <- w0 + dx.fill
	if(has.pch && !merge)	w0 <- w0 + dx.pch
	if(do.lines)		w0 <- w0 + (2+x.off) * xchar
	w <- ncol*w0 + .5* xchar
	##-- (w,h) are now the final box width/height.
	left <- x      - xjust	* w
	top  <- y + (1 - yjust) * h
    }

    if (plot && bty != "n") { ## The legend box :
	if(trace)
	    catn("  rect2(",left,",",top,", w=",w,", h=",h,", ...)",sep="")
	rect2(left, top, dx = w, dy = h, col = bg, density = NULL)
    }
    ## (xt[],yt[]) := `current' vectors of (x/y) legend text
    xt <- left + xchar + (w0 * rep.int(0:(ncol-1),
                                       rep.int(n.legpercol,ncol)))[1:n.leg]
    yt <- top - (rep.int(1:n.legpercol,ncol)[1:n.leg]-1) * ychar -
        0.5 * yextra - ymax

    if (mfill) {		#- draw filled boxes -------------
	if(plot) {
	    fill <- rep(fill, length.out = n.leg)
	    rect2(left = xt, top=yt+ybox/2, dx = xbox, dy = ybox,
		  col = fill,
                  density = density, angle = angle, border = "black")
	}
	xt <- xt + dx.fill
    }
    if(plot && (has.pch || do.lines))
	col <- rep(col, length.out = n.leg)

    if(missing(lwd)) lwd <- par("lwd")
    force(pt.lwd)
    if (do.lines) {			#- draw lines ---------------------
	seg.len <- 2 # length of drawn segment, in xchar units
	if(missing(lty)) lty <- 1
	ok.l <- !is.na(lty) & (is.character(lty) | lty > 0)
	lty <- rep(lty, length.out = n.leg)
	lwd <- rep(lwd, length.out = n.leg)
	if(trace)
	    catn("  segments2(",xt[ok.l] + x.off*xchar, ",", yt[ok.l],
		 ", dx=", seg.len*xchar, ", dy=0, ...)")
	if(plot)
	    segments2(xt[ok.l] + x.off*xchar, yt[ok.l], dx= seg.len*xchar, dy=0,
		      lty = lty[ok.l], lwd = lwd[ok.l], col = col[ok.l])
	# if (!merge)
	xt <- xt + (seg.len+x.off) * xchar
    }
    if (has.pch) {			#- draw points -------------------
	pch   <- rep(pch, length.out = n.leg)
	pt.bg <- rep(pt.bg, length.out = n.leg)
	pt.cex<- rep(pt.cex, length.out = n.leg)
	ok <- !is.na(pch) & (is.character(pch) | pch >= 0)
	x1 <- (if(merge) xt-(seg.len/2)*xchar else xt)[ok]
	y1 <- yt[ok]
	if(trace)
	    catn("  points2(", x1,",", y1,", pch=", pch[ok],", ...)")
	if(plot)
	    points2(x1, y1, pch = pch[ok], col = col[ok],
                    cex = pt.cex[ok], bg = pt.bg[ok], lwd = pt.lwd)
	if (!merge) xt <- xt + dx.pch
    }

    xt <- xt + x.intersp * xchar
    if(plot)
	text2(xt, yt, labels = legend, adj = adj, cex = cex, col = text.col)

    invisible(list(rect = list(w = w, h = h, left = left, top = top),
		   text = list(x = xt, y = yt)))
}
