### This code started life as spatial star plots by David A. Andrews.
### See http://www.udallas.edu:8080/~andrews/software/software.html
### T. Dye <tdye@lava.net>, July 1999;  many improvements by MM

stars <-
function(x, full = TRUE, scale = TRUE, radius = TRUE,
	 labels = dimnames(x)[[1]], locations = NULL,
         nrow = NULL, ncol = NULL, len = 1,
         key.loc = NULL, key.labels = dimnames(x)[[2]], key.xpd = TRUE,
         xlim = NULL, ylim = NULL, flip.labels = NULL,
         draw.segments = FALSE, col.segments = 1:n.seg,
         col.stars = NA,
         axes = FALSE, frame.plot = axes,
         main = NULL, sub = NULL, xlab = "", ylab = "",
         cex = 0.8, lwd = 0.25, lty = par("lty"), xpd = FALSE,
         mar = pmin(par("mar"),
                    1.1+ c(2*axes+ (xlab != ""), 2*axes+ (ylab != ""), 1,0)),
         add=FALSE, plot=TRUE, ...)
{
    if (is.data.frame(x))
	x <- data.matrix(x)
    else if (!is.matrix(x))
	stop("x must be a matrix or a data frame")
    if (!is.numeric(x))
	stop("data in x must be numeric")

    n.loc <- nrow(x)
    n.seg <- ncol(x)

    if (is.null(locations)) { ## Default (x,y) locations matrix
	if(is.null(nrow))
            nrow <- ceiling(if(!is.numeric(ncol)) sqrt(n.loc) else n.loc/ncol)
	if(is.null(ncol))
            ncol <- ceiling(n.loc/nrow)
        if(nrow * ncol < n.loc)
            stop("nrow * ncol <  number of observations")
        ff <- if(!is.null(labels)) 2.3 else 2.1
        locations <- expand.grid(ff * 1:ncol, ff * nrow:1)[1:n.loc, ]
        if(!is.null(labels) && (missing(flip.labels) ||
                                !is.logical(flip.labels)))
            flip.labels <- ncol * mean(nchar(labels, type="c")) > 30
    }
    else {
        if (is.numeric(locations) && length(locations) == 2) {
            ## all stars around the same origin
            locations <- cbind(rep.int(locations[1],n.loc),
                               rep.int(locations[2],n.loc))
            if(!missing(labels) && n.loc > 1)
                warning("labels don't make sense for a single location")
            else labels <- NULL
        }
        else {
            if (is.data.frame(locations))
                locations <- data.matrix(locations)
            if (!is.matrix(locations) || ncol(locations) != 2)
                stop("locations must be a 2-column matrix.")
            if (n.loc != nrow(locations))
                stop("number of rows of locations and x must be equal.")
        }
        if(missing(flip.labels) || !is.logical(flip.labels))
            flip.labels <- FALSE # have no grid
    }
    xloc <- locations[,1]
    yloc <- locations[,2]
    ## Angles start at zero and pace around the circle counter
    ## clock-wise in equal increments.
    angles <-
	if(full)
	    seq(0, 2*pi, length=n.seg+1)[-(n.seg+1)]
	else if (draw.segments)
	    seq(0, pi, length=n.seg+1)[-(n.seg+1)]
	else
	    seq(0, pi, length=n.seg)

    if (length(angles) != n.seg)
	stop("length(angles) must be the same as ncol(x)")

    ## Missing values are treated as 0
    if (scale) {
        x <- apply(x, 2, function(x)
                   (x - min(x, na.rm = TRUE))/diff(range(x, na.rm = TRUE)))
#	x <- sweep(x,2,apply(x,2,max), FUN="/")
    }
    ## Missing values are treated as 0
    x[is.na(x)] <- 0
    mx <- max(x <- x * len)

    if(is.null(xlim)) xlim <- range(xloc) + c(-mx, mx)
    if(is.null(ylim)) ylim <- range(yloc) + c(-mx, mx)

    deg <- pi / 180

    ## The asp argument keeps everything (the symbols!) square
    op <- par(mar = mar, xpd = xpd) ; on.exit(par(op))
    if(!add)
        plot(0, type="n", ..., xlim=xlim, ylim=ylim,
             main = main, sub = sub, xlab = xlab, ylab=ylab,
             asp = 1, axes = axes)

    if(!plot)
        return()

    s.x <- xloc + x * rep.int(cos(angles), rep.int(n.loc,n.seg))
    s.y <- yloc + x * rep.int(sin(angles), rep.int(n.loc,n.seg))

    if ( draw.segments ) {
        aangl <- c(angles, if(full)2*pi else pi)
	for (i in 1:n.loc) { ## for each location, draw a segment diagram
	    px <- py <- numeric()
	    for (j in 1:n.seg) {
		k <- seq(from = aangl[j], to = aangl[j+1], by = 1*deg)
		px <- c(px, xloc[i], s.x[i,j], x[i,j]*cos(k) + xloc[i], NA)
		py <- c(py, yloc[i], s.y[i,j], x[i,j]*sin(k) + yloc[i], NA)
	    }
	    polygon(px, py, col = col.segments, lwd=lwd, lty=lty)
	}
    } # Segment diagrams

    else { # Draw stars instead
	for (i in 1:n.loc) {
	    polygon(s.x[i,], s.y[i,], lwd=lwd, lty=lty, col = col.stars[i])
	    if (radius)
		segments(rep.int(xloc[i],n.seg),
			 rep.int(yloc[i],n.seg),
			 s.x[i,], s.y[i,], lwd=lwd, lty=lty)
	}
    }

    if(!is.null(labels)) {
        ## vertical text offset from center
        y.off <- mx * (if(full) 1 else 0.1)
        if(flip.labels)
            y.off <- y.off + cex*par("cxy")[2] *
                ((1:n.loc)%%2 - if(full) .4 else 0)
        ##DBG cat("mx=",format(mx),"y.off:"); str(y.off)
        text(xloc, yloc - y.off, labels, cex=cex, adj=c(0.5, 1))
    }

    if ( !is.null(key.loc) ) { ## Draw unit key

        ## usually allow drawing outside plot region:
        par(xpd = key.xpd) # had `xpd' already above
        key.x <- len * cos(angles) + key.loc[1]
        key.y <- len * sin(angles) + key.loc[2]
	if (draw.segments) {
	    px <- py <- numeric()
	    for (j in 1:n.seg) {
		k <- seq(from = aangl[j], to = aangl[j+1], by = 1*deg)
		px <- c(px, key.loc[1], key.x[j], len * cos(k) + key.loc[1], NA)
		py <- c(py, key.loc[2], key.y[j], len * sin(k) + key.loc[2], NA)
	    }
	    polygon(px, py, col = col.segments, lwd=lwd, lty=lty)
	}
	else { # draw unit star
	    polygon(key.x, key.y, lwd=lwd, lty=lty)
	    if (radius)
		segments(rep.int(key.loc[1],n.seg), rep.int(key.loc[2],n.seg),
			 key.x, key.y, lwd=lwd, lty=lty)
	}

        ## Radial Labeling -- should this be a standalone function ?
	lab.angl <- angles +
            if(draw.segments) (angles[2] - angles[1]) / 2 else 0
	label.x <- 1.1 * len * cos(lab.angl) + key.loc[1]
	label.y <- 1.1 * len * sin(lab.angl) + key.loc[2]
        ## Maybe do the following without loop {need not use adj but ..)!
	for (k in 1:n.seg) {
	    text.adj <-
                c(## horizontal
                  if      (lab.angl[k] < 90*deg || lab.angl[k] > 270*deg) 0
                  else if (lab.angl[k] > 90*deg && lab.angl[k] < 270*deg) 1
                  else 0.5,
                  ## vertical
                  if (lab.angl[k] <= 90*deg) (1 - lab.angl[k] / (90*deg)) /2
                  else if (lab.angl[k] <= 270*deg)
                  (lab.angl[k] - 90*deg) / (180*deg)
                  else ## lab.angl[k] > 270*deg
                  1 - (lab.angl[k] - 270*deg) / (180*deg)
                  )
	    text(label.x[k], label.y[k],
                 labels= key.labels[k], cex = cex, adj = text.adj)
	}
    } # Unit key is drawn and labelled

    if (frame.plot) box(...)

    invisible(locations)
}
