### T. Dye <tdye@lava.net>, July 1999
### This code started life as spatial star plots by David A. Andrews.
### See http://www.stat.rice.edu/~andrewsd/software/software.html.

"stars" <-
function(x, full = TRUE, scale = TRUE, radius = TRUE,
	 labels = dimnames(x)[[1]], locations = NULL, xlimit = NULL,
	 ylimit = NULL, len = 1, colors = NULL, key.loc = NULL,
	 key.labels = NULL, draw.segments = FALSE, draw.axes = FALSE, ...) {

    if (is.data.frame(x))
	x <- as.matrix(x)
    else if (!is.matrix(x))
	stop("x must be a matrix or a data frame")
    if (!is.numeric(x))
	stop("data in x must be numeric")

    n.loc <- nrow(x)
    n.seg <- ncol(x)
    deg <- pi / 180			# segments only

    seg.colors <- if(!is.null(colors)) colors else 1:n.seg

    if (is.null(locations)) {		# make loc matrix
	mat.dim <- ceiling(sqrt(n.loc))
	temp.loc.1 <- rep(x=2.1* 1:mat.dim, times=mat.dim, length=n.loc)
	temp.loc.2 <- rep(x=2.1* mat.dim:1,
			  rep(x=mat.dim, times=mat.dim), length=n.loc)
	loc <- matrix(data=c(temp.loc.1,temp.loc.2),ncol=2)
    }
    else {
	if (!is.matrix(locations) || ncol(locations) != 2)
	    stop("locations must be a 2-column matrix.")
	loc <- .Alias(locations)
    }


    if ( n.loc != nrow(loc) )
	stop("number of rows of locations and x must be equal.")

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
    x[is.na(x)] <- 0

    if (scale)
	x <- sweep(x,2,apply(x,2,max),FUN="/")

    x <- x * len

    temp.xlim <-
	if(is.null(xlimit))
	    range(loc[,1] + max(x), loc[,1] - max(x)) else xlimit
    temp.ylim <-
	if(is.null(ylimit))
	    range(loc[,2] + max(x), loc[,2] - max(x)) else ylimit

    opar <- par(no.readonly = TRUE)
    on.exit(par(opar))

    ## The asp argument keeps everything square
    plot(0, type="n", ..., xlim=temp.xlim, ylim=temp.ylim,
	 xlab="", ylab="", asp = 1, axes = draw.axes)

    if ( draw.segments ) {
	## for each location, draw a segment diagram
	for ( i in 1:n.loc ) {
	    poly.x <- NA
	    poly.y <- NA
	    start.x.coord <- x[i,] * cos( angles ) + loc[i,1]
	    start.y.coord <- x[i,] * sin( angles ) + loc[i,2]
	    for (j in 1:n.seg) {
		poly.x <- c(poly.x,loc[i,1],start.x.coord[j])
		poly.y <- c(poly.y,loc[i,2],start.y.coord[j])

		next.angle <-
		    if(j < n.seg)
			angles[j+1]
		    else (if(full) 360 else 180) * deg

		k <- seq(from = angles[j], to = next.angle, by = deg)
		poly.x <- c(poly.x, x[i,j] * cos( k ) + loc[i,1], NA)
		poly.y <- c(poly.y, x[i,j] * sin( k ) + loc[i,2], NA)
	    }
	    par(lwd=0.25)
	    polygon(poly.x, poly.y, col = seg.colors)
	    par(lwd=1)
	    if (!is.null(labels))
		text(loc[i,1], loc[i,2] - if(full)max(x) else 0.1 * max(x),
		     labels[i], cex=0.5, adj=c(0.5,1))
	}
    } # Segment diagrams are drawn
    else { # Draw stars instead
	for ( i in 1:n.loc ) {
	    temp.x.coord <- x[i,] * cos( angles ) + loc[i,1]
	    temp.y.coord <- x[i,] * sin( angles ) + loc[i,2]
	    if ( radius ) {
		par(lwd=0.25)
		segments(rep(loc[i,1],n.seg),
			 rep(loc[i,2],n.seg),
			 temp.x.coord, temp.y.coord)
		par(lwd=1)
	    }
	    lines(c(temp.x.coord, temp.x.coord[1]),
		  c(temp.y.coord, temp.y.coord[1]), lwd=0.25)
	    if (!is.null(labels))
		text(loc[i,1], loc[i,2] - if(full)max(x) else 0.1 * max(x),
		     labels[i], cex=0.5, adj=c(0.5,1))
	}
    }

    if ( ! is.null(key.loc) ) {		# Draw unit key
	if ( draw.segments ) {
	    key.x <- NA
	    key.y <- NA
	    key.x.coord <- cos( angles ) * len + key.loc[1]
	    key.y.coord <- sin( angles ) * len + key.loc[2]
	    for (j in 1:n.seg){
		key.x <- c(key.x,key.loc[1],key.x.coord[j])
		key.y <- c(key.y,key.loc[2],key.y.coord[j])
		k <- angles[j] + deg
		next.angle <-
		    if (j < n.seg)
			angles[j+1]
		    else (if(full) 360 else 180) * deg

		while (k < next.angle) {
		    key.x <- c(key.x, len * cos( k ) + key.loc[1])
		    key.y <- c(key.y, len * sin( k ) + key.loc[2])
		    k <- k + deg
		}
		key.x <- c(key.x, len * cos( next.angle ) + key.loc[1], NA)
		key.y <- c(key.y, len * sin( next.angle ) + key.loc[2], NA)
	    }
	    par(lwd=0.25)
	    polygon(key.x, key.y, col = seg.colors)
	    par(lwd=1)
	}
	else { # draw a star
	    temp.x.coord <- cos( angles ) * len + key.loc[1]
	    temp.y.coord <- sin( angles ) * len + key.loc[2]
	    par(lwd=0.25)
	    if ( radius )
		segments(rep(key.loc[1],n.seg), rep(key.loc[2],n.seg),
			 temp.x.coord, temp.y.coord)
	    lines(c(temp.x.coord, temp.x.coord[1]),
		  c(temp.y.coord, temp.y.coord[1]))
	    par(lwd=1)
	}
	if (is.null(key.labels))
	    key.labels <- dimnames(x)[[2]]

	lab.angl <- angles +
	    if(draw.segments) (angles[2] - angles[1]) / 2 else 0

	label.x.coord <- cos( lab.angl ) * len * 1.1 + key.loc[1]
	label.y.coord <- sin( lab.angl ) * len * 1.1 + key.loc[2]

	for (k in 1:n.seg) {
	    text.adj <-
		if (lab.angl[k] < (90 * deg) || lab.angl[k] > (270 * deg))
		    0
		else if (lab.angl[k] > (90 * deg) && lab.angl[k] < (270 * deg))
		    1
		else
		    0.5

	    if (lab.angl[k] <= (90 * deg))
		text.adj <-
		    c(text.adj, 0.5 * (1 - lab.angl[k] / (90 * deg)))
	    else if (lab.angl[k] > (90 * deg) & lab.angl[k] <= (270 * deg))
		text.adj <-
		    c(text.adj, (lab.angl[k] - (90 * deg)) / (180 * deg))
	    else if (lab.angl[k] > (270 * deg))
		text.adj <-
		    c(text.adj, 1 - (0.5 * (lab.angl[k] - (270 * deg)) /
				     (90 * deg)))
	    text.default(x=label.x.coord[k], y=label.y.coord[k],
			 labels= key.labels[k], cex = 0.5, adj = text.adj)
	}
    } # Unit key is drawn and labelled
    invisible()
}
