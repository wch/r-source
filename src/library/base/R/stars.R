### T. Dye <tdye@lava.net>, July 1999
### This code started life as spatial star plots by David A. Andrews.
### See http://www.udallas.edu:8080/~andrews/software/software.html

stars <-
function(x, full = TRUE, scale = TRUE, radius = TRUE,
	 labels = dimnames(x)[[1]],
         locations = NULL, xlim = NULL, ylim = NULL, len = 1,
         colors = NULL,
         key.loc = NULL, key.labels = NULL, key.xpd = TRUE,
         draw.segments = FALSE, axes = FALSE,
         cex = 0.8, lwd = 0.25, ...) 
{
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
	md <- ceiling(sqrt(n.loc)) # =>  md^2 >= n.loc
        loc0 <- 2.1* 1:md
        loc <- expand.grid(loc0, rev(loc0))[1:n.loc, ]
    }
    else {
        if (is.numeric(locations) && length(locations) == 2) {
            ## all stars around the same origin
            loc <- cbind(rep(locations[1],n.loc),
                         rep(locations[2],n.loc))
            if(!missing(labels) && n.loc > 1)
                warning("labels don't make sense for a single location")
            else labels <- NULL
        }
        else {
            if (is.data.frame(locations))
                locations <- data.matrix(locations)
            if (!is.matrix(locations) || ncol(locations) != 2)
                stop("locations must be a 2-column matrix.")
            loc <- .Alias(locations)
            if (n.loc != nrow(loc))
                stop("number of rows of locations and x must be equal.")
        }
    }

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

    if (scale) {
	x <- sweep(x,2,apply(x,2,max),FUN="/")
	## Columns of 0s will put NAs in x, next line gets rid of them
	x[is.na(x)] <- 0
    }

    x <- x * len

    if(is.null(xlim)) xlim <- range(loc[,1] + max(x), loc[,1] - max(x))
    if(is.null(ylim)) ylim <- range(loc[,2] + max(x), loc[,2] - max(x))

    ## The asp argument keeps everything square
    plot(0, type="n", ..., xlim=xlim, ylim=ylim,
	 xlab="", ylab="", asp = 1, axes = axes)

    if ( draw.segments ) {
	## for each location, draw a segment diagram
	for ( i in 1:n.loc ) {
	    poly.x <- NA
	    poly.y <- NA
	    start.x <- x[i,] * cos( angles ) + loc[i,1]
	    start.y <- x[i,] * sin( angles ) + loc[i,2]

### FIXME : we can do without the following inner loop !

	    for (j in 1:n.seg) {
		poly.x <- c(poly.x,loc[i,1],start.x[j])
		poly.y <- c(poly.y,loc[i,2],start.y[j])

		next.angle <-
		    if(j < n.seg)
			angles[j+1]
		    else (if(full) 360 else 180) * deg

		k <- seq(from = angles[j], to = next.angle, by = deg)
		poly.x <- c(poly.x, x[i,j] * cos( k ) + loc[i,1], NA)
		poly.y <- c(poly.y, x[i,j] * sin( k ) + loc[i,2], NA)
	    }
	    polygon(poly.x, poly.y, col = seg.colors, lwd=lwd)
	    if (!is.null(labels))
		text(loc[i,1], loc[i,2] - if(full)max(x) else 0.1 * max(x),
		     labels[i], cex=cex, adj=c(0.5,1))
	}
    } # Segment diagrams

    else { # Draw stars instead
	for ( i in 1:n.loc ) {
	    temp.x <- x[i,] * cos( angles ) + loc[i,1]
	    temp.y <- x[i,] * sin( angles ) + loc[i,2]
	    if (radius)
		segments(rep(loc[i,1],n.seg),
			 rep(loc[i,2],n.seg),
			 temp.x, temp.y, lwd=lwd)
	    lines(c(temp.x, temp.x[1]),
		  c(temp.y, temp.y[1]), lwd=lwd)
	    if (!is.null(labels))
		text(loc[i,1], loc[i,2] - if(full)max(x) else 0.1 * max(x),
		     labels[i], cex=cex, adj=c(0.5,1))
	}
    }

    if ( !is.null(key.loc) ) { ## Draw unit key

        ## allow drawing outside plot region (inside figure region):
        op <- par(xpd = key.xpd); on.exit(par(op))
        key.x.coord <- cos( angles ) * len + key.loc[1]
        key.y.coord <- sin( angles ) * len + key.loc[2]
	if ( draw.segments ) {
	    key.x <- NA
	    key.y <- NA
	    for (j in 1:n.seg){
		key.x <- c(key.x,key.loc[1],key.x.coord[j])
		key.y <- c(key.y,key.loc[2],key.y.coord[j])
		k <- angles[j] + deg
		next.angle <-
		    if (j < n.seg) angles[j+1]
		    else (if(full) 360 else 180) * deg

		while (k < next.angle) {
		    key.x <- c(key.x, len * cos( k ) + key.loc[1])
		    key.y <- c(key.y, len * sin( k ) + key.loc[2])
		    k <- k + deg
		}
		key.x <- c(key.x, len * cos( next.angle ) + key.loc[1], NA)
		key.y <- c(key.y, len * sin( next.angle ) + key.loc[2], NA)
	    }
	    polygon(key.x, key.y, col = seg.colors, lwd=lwd)
	}
	else { # draw unit star
	    if ( radius )
		segments(rep(key.loc[1],n.seg), rep(key.loc[2],n.seg),
			 key.x.coord, key.y.coord, lwd=lwd)
	    lines(c(key.x.coord, key.x.coord[1]),
		  c(key.y.coord, key.y.coord[1]), lwd=lwd)
	}
	if (is.null(key.labels))
	    key.labels <- dimnames(x)[[2]]

	lab.angl <- angles +
	    if(draw.segments) (angles[2] - angles[1]) / 2 else 0

	label.x <- cos( lab.angl ) * len * 1.1 + key.loc[1]
	label.y <- sin( lab.angl ) * len * 1.1 + key.loc[2]

        ##-- FIXME : Do the following witout loop !
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
    invisible()
}
