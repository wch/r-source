
## Functions to calculate a set of points around the perimeter
## (or along the length) of a grob

grobCoords <- function(x, closed, ...) {
    UseMethod("grobCoords")
}

emptyCoords <- list(x = 0, y = 0)

isEmptyCoords <- function(coords) {
    identical(coords, emptyCoords)
}

grobCoords.grob <- function(x, closed, ...) {
    vp <- x$vp
    trans <- current.transform()
    # Same set up as drawGrob()
    dlon <- grid.Call(C_setDLon, FALSE)
    on.exit(grid.Call(C_setDLon, dlon))
    tempgpar <- grid.Call(C_getGPar)
    on.exit(grid.Call(C_setGPar, tempgpar), add=TRUE)
    # Same drawing context set up as drawGrob()
    # including enforcing the drawing context
    x <- preDraw(x)
    # Same drawing content set up as drawGrob() ...
    x <- makeContent(x)
    ## Does this grob change the viewport ?
    ## (including has preDraw() changed the viewport)
    vpgrob <- !is.null(x$vp) || !identical(vp, x$vp)
    # BUT NO DRAWING
    ## Polygon outline in inches
    pts <- grobPoints(x, closed, ...)
    if (vpgrob && !isEmptyCoords(pts)) {
        ## Calc locations on device
        pts <- lapply(pts,
                      function(p) {
                          deviceLoc(unit(p$x, "in"), unit(p$y, "in"),
                                    valueOnly=TRUE)
                      })
    }
    # Same context clean up as drawGrob()
    postDraw(x)
    if (vpgrob && !isEmptyCoords(pts)) {
        ## Transform back to locations
        pts <- lapply(pts,
                      function(p) {
                          ptsMatrix <- cbind(p$x, p$y, 1) %*% solve(trans)
                          list(x=ptsMatrix[,1], y=ptsMatrix[,2])
                      })
    }
    pts
}

## "gList"s
grobCoords.gList <- function(x, closed, ...) {
    ## Some children may produce list of lists
    coords <- lapply(x, grobCoords, closed, ...)
    coordLists <- lapply(coords,
                        function(p) {
                            if ("x" %in% names(p)) {
                                list(p)
                            } else {
                                p
                            }
                        })
    do.call("c", coordLists)
}

## "gTree"s
grobCoords.gTree <- function(x, closed, ...) {
    vp <- x$vp
    trans <- current.transform()
    # Same set up as drawGTree()
    dlon <- grid.Call(C_setDLon, FALSE)
    on.exit(grid.Call(C_setDLon, dlon))
    tempgrob <- grid.Call(C_getCurrentGrob)
    tempgpar <- grid.Call(C_getGPar)
    on.exit({ grid.Call(C_setGPar, tempgpar)
              grid.Call(C_setCurrentGrob, tempgrob)
            }, add=TRUE)
    # Same drawing context set up as drawGTree(),
    # including enforcing the drawing context
    x <- preDraw(x)
    # Same drawing content set up as drawGTree() ...
    x <- makeContent(x)
    ## Does this grob change the viewport ?
    ## (including has preDraw() changed the viewport)
    vpgrob <- !is.null(x$vp) || !identical(vp, x$vp)
    ## Polygon outline in inches
    pts <- grobCoords(x$children[x$childrenOrder], closed, ...)
    if (vpgrob && !isEmptyCoords(pts)) {
        ## Calc locations on device
        pts <- lapply(pts,
                      function(p) {
                          deviceLoc(unit(p$x, "in"), unit(p$y, "in"),
                                    valueOnly=TRUE)
                      })
    }
    # Same context clean up as drawGTree()
    postDraw(x)
    if (vpgrob && !isEmptyCoords(pts)) {
        ## Transform back to locations
        pts <- lapply(pts,
                      function(p) {
                          ptsMatrix <- cbind(p$x, p$y, 1) %*% solve(trans)
                          list(x=ptsMatrix[,1], y=ptsMatrix[,2])
                      })
    }
    pts
}

grobPoints <- function(x, closed, ...) {
    UseMethod("grobPoints")
}

## Currently covers gLists and gTrees
grobPoints.default <- function(x, closed, ...) {
    emptyCoords
}

grobPoints.move.to <- function(x, closed, ...) {
    emptyCoords
}

grobPoints.line.to <- function(x, closed, ...) {
    emptyCoords
}

grobPoints.circle <- function(x, closed, ..., n=100) {
    if (closed) {
        cx <- convertX(x$x, "in", valueOnly=TRUE)
        cy <- convertY(x$y, "in", valueOnly=TRUE)
        r <- min(convertWidth(x$r, "in", valueOnly=TRUE),
                 convertHeight(x$r, "in", valueOnly=TRUE))
        t <- seq(0, 2*pi, length.out=n+1)[-(n+1)]
        ## Recycle via cbind()
        circs <- cbind(cx, cy, r)
        n <- nrow(circs)
        lapply(1:n,
               function(i) {
                   list(x=circs[i, 1] + circs[i, 3]*cos(t),
                        y=circs[i, 2] + circs[i, 3]*sin(t))
               })
    } else {
        emptyCoords
    }
}

grobPoints.lines <- function(x, closed, ..., n=100) {
    if (closed) {
        emptyCoords
    } else {
        xx <- convertX(x$x, "in", valueOnly=TRUE)
        yy <- convertY(x$y, "in", valueOnly=TRUE)
        list(list(x=xx, y=yy))
    }
}

grobPoints.polyline <- function(x, closed, ...) {
    if (closed) {
        emptyCoords
    } else {
        ## polylineGrob() ensures that x/y same length
        xx <- convertX(x$x, "in", valueOnly=TRUE)
        yy <- convertY(x$y, "in", valueOnly=TRUE)
        pts <- list(x=xx, y=yy)
        if (is.null(x$id) && is.null(x$id.lengths)) {
            list(pts)
        } else {
            if (is.null(x$id)) {
                n <- length(x$id.lengths)
                id <- rep(1L:n, x$id.lengths)
            } else {
                n <- length(unique(x$id))
                id <- x$id
            }
            if (n > 1) {
                split(as.data.frame(pts), id)
            } else {
                list(pts)
            }
        }
    }    
}

grobPoints.polygon <- function(x, closed, ...) {
    if (closed) {
        ## polygonGrob() ensures that x/y same length
        xx <- convertX(x$x, "in", valueOnly=TRUE)
        yy <- convertY(x$y, "in", valueOnly=TRUE)
        pts <- list(x=xx, y=yy)
        if (is.null(x$id) && is.null(x$id.lengths)) {
            list(pts)
        } else {
            if (is.null(x$id)) {
                n <- length(x$id.lengths)
                id <- rep(1L:n, x$id.lengths)
            } else {
                n <- length(unique(x$id))
                id <- x$id
            }
            if (n > 1) {
                split(as.data.frame(pts), id)
            } else {
                list(pts)
            }
        }
    } else {
        emptyCoords
    }
}

xyListFromMatrix <- function(m, xcol, ycol) {
    n <- nrow(m)
    lapply(1:n,
           function(i) {
               list(x=m[i, xcol], y=m[i, ycol])
           })
}

grobPoints.pathgrob <- function(x, closed, ...) {
    if (closed) {
        ## pathGrob() ensures that x/y same length
        xx <- convertX(x$x, "in", valueOnly=TRUE)
        yy <- convertY(x$y, "in", valueOnly=TRUE)
        pts <- list(x=xx, y=yy)
        hasMultiple <- !(is.null(x$pathId) && is.null(x$pathId.lengths))
        if (hasMultiple) {
            if (is.null(x$pathId)) {
                n <- length(x$pathId.lengths)
                pathId <- rep(1L:n, x$pathId.lengths)
            } else {
                pathId <- x$pathId
            }
        }
        if (is.null(x$id) && is.null(x$id.lengths)) {
            if (hasMultiple) {
                split(as.data.frame(pts), pathId)
            } else {
                list(pts)
            }
        } else {
            if (is.null(x$id)) {
                n <- length(x$id.lengths)
                id <- rep(1L:n, x$id.lengths)
            } else {
                n <- length(unique(x$id))
                id <- x$id
            }
            if (hasMultiple) {
                split(as.data.frame(pts), list(id, pathId))
            } else {
                split(as.data.frame(pts), id)
            }
        }
    } else {
        emptyCoords
    }
}

grobPoints.rect <- function(x, closed, ...) {
    if (closed) {
        hjust <- resolveHJust(x$just, x$hjust)
        vjust <- resolveVJust(x$just, x$vjust)
        w <- convertWidth(x$width, "in", valueOnly=TRUE)
        h <- convertHeight(x$height, "in", valueOnly=TRUE)
        left <- convertX(x$x, "in", valueOnly=TRUE) - hjust*w
        bottom <- convertY(x$y, "in", valueOnly=TRUE) - vjust*h
        right <- left + w
        top <- bottom + h
        ## Recycle via cbind()
        rects <- cbind(left, right, bottom, top)
        xyListFromMatrix(rects, c(1, 1, 2, 2), c(3, 4, 4, 3))
    } else {
        emptyCoords
    }
}

grobPoints.segments <- function(x, closed, ...) {
    if (closed) {
        emptyCoords
    } else {
        x0 <- convertX(x$x0, "in", valueOnly=TRUE)
        x1 <- convertX(x$x1, "in", valueOnly=TRUE)
        y0 <- convertY(x$y0, "in", valueOnly=TRUE)
        y1 <- convertY(x$y1, "in", valueOnly=TRUE)
        ## Recycle via cbind()        
        xy <- cbind(x0, x1, y0, y1)
        xyListFromMatrix(xy, 1:2, 3:4)
    }
}

grobPoints.xspline <- function(x, closed, ...) {
    if ((closed && !x$open) ||
        (!closed && x$open)) {
        ## xsplinePoints() takes care of multiple X-splines
        trace <- xsplinePoints(x)
        if ("x" %in% names(trace)) {
            ## Single X-spline
            list(list(x=as.numeric(trace$x),
                      y=as.numeric(trace$y)))
        } else {
            lapply(trace,
                   function(t) {
                       list(x=as.numeric(t$x), y=as.numeric(t$y))
                   })
        }
    } else {
        emptyCoords
    }
}

## beziergrob covered by splinegrob (via makeContent)

## Do not treat these as open or closed shapes (for now)
grobPoints.text <- function(x, closed, ...) {
    emptyCoords
}

grobPoints.points <- function(x, closed, ...) {
    emptyCoords
}

grobPoints.rastergrob <- function(x, closed, ...) {
    emptyCoords
}

grobPoints.clip <- function(x, closed, ...) {
    emptyCoords
}

grobPoints.null <- function(x, closed, ...) {
    emptyCoords
}

