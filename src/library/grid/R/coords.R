
## Functions to calculate a set of points around the perimeter
## (or along the length) of a grob

## grobCoords() is a user-level function that emulates drawing
## set up behaviour (pushing viewports and setting graphical parameters)

## grobPoints() does not perform any set up and is for use
## within other 'grid' functions when set up has already been done,
## e.g., within resolveFill()

################################################################################
## Functions for creating coords data structures

validCoords <- function(x) {
    is.list(x) && length(x) > 0 &&
        is.numeric(x$x) && is.numeric(x$y) && length(x$x) == length(x$y)
        
}

validGrobCoords <- function(x) {
    is.list(x) && length(x) > 0 &&
        all(sapply(x, inherits, "GridCoords"))
}

validGTreeCoords <- function(x) {
    is.list(x) && length(x) > 0 &&
        all(sapply(x, inherits, "GridGrobCoords") |
            sapply(x, inherits, "GridGTreeCoords"))
}

coordPrintIndent <- "  "

## Public function for creating valid 'grid' points/coords result
gridCoords <- function(x, y) {
    coords <- list(x=as.numeric(x), y=as.numeric(y))
    if (validCoords(coords)) {
        class(coords) <- "GridCoords"
        coords
    } else
        stop("Invalid coordinates")
}

print.GridCoords <- function(x, indent="", ...) {
    if (length(x$x) > 3) {
        dots <- "..."
    } else {
        dots <- ""
    }
    cat(paste0(indent, "x:"),
        head(x$x, 3), dots, paste0("[", length(x$x), " values]\n"))
    cat(paste0(indent, "y:"),
        head(x$y, 3), dots, paste0("[", length(x$y), " values]\n"))    
}

gridGrobCoords <- function(x, name, rule = NULL) {
    if (validGrobCoords(x)) {
        class(x) <- "GridGrobCoords"
        attr(x, "name") <- name
        attr(x, "rule") <- rule
        x
    } else
        stop("Invalid grob coordinates")
}

print.GridGrobCoords <- function(x, indent="", ...) {
    if (is.null(names(x))) {
        names <- 1:length(x)
    } else {
        names <- names(x)
    }
    rule <- attr(x, "rule")
    if (is.null(rule)) {
        fillrule <- ""
    } else {
        fillrule <- paste0(" (fill: ", rule, ")")
    }
    cat(paste0(indent, "grob"), attr(x, "name"), fillrule, "\n")
    for (i in seq_along(x)) {
        cat(paste0(indent, coordPrintIndent, "shape"), names[i], "\n")
        print(x[[i]], indent=paste0(indent, coordPrintIndent, coordPrintIndent))
    }
}

gridGTreeCoords <- function(x, name) {
    if (validGTreeCoords(x)) {
        class(x) <- "GridGTreeCoords"
        attr(x, "name") <- name
        x
    } else {
        stop("Invalid gTree coordinates")
    }
}

print.GridGTreeCoords <- function(x, indent="", ...) {
    cat(paste0(indent, "gTree"), attr(x, "name"), "\n")
    for (i in seq_along(x)) {
        print(x[[i]], indent=paste0(indent, coordPrintIndent))
    }
}

################################################################################
## Calculate bounding box of coordinates

getX <- function(x, ...) {
    UseMethod("getX")
}

getY <- function(x, ...) {
    UseMethod("getY")
}

getX.GridCoords <- function(x, ...) {
    x$x
}

getY.GridCoords <- function(x, ...) {
    x$y
}

getX.GridGrobCoords <- function(x, subset = NULL, ...) {
    if (is.null(subset)) {
        x <- unlist(lapply(x, getX, ...))
    } else {
        x <- unlist(lapply(x[subset], getX, ...))
    }
}

getY.GridGrobCoords <- function(x, subset = NULL, ...) {
    if (is.null(subset)) {
        x <- unlist(lapply(x, getY, ...))
    } else {
        x <- unlist(lapply(x[subset], getY, ...))
    }
}

getX.GridGTreeCoords <- function(x, ...) {
    x <- unlist(lapply(x, getX, ...))
}

getY.GridGTreeCoords <- function(x, ...) {
    y <- unlist(lapply(x, getY, ...))
}

coordsBBox <- function(x, subset = NULL) {
    xx <- getX(x, subset)
    yy <- getY(x, subset)
    list(left = min(xx),
         bottom = min(yy),
         width = diff(range(xx)),
         height = diff(range(yy)))
}

################################################################################
## Support functions for calculating gTree coords

toDevice <- function(x) {
    if (isEmptyCoords(x)) return(x)
    UseMethod("toDevice")
}

toDevice.GridCoords <- function(x) {
    pts <- deviceLoc(unit(x$x, "in"), unit(x$y, "in"),
                        valueOnly=TRUE)
    gridCoords(pts$x, pts$y)
}

toDevice.GridGrobCoords <- function(x) {
    pts <- lapply(x, toDevice)
    gridGrobCoords(pts, attr(x, "name"), attr(x, "rule"))
}

toDevice.GridGTreeCoords <- function(x) {
    pts <- lapply(x, toDevice)
    gridGTreeCoords(pts, attr(x, "name"))
}

fromDevice <- function(x, trans) {
    UseMethod("fromDevice")
}

fromDevice.GridCoords <- function(x, trans) {
    ptsMatrix <- cbind(x$x, x$y, 1) %*% solve(trans)
    gridCoords(x=ptsMatrix[,1], y=ptsMatrix[,2])
}

fromDevice.GridGrobCoords <- function(x, trans) {
    pts <- lapply(x, fromDevice, trans)
    gridGrobCoords(pts, attr(x, "name"), attr(x, "rule"))
}

fromDevice.GridGTreeCoords <- function(x, trans) {
    pts <- lapply(x, fromDevice, trans)
    gridGTreeCoords(pts, attr(x, "name"))
}

################################################################################
## Empty coordinates

emptyCoords <- gridCoords(x = 0, y = 0)

emptyGrobCoords <- function(name) {
    gridGrobCoords(list("0"=emptyCoords), name)
}

emptyGTreeCoords <- function(name) {
    gridGTreeCoords(list(emptyGrobCoords("0")), name)
}

isEmptyCoords <- function(coords) {
    UseMethod("isEmptyCoords")
}

isEmptyCoords.GridCoords <- function(coords) {
    identical(coords, emptyCoords) 
}

isEmptyCoords.GridGrobCoords <- function(coords) {
    all(sapply(coords, identical, emptyCoords))
}

isEmptyCoords.GridGTreeCoords <- function(coords) {
    all(sapply(coords, isEmptyCoords))    
}

################################################################################
## Determine default 'closed' value
isClosed <- function(x, ...) {
    UseMethod("isClosed")
}

isClosedTRUE <- function(x, ...) {
    TRUE
}

isClosedFALSE <- function(x, ...) {
    FALSE
}

isClosed.default <- isClosedTRUE

isClosed.move.to <- isClosedFALSE
isClosed.line.to <- isClosedFALSE
isClosed.lines <- isClosedFALSE
isClosed.polyline <- isClosedFALSE
isClosed.segments <- isClosedFALSE
isClosed.beziergrob <- isClosedFALSE

isClosed.xspline <- function(x, ...) {
    if (x$open)
        FALSE
    else
        TRUE
}

isClosed.points <- function(x, ...) {
    switch(as.character(x$pch),
           "3"=, ## plus
           "4"=, ## times
           "8"=FALSE, ## plus-times
           TRUE)
}

################################################################################
## grobCoords()
##   Do drawing set up then calculate points

grobCoords <- function(x, closed, ...) {
    UseMethod("grobCoords")
}

grobCoords.grob <- function(x, closed=isClosed(x), ...) {
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
        pts <- gridGrobCoords(lapply(pts, toDevice), x$name,
                              attr(pts, "rule"))
    }
    # Same context clean up as drawGrob()
    postDraw(x)
    if (vpgrob && !isEmptyCoords(pts)) {
        ## Transform back to locations
        pts <- gridGrobCoords(lapply(pts, fromDevice, trans), x$name,
                              attr(pts, "rule"))
    }
    pts
}

## "gTree"s
grobCoords.gList <- function(x, closed=isClosed(x), ...) {
    gridGTreeCoords(lapply(x, grobCoords, closed, ...),
                    grobName())
}

grobCoords.gTree <- function(x, closed=isClosed(x), ...) {
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
    pts <- gridGTreeCoords(unname(lapply(x$children[x$childrenOrder],
                                         grobCoords, closed, ...)),
                           x$name)
    if (vpgrob && !isEmptyCoords(pts)) {
        ## Calc locations on device
        pts <- gridGTreeCoords(lapply(pts, toDevice), x$name)
    }
    # Same context clean up as drawGTree()
    postDraw(x)
    if (vpgrob && !isEmptyCoords(pts)) {
        ## Transform back to locations
        pts <- gridGTreeCoords(lapply(pts, fromDevice, trans), x$name)
    }
    pts
}

################################################################################
## grobPoints()
##   No drawing set up

grobPoints <- function(x, closed, ...) {
    UseMethod("grobPoints")
}

grobPoints.grob <- function(x, closed, ...) {
    emptyGrobCoords(x$name)
}

grobPoints.move.to <- function(x, closed, ...) {
    emptyGrobCoords(x$name)
}

grobPoints.line.to <- function(x, closed, ...) {
    emptyGrobCoords(x$name)
}

grobPoints.circle <- function(x, closed=TRUE, ..., n=100) {
    if (closed) {
        cx <- convertX(x$x, "in", valueOnly=TRUE)
        cy <- convertY(x$y, "in", valueOnly=TRUE)
        r <- pmin(convertWidth(x$r, "in", valueOnly=TRUE),
                  convertHeight(x$r, "in", valueOnly=TRUE))
        t <- seq(0, 2*pi, length.out=n+1)[-(n+1)]
        ## Recycle via cbind()
        circs <- cbind(cx, cy, r)
        n <- nrow(circs)
        pts <- lapply(1:n,
                      function(i) {
                          gridCoords(x=circs[i, 1] + circs[i, 3]*cos(t),
                                     y=circs[i, 2] + circs[i, 3]*sin(t))
                      })
        names(pts) <- 1:n
        gridGrobCoords(pts, x$name)
    } else {
        emptyGrobCoords(x$name)
    }
}

grobPoints.lines <- function(x, closed=FALSE, ..., n=100) {
    if (closed) {
        emptyGrobCoords(x$name)
    } else {
        xx <- convertX(x$x, "in", valueOnly=TRUE)
        yy <- convertY(x$y, "in", valueOnly=TRUE)
        ## Recycle via cbind()
        lines <- cbind(xx, yy)
        gridGrobCoords(list("1"=gridCoords(x=lines[,1], y=lines[,2])), x$name)
    }
}

grobPoints.polyline <- function(x, closed=FALSE, ...) {
    if (closed) {
        emptyGrobCoords(x$name)
    } else {
        ## polylineGrob() ensures that x/y same length
        xx <- convertX(x$x, "in", valueOnly=TRUE)
        yy <- convertY(x$y, "in", valueOnly=TRUE)
        pts <- list(x=xx, y=yy)
        if (is.null(x$id) && is.null(x$id.lengths)) {
            gridGrobCoords(list("1"=do.call(gridCoords, pts)), x$name)
        } else {
            if (is.null(x$id)) {
                n <- length(x$id.lengths)
                id <- rep(1L:n, x$id.lengths)
            } else {
                n <- length(unique(x$id))
                id <- x$id
            }
            if (n > 1) {
                gridGrobCoords(lapply(split(as.data.frame(pts), id),
                                  function(z) do.call(gridCoords, z)),
                               x$name)
            } else {
                gridGrobCoords(list("1"=do.call(gridCoords, pts)), x$name)
            }
        }
    }    
}

## NOTE that grid.polygon() does not provide ability to set fill rule
## (and neither does dev->polygon());  some devices allow a global
## device fill rule (!), e.g., pdf(), postscript(), windows(),
## but we can't do anything about that.
## If you want proper control, use grid.path() instead
## (which does have a fill rule arg).
grobPoints.polygon <- function(x, closed=TRUE, ...) {
    if (closed) {
        ## polygonGrob() ensures that x/y same length
        xx <- convertX(x$x, "in", valueOnly=TRUE)
        yy <- convertY(x$y, "in", valueOnly=TRUE)
        pts <- list(x=xx, y=yy)
        if (is.null(x$id) && is.null(x$id.lengths)) {
            gridGrobCoords(list("1"=do.call(gridCoords, pts)), x$name)
        } else {
            if (is.null(x$id)) {
                n <- length(x$id.lengths)
                id <- rep(1L:n, x$id.lengths)
            } else {
                n <- length(unique(x$id))
                id <- x$id
            }
            if (n > 1) {
                gridGrobCoords(lapply(split(as.data.frame(pts), id),
                                  function(z) do.call(gridCoords, z)),
                               x$name)
            } else {
                gridGrobCoords(list("1"=do.call(gridCoords, pts)), x$name)
            }
        }
    } else {
        emptyGrobCoords(x$name)
    }
}

xyListFromMatrix <- function(m, xcol, ycol) {
    n <- nrow(m)
    lapply(1:n,
           function(i) {
               gridCoords(x=m[i, xcol], y=m[i, ycol])
           })
}

grobPoints.pathgrob <- function(x, closed=TRUE, ...) {
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
                gridGrobCoords(lapply(split(as.data.frame(pts), pathId),
                                  function(z) do.call(gridCoords, z)),
                               x$name, x$rule)
            } else {
                gridGrobCoords(list("1"=do.call(gridCoords, pts)),
                               x$name, x$rule)
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
                pts <- unlist(mapply(split,
                                     split(as.data.frame(pts), pathId),
                                     split(id, pathId),
                                     SIMPLIFY=FALSE),
                              recursive=FALSE)
                names(pts) <- gsub("[.][0-9]+$", "", names(pts))
                gridGrobCoords(lapply(pts,
                                  function(z) do.call(gridCoords, z)),
                               x$name, x$rule)
            } else {
                pts <- split(as.data.frame(pts), id)
                names(pts) <- rep(1, length(pts))
                gridGrobCoords(lapply(pts,
                                  function(z) do.call(gridCoords, z)),
                               x$name, x$rule)
            }
        }
    } else {
        emptyGrobCoords(x$name)
    }
}

grobPoints.rect <- function(x, closed=TRUE, ...) {
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
        pts <- xyListFromMatrix(rects, c(1, 1, 2, 2), c(3, 4, 4, 3))
        names(pts) <- seq_along(pts)
        gridGrobCoords(pts, x$name)
    } else {
        emptyGrobCoords(x$name)
    }
}

grobPoints.segments <- function(x, closed=FALSE, ...) {
    if (closed) {
        emptyGrobCoords(x$name)
    } else {
        x0 <- convertX(x$x0, "in", valueOnly=TRUE)
        x1 <- convertX(x$x1, "in", valueOnly=TRUE)
        y0 <- convertY(x$y0, "in", valueOnly=TRUE)
        y1 <- convertY(x$y1, "in", valueOnly=TRUE)
        ## Recycle via cbind()        
        xy <- cbind(x0, x1, y0, y1)
        pts <- xyListFromMatrix(xy, 1:2, 3:4)
        names(pts) <- seq_along(pts)
        gridGrobCoords(pts, x$name)
    }
}

grobPoints.xspline <- function(x, closed=!x$open, ...) {
    if ((closed && !x$open) ||
        (!closed && x$open)) {
        ## xsplinePoints() takes care of multiple X-splines
        trace <- xsplinePoints(x)
        if ("x" %in% names(trace)) {
            ## Single X-spline
            gridGrobCoords(list("1"=gridCoords(x=as.numeric(trace$x),
                                           y=as.numeric(trace$y))),
                           x$name)
        } else {
            pts <- lapply(trace,
                          function(t) {
                              gridCoords(x=as.numeric(t$x), y=as.numeric(t$y))
                          })
            names(pts) <- seq_along(pts)
            gridGrobCoords(pts, x$name)
        }
    } else {
        emptyGrobCoords(x$name)
    }
}

## beziergrob covered by splinegrob (via makeContent)

## Just return a bounding box for the text (if closed=TRUE)
grobPoints.text <- function(x, closed=TRUE, ...) {
    if (closed) {
        bounds <- grid.Call(C_textBounds, as.graphicsAnnot(x$label),
                            x$x, x$y,
                            resolveHJust(x$just, x$hjust),
                            resolveVJust(x$just, x$vjust),
                            x$rot, as.numeric(NA))
        if (is.null(bounds))
            emptyGrobCoords(x$name)
        else {
            left <- bounds[5]
            bottom <- bounds[6]
            right <- left + bounds[3]
            top <- bottom + bounds[4]
            gridGrobCoords(list("1"=gridCoords(x=c(left, left, right, right),
                                           y=c(bottom, top, top, bottom))),
                           x$name)
        }
    } else {
        emptyGrobCoords(x$name)
    }
}

grobPoints.points <- function(x, closed=TRUE, ...) {
    closed <- as.logical(closed)
    if (length(closed) != 1 || is.na(closed)) 
        stop("Closed must be length 1 and must not be a missing value")
    pts <- grid.Call(C_pointsPoints, x$x, x$y, x$pch, x$size, closed)
    if (is.null(pts) ||
        all(sapply(pts, is.null))) {
        emptyGrobCoords(x$name)
    } else {
        names <- attr(pts, "coordNames")
        pts <- lapply(pts,
                      function(x) {
                          if (is.null(x))
                              emptyCoords
                          else {
                              names(x) <- c("x", "y")
                              do.call(gridCoords, x)
                          }
                      })
        names(pts) <- names
        gridGrobCoords(pts, x$name)
    }
}

## Do not treat these as open or closed shapes (for now at least)
grobPoints.rastergrob <- function(x, closed, ...) {
    emptyGrobCoords(x$name)
}

grobPoints.clip <- function(x, closed, ...) {
    emptyGrobCoords(x$name)
}

grobPoints.null <- function(x, closed, ...) {
    emptyGrobCoords(x$name)
}

## Collections of grobs

## NOTE that these generate coordinates from their children
## and they must call grobCoords() rather than grobPoints()
## on those children so that the children can perform any
## relevant set up

grobPoints.gList <- function(x, closed=TRUE, ...) {
    if (length(x) > 0) {
        gridGTreeCoords(lapply(x, grobCoords, closed, ...), grobName())
    } else {
        emptyGTreeCoords(grobName())
    }
}

grobPoints.gTree <- function(x, closed=TRUE, ...) {
    if (length(x$children) > 0) {
        pts <- lapply(x$children[x$childrenOrder], grobCoords, closed, ...)
        gridGTreeCoords(unname(pts), x$name)
    } else {
        emptyGTreeCoords(x$name)
    }
}

