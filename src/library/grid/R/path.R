
## Mark a grob as something to draw as a single "path"
as.path <- function(x, gp=gpar(), rule=c("winding", "evenodd")) {
    if (!is.grob(x))
        stop("Only a grob can be converted to a path")
    path <- list(grob=x, gp=gp, rule=match.arg(rule))
    class(path) <- "GridPath"
    path
}


## Stroke the outline of a path defined by a grob
drawDetails.GridStroke <- function(x, recording) {
    path <- function() {
        grid.draw(x$path, recording=FALSE)
    }
    grid.Call.graphics(C_stroke, path)
}

strokeGrob <- function(x, ...) {
    UseMethod("strokeGrob")
}

strokeGrob.grob <- function(x, name=NULL, gp=gpar(), vp=NULL, ...) {
    ## grob() rather than gTree() because a "path" only produces
    ## a single shape
    ## (e.g., pattern fills should ALWAYS be resolved)
    stroke <- grob(path=x, name=name, gp=gp, vp=vp, cl="GridStroke")
    stroke
}

strokeGrob.GridPath <- function(x, name=NULL, vp=NULL, ...) {
    stroke <- grob(path=x$grob, name=name, gp=x$gp, vp=vp, cl="GridStroke")
    stroke
}

grid.stroke <- function(...) {
    grid.draw(strokeGrob(...))
}

## Fill the outline of a path defined by a grob
drawDetails.GridFill <- function(x, recording) {
    path <- function() {
        grid.draw(x$path, recording=FALSE)
    }
    grid.Call.graphics(C_fill, path, .ruleIndex(x$rule))
}

fillGrob <- function(x, ...) {
    UseMethod("fillGrob")
}

fillGrob.grob <- function(x, rule=c("winding", "evenodd"),
                          name=NULL, gp=gpar(), vp=NULL, ...) {
    fill <- grob(path=x, rule=match.arg(rule),
                 name=name, gp=gp, vp=vp, cl="GridFill")
    fill
}

fillGrob.GridPath <- function(x, name=NULL, vp=NULL, ...) {
    fill <- grob(path=x$grob, rule=x$rule,
                 name=name, gp=x$gp, vp=vp, cl="GridFill")
    fill
}

grid.fill <- function(...) {
    grid.draw(fillGrob(...))
}



## Stroke and fill the outline of a path defined by a grob
drawDetails.GridFillStroke <- function(x, recording) {
    path <- function() {
        grid.draw(x$path, recording=FALSE)
    }
    grid.Call.graphics(C_fillStroke, path, .ruleIndex(x$rule))
}

fillStrokeGrob <- function(x, ...) {
    UseMethod("fillStrokeGrob")
}

fillStrokeGrob.grob <- function(x, rule=c("winding", "evenodd"),
                                name=NULL, gp=gpar(), vp=NULL, ...) {
    fillStroke <- grob(path=x, rule=match.arg(rule),
                       name=name, gp=gp, vp=vp, cl="GridFillStroke")
    fillStroke
}

fillStrokeGrob.GridPath <- function(x, name=NULL, vp=NULL, ...) {
    fillStroke <- grob(path=x$grob, rule=x$rule,
                       name=name, gp=x$gp, vp=vp, cl="GridFillStroke")
    fillStroke
}

grid.fillStroke <- function(...) {
    grid.draw(fillStrokeGrob(...))
}

################################
## Other grob methods

flattenCoords <- function(coords, name, rule) {
    UseMethod("flattenCoords")
}

flattenCoords.GridGrobCoords <- function(coords, name, rule) {
    names(coords) <- rep("1", length(coords))
    attr(coords, "rule") <- rule
    coords
}

flattenCoords.GridGTreeCoords <- function(coords, name, rule) {
    childCoords <- lapply(coords, flattenCoords, name, rule)
    coords <- do.call(c, childCoords)
    names(coords) <- rep("1", length(coords))
    gridGrobCoords(coords, name, rule)
}   

## NOTE, we need to create a gTree to include any 'gp' and 'vp' settings
## (and cannot just call grobCoords(x) because, although 'x' is a gTree
##  is has no children - the "child" is x$grob)
grobCoords.GridStroke <- function(x, closed, ...) {
    if (closed) {
        emptyGrobCoords(x$name)
    } else {
        coords <- grobCoords(gTree(children=gList(x$path), gp=x$gp, vp=x$vp),
                             closed, ...)
        flattenCoords(coords, x$name, NULL)
    }
}

## NOTE, like grobPoints.gList(), we need to call grobCoords()
## on the "child" grob so that it can perform any relevant set up
grobPoints.GridStroke <- function(x, closed, ...) {
    if (closed) {
        emptyGrobCoords(x$name)
    } else {
        coords <- grobCoords(x$path, closed, ...)
        flattenCoords(coords, x$name, NULL)
    }
}

grobCoords.GridFill <- function(x, closed, ...) {
    if (closed) {
        coords <- grobCoords(gTree(children=gList(x$path), gp=x$gp, vp=x$vp),
                             closed, ...)
        flattenCoords(coords, x$name, x$rule)
    } else {
        emptyGrobCoords(x$name)
    }
}

grobPoints.GridFill <- function(x, closed, ...) {
    if (closed) {
        coords <- grobCoords(x$path, closed, ...)
        flattenCoords(coords, x$name, x$rule)
    } else {
        emptyGrobCoords(x$name)
    }
}

grobCoords.GridFillStroke <- function(x, closed, ...) {
    coords <- grobCoords(gTree(children=gList(x$path), gp=x$gp, vp=x$vp),
                         closed, ...)
    flattenCoords(coords, x$name, x$rule)    
}

grobPoints.GridFillStroke <- function(x, closed, ...) {
    coords <- grobCoords(x$path, closed, ...)
    flattenCoords(coords, x$name, x$rule)
}

