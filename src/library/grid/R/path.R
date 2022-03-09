
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
        grid.draw(x$grob, recording=FALSE)
    }
    grid.Call.graphics(C_stroke, path)
}

strokeGrob <- function(x, ...) {
    UseMethod("strokeGrob")
}

strokeGrob.grob <- function(x, name=NULL, gp=gpar(), vp=NULL, ...) {
    stroke <- gTree(grob=x, name=name, gp=gp, vp=vp, cl="GridStroke")
    stroke
}

strokeGrob.GridPath <- function(x, name=NULL, vp=NULL, ...) {
    stroke <- gTree(grob=x$grob, name=name, gp=x$gp, vp=vp, cl="GridStroke")
    stroke
}

grid.stroke <- function(...) {
    grid.draw(strokeGrob(...))
}

## Fill the outline of a path defined by a grob
drawDetails.GridFill <- function(x, recording) {
    path <- function() {
        grid.draw(x$grob, recording=FALSE)
    }
    grid.Call.graphics(C_fill, path, .ruleIndex(x$rule))
}

fillGrob <- function(x, ...) {
    UseMethod("fillGrob")
}

fillGrob.grob <- function(x, rule=c("winding", "evenodd"),
                          name=NULL, gp=gpar(), vp=NULL, ...) {
    fill <- gTree(grob=x, rule=match.arg(rule),
                  name=name, gp=gp, vp=vp, cl="GridFill")
    fill
}

fillGrob.GridPath <- function(x, name=NULL, vp=NULL, ...) {
    fill <- gTree(grob=x$grob, rule=x$rule,
                  name=name, gp=x$gp, vp=vp, cl="GridFill")
    fill
}

grid.fill <- function(...) {
    grid.draw(fillGrob(...))
}



## Stroke and fill the outline of a path defined by a grob
drawDetails.GridFillStroke <- function(x, recording) {
    path <- function() {
        grid.draw(x$grob, recording=FALSE)
    }
    grid.Call.graphics(C_fillStroke, path, .ruleIndex(x$rule))
}

fillStrokeGrob <- function(x, ...) {
    UseMethod("fillStrokeGrob")
}

fillStrokeGrob.grob <- function(x, rule=c("winding", "evenodd"),
                                name=NULL, gp=gpar(), vp=NULL, ...) {
    fillStroke <- gTree(grob=x, rule=match.arg(rule),
                        name=name, gp=gp, vp=vp, cl="GridFillStroke")
    fillStroke
}

fillStrokeGrob.GridPath <- function(x, name=NULL, vp=NULL, ...) {
    fillStroke <- gTree(grob=x$grob, rule=x$rule,
                        name=name, gp=x$gp, vp=vp, cl="GridFillStroke")
    fillStroke
}

grid.fillStroke <- function(...) {
    grid.draw(fillStrokeGrob(...))
}

################################
## Other grob methods

## NOTE, we need to create a gTree to include any 'gp' and 'vp' settings
## (and cannot just call grobCoords(x) because, although 'x' is a gTree
##  is has no children - the "child" is x$grob)
grobCoords.GridStroke <- function(x, closed, ...) {
    if (closed)
        emptyGrobCoords(x$name)
    else
        grobCoords(gTree(children=gList(x$grob), gp=x$gp, vp=x$vp),
                   closed, ...)
}

## NOTE, like grobPoints.gList(), we need to call grobCoords()
## on the "child" grob so that it can perform any relevant set up
grobPoints.GridStroke <- function(x, closed, ...) {
    if (closed)
        emptyGrobCoords(x$name)
    else
        grobCoords(x$grob, closed, ...)
}

grobCoords.GridFill <- function(x, closed, ...) {
    if (closed)
        grobCoords(gTree(children=gList(x$grob), gp=x$gp, vp=x$vp),
                   closed, ...)
    else
        emptyGrobCoords(x$name)
}

grobPoints.GridFill <- function(x, closed, ...) {
    if (closed)
        grobCoords(x$grob, closed, ...)
    else
        emptyGrobCoords(x$name)
}

grobCoords.GridFillStroke <- function(x, closed, ...) {
    grobCoords(gTree(children=gList(x$grob), gp=x$gp, vp=x$vp),
                     closed, ...)
}

grobPoints.GridFillStroke <- function(x, closed, ...) {
    grobCoords(x$grob, closed, ...)
}

