
fillRules <- c("winding", "evenodd")

ruleIndex <- function(x) {
    rule <- match(x, fillRules)
    if (is.na(rule))
        stop("Invalid fill rule")
    as.integer(rule)
}

## Mark a grob as something to draw as a single "path"
asPath <- function(x, gp=gpar(), rule=c("winding", "evenodd")) {
    if (!is.grob(x))
        stop("Only a grob can be converted to a path")
    path <- list(grob=x, gp=gp, rule=match.arg(rule))
    class(path) <- "grobAsPath"
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

strokeGrob.grob <- function(x, name=NULL, gp=gpar(), vp=NULL) {
    stroke <- gTree(grob=x, name=name, gp=gp, vp=vp, cl="GridStroke")
    stroke
}

strokeGrob.grobAsPath <- function(x, name=NULL, vp=NULL) {
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
    grid.Call.graphics(C_fill, path, ruleIndex(x$rule))
}

fillGrob <- function(x, ...) {
    UseMethod("fillGrob")
}

fillGrob.grob <- function(x, rule=c("winding", "evenodd"),
                          name=NULL, gp=gpar(), vp=NULL) {
    fill <- gTree(grob=x, rule=match.arg(rule),
                  name=name, gp=gp, vp=vp, cl="GridFill")
    fill
}

fillGrob.grobAsPath <- function(x, name=NULL, vp=NULL) {
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
    grid.Call.graphics(C_fillStroke, path, ruleIndex(x$rule))
}

fillStrokeGrob <- function(x, ...) {
    UseMethod("fillStrokeGrob")
}

fillStrokeGrob.grob <- function(x, rule=c("winding", "evenodd"),
                                name=NULL, gp=gpar(), vp=NULL) {
    fillStroke <- gTree(grob=x, rule=match.arg(rule),
                        name=name, gp=gp, vp=vp, cl="GridFillStroke")
    fillStroke
}

fillStrokeGrob.grobAsPath <- function(x, name=NULL, vp=NULL) {
    fillStroke <- gTree(grob=x$grob, rule=x$rule,
                        name=name, gp=x$gp, vp=vp, cl="GridFillStroke")
    fillStroke
}

grid.fillStroke <- function(...) {
    grid.draw(fillStrokeGrob(...))
}

