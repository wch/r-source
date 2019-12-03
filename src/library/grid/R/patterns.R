
## Create R objects defining patterns
## These OVERRIDE functions like grDevices::linearGradient()
## When a 'grid' GridLinearGradient needs resolving, we
## create a 'grDevices' LinearGradient

is.pattern <- function(x) {
    inherits(x, "GridPattern")
}

linearGradient <- function(colours = c("black", "white"),
                           stops = seq(0, 1, length.out = length(colours)),
                           x1 = unit(0, "npc"), x2 = unit(1, "npc"),
                           y1 = unit(0, "npc"), y2 = unit(1, "npc"),
                           default.units = "npc",
                           extend = c("pad", "repeat", "reflect", "none")) {

    nstops <- max(length(colours), length(stops))
    colours <- rep(colours, length.out = nstops)
    stops <- rep(stops, length.out = nstops)

    if (! is.unit(x1))
        x1 <- unit(x1, default.units)
    if (! is.unit(x2))
        x2 <- unit(x2, default.units)
    if (! is.unit(y1))
        y1 <- unit(y1, default.units)
    if (! is.unit(y2))
        y2 <- unit(y2, default.units)

    grad <- list(x1 = x1, y1 = y1,
                 x2 = x2, y2 = y2,
                 stops = as.numeric(stops), colours = colours,
                 extend = match.arg(extend))
    class(grad) <- c("GridLinearGradient", "GridPattern")
    grad
}

## Called when drawing a grob
resolveFill <- function(fill, grob) {
    UseMethod("resolveFill")
}

## Simple fills include an R colour (integer or string) or NA
## These just pass through
resolveFill.default <- function(fill, grob) {
    fill
}

## A pattern fill that has already been resolved
## (a grDevices::Pattern)
resolveFill.Pattern <- function(fill, grob) {
    ## The pattern has already been resolved so we just leave it alone
    fill
}

## A pattern fill that needs resolving
## (a grid::GridPattern)
resolveFill.GridPattern <- function(fill, grob) {
    ## All predrawing has been done
    if (is.null(grob)) {
        ## We are pushing a viewport, so resolve relative to viewport
        resolvePattern(fill)
    } else {
        pts <- grobPoints(grob, closed=TRUE)
        if (!isEmptyCoords(pts)) {
            x <- unlist(lapply(pts, function(p) p$x))
            y <- unlist(lapply(pts, function(p) p$y))
            left <- min(x)
            bottom <- min(y)
            width <- diff(range(x))
            height <- diff(range(y))
            pushViewport(viewport(left, bottom, width, height,
                                  default.units="in",
                                  just=c("left", "bottom")))
            gradient <- resolvePattern(fill)
            popViewport()
            gradient
        } else {
            warning("Gradient fill applied to object with no inside")
            ## Set fill to transparent
            "transparent"
        }
    }
}

resolvePattern <- function(pattern) {
    UseMethod("resolvePattern")
}

resolvePattern.GridLinearGradient <- function(gradient) {
    p1 <- deviceLoc(gradient$x1, gradient$y1, valueOnly=TRUE, device=TRUE)
    p2 <- deviceLoc(gradient$x2, gradient$y2, valueOnly=TRUE, device=TRUE)
    grDevices::linearGradient(gradient$colours,
                              gradient$stops,
                              p1$x, p1$y, p2$x, p2$y,
                              extend=gradient$extend)
}

