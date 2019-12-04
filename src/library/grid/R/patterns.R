
## Create R objects defining patterns
## These OVERRIDE functions like grDevices::linearGradient()
## When a 'grid' GridLinearGradient needs resolving, we
## create a 'grDevices' LinearGradient

is.pattern <- function(x) {
    inherits(x, "GridPattern")
}

linearGradient <- function(colours = c("black", "white"),
                           stops = seq(0, 1, length.out = length(colours)),
                           x1 = unit(0, "npc"), y1 = unit(0, "npc"),  
                           x2 = unit(1, "npc"), y2 = unit(1, "npc"),
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

radialGradient <- function(colours = c("black", "white"),
                           stops = seq(0, 1, length.out = length(colours)),
                           cx1 = unit(.5, "npc"), cy1 = unit(.5, "npc"),
                           r1 = unit(0, "npc"),
                           cx2 = unit(.5, "npc"), cy2 = unit(.5, "npc"),
                           r2 = unit(.5, "npc"),
                           default.units = "npc",
                           extend = c("pad", "repeat", "reflect", "none")) {

    nstops <- max(length(colours), length(stops))
    colours <- rep(colours, length.out = nstops)
    stops <- rep(stops, length.out = nstops)

    if (!is.unit(cx1))
        cx1 <- unit(cx1, default.units)
    if (!is.unit(cy1))
        cy1 <- unit(cy1, default.units)
    if (!is.unit(r1))
        r1 <- unit(r1, default.units)
    if (!is.unit(cx2))
        cx2 <- unit(cx2, default.units)
    if (!is.unit(cy2))
        cy2 <- unit(cy2, default.units)
    if (!is.unit(r2))
        r2 <- unit(r2, default.units)

    grad <- list(cx1 = cx1, cy1 = cy1, r1=r1,
                 cx2 = cx2, cy2 = cy2, r2=r2,
                 stops = as.numeric(stops), colours = colours,
                 extend = match.arg(extend))
    class(grad) <- c("GridRadialGradient", "GridPattern")
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

resolvePattern.GridRadialGradient <- function(gradient) {
    c1 <- deviceLoc(gradient$cx1, gradient$cy1, valueOnly=TRUE, device=TRUE)
    r1 <- min(sqrt(sum(unlist(deviceDim(unit(0, "in"), gradient$r1,
                                        valueOnly=TRUE, device=TRUE))^2)),
              sqrt(sum(unlist(deviceDim(gradient$r1, unit(0, "in"), 
                                        valueOnly=TRUE, device=TRUE))^2)))
    c2 <- deviceLoc(gradient$cx2, gradient$cy2, valueOnly=TRUE, device=TRUE)
    r2 <- min(sqrt(sum(unlist(deviceDim(unit(0, "in"), gradient$r2,
                                        valueOnly=TRUE, device=TRUE))^2)),
              sqrt(sum(unlist(deviceDim(gradient$r2, unit(0, "in"), 
                                        valueOnly=TRUE, device=TRUE))^2)))
    grDevices::radialGradient(gradient$colours,
                              gradient$stops,
                              c1$x, c1$y, r1, c2$x, c2$y, r2,
                              extend=gradient$extend)
}

