
## Create R objects defining patterns

is.pattern <- function(x) {
    inherits(x, "GridPattern")
}

is.patternList <- function(x) {
    inherits(x, "GridPatternList")
}

linearGradient <- function(colours = c("black", "white"),
                           stops = seq(0, 1, length.out = length(colours)),
                           x1 = unit(0, "npc"), y1 = unit(0, "npc"),  
                           x2 = unit(1, "npc"), y2 = unit(1, "npc"),
                           default.units = "npc",
                           extend = c("pad", "repeat", "reflect", "none"),
                           group = TRUE) {

    nstops <- max(length(colours), length(stops))
    if (nstops < 1)
        stop("colours and stops must be at least length 1")
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

    if (length(x1) != 1 || length(x2) != 1 ||
        length(y1) != 1 || length(y2) != 1)
        stop("x1, y1, x2, and y2 must all be length 1")

    grad <- list(x1 = x1, y1 = y1,
                 x2 = x2, y2 = y2,
                 stops = as.numeric(stops), colours = colours,
                 extend = match.arg(extend), group = as.logical(group))
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
                           extend = c("pad", "repeat", "reflect", "none"),
                           group = TRUE) {

    nstops <- max(length(colours), length(stops))
    if (nstops < 1)
        stop("colours and stops must be at least length 1")
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

    if (length(cx1) != 1 || length(cx2) != 1 ||
        length(cy1) != 1 || length(cy2) != 1 ||
        length(r1) != 1  || length(r2) != 1)
        stop("cx1, cy1, cx2, cy2, r1, and r2 must all be length 1")

    grad <- list(cx1 = cx1, cy1 = cy1, r1=r1,
                 cx2 = cx2, cy2 = cy2, r2=r2,
                 stops = as.numeric(stops), colours = colours,
                 extend = match.arg(extend), group = as.logical(group))
    class(grad) <- c("GridRadialGradient", "GridPattern")
    grad
}

## Wrap the pattern grob in a gTree with "initial" 'gp' settings
## for the grob to inherit
## (we are particularly concerned about the grob inheriting the
##  fill from its parent, which would mean infinite recursion)
## AND wrap that gTree in a function that draws it.
pattern <- function(grob,
                    x = 0.5, y = 0.5, width = 1, height = 1,
                    default.units = "npc",
                    just="centre", hjust=NULL, vjust=NULL,
                    extend = c("pad", "repeat", "reflect", "none"),
                    gp = gpar(fill="transparent"),
                    group = TRUE) {

    if (! is.unit(x))
        x <- unit(x, default.units)
    if (! is.unit(y))
        y <- unit(y, default.units)
    if (! is.unit(width))
        width <- unit(width, default.units)
    if (! is.unit(height))
        height <- unit(height, default.units)
    hjust = resolveHJust(just, hjust)
    vjust = resolveVJust(just, vjust)

    if (length(x) != 1 || length(y) != 1 ||
        length(width) != 1 || length(height) != 1)
        stop("x, y, width, and height must all be length 1")

    force(grob)
    if (!is.grob(grob))
        stop("Pattern must be based on grob")
    
    ## Do NOT want x$gp$fill to be NULL because that would mean
    ## that 'x' inherits its fill from the grob that it is
    ## filling, which means infinite recursion
    if (is.null(gp$fill)) {
        gp$fill <- "transparent"
        warning("Missing pattern fill has been set to transparent")
    }
    patternFun <- function() {
        grid.draw(gTree(children=gList(grob), gp=gp), recording=FALSE)
    }
    pat <- list(f=patternFun,
                x=x, y=y, width=width, height=height,
                hjust=hjust, vjust=vjust,
                extend = match.arg(extend), group = as.logical(group))
    class(pat) <- c("GridTilingPattern", "GridPattern")
    pat
}

################################################################################
## Pattern resolution

## If CURRENT gp$fill is a pattern then need to attach
## "built" grob (post makeContent() call) to gp$fill for (subsequent)
## resolution of the pattern
## NOTE that this is NOT just for grob$gp$fill because inherited
## gp$fill may be an unresolved pattern
## NOTE that can indiscriminately attach grob to both resolved
## and unresolved patterns because resolved patterns will just ignore
## attached grob
recordGrobForPatternResolution <- function(x) {
    gpar <- grid.Call(C_getGPar)
    if (is.pattern(gpar$fill)) {
        attr(gpar$fill, "grob") <- x
        class(gpar$fill) <- c("GridGrobPattern", class(gpar$fill))
        grid.Call(C_setGPar, gpar)
    } else if (is.patternList(gpar$fill)) {
        attr(gpar$fill, "grob") <- x
        class(gpar$fill) <- c("GridGrobPatternList", class(gpar$fill))
        grid.Call(C_setGPar, gpar)
    }
}

## If gTree has a pattern in gp$fill AND gp$fill$group then attach
## "built" gTree (post makeContent() call) to gp$fill for resolution
## of the pattern AND resolve the pattern
## (if gTree has a LIST of patterns in gp$fill, this may resolve
##  *some* patterns in the list)
## NOTE that this IS with gp$fill because inherited fills pass through
## (see below)
## NOTE that patterns within pattern list that are unresolved
## will get grob attached to list and get resolved in drawing of
## gTree children
## NOTE that inherited patterns SHOULD pass through untouched
## (if group is TRUE inherited pattern should already be resolved
##  and if group is FALSE inherited pattern should be passed through)
recordGTreeForPatternResolution <- function(x) {
    if ((is.pattern(x$gp$fill) && x$gp$fill$group) ||
        is.patternList(x$gp$fill)) { 
        gpar <- grid.Call(C_getGPar)
        attr(gpar$fill, "grob") <- x
        resolvedFill <- resolveFill(gpar$fill, 1)
        ## Resolution may generate NULL (e.g., if gTree has nothing to fill)
        if (is.null(resolvedFill)) {
            gpar$fill <- "transparent"
        } else {
            gpar$fill <- resolvedFill
        }
        grid.Call(C_setGPar, gpar)
    }
}

resolvedPattern <- function(pattern, ref) {
    pattern$ref <- ref
    class(pattern) <- c("GridResolvedPattern", class(pattern))
    pattern
}

## Called when drawing a grob
resolveFill <- function(fill, ...) {
    UseMethod("resolveFill")
}

## Simple fills include an R colour (integer or string) or NA
## These just pass through
resolveFill.default <- function(fill, ...) {
    fill
}

## A pattern fill that has already been resolved
resolveFill.GridResolvedPattern <- function(fill, ...) {
    fill
}

## A pattern fill that needs resolving
## (a grid::GridPattern)
## This will handle viewports (with a single pattern)
resolveFill.GridPattern <- function(fill, ...) {
    resolvePattern(fill)
}

## This will handle viewports (with a list of patterns)
resolveFill.GridPatternList <- function(fill, ...) {
    ## NOTE that some patterns may be resolved, but others may not
    ## (so we cannot mark the entire list as resolved)
    resolvedPatterns <- lapply(fill,
                               function(x) {
                                   if (x$group)
                                       resolvePattern(x)
                                   else
                                       x
                               })
    class(resolvedPatterns) <- class(fill)
    resolvedPatterns
}

## This will handle grobs (with a single pattern)
resolveFill.GridGrobPattern <- function(fill, index=1, ...) {
    ## All predrawing has been done
    ## ('return' is just for GridDefine grobs)
    pts <- grobPoints(attr(fill, "grob"), closed=TRUE, return=TRUE)
    if (!isEmptyCoords(pts)) {
        if (fill$group || length(pts) == 1) {
            ## Pattern is relative to bounding box of all shapes
            bbox <- coordsBBox(pts)
        } else {
            ## Pattern is relative to bounding box of individual shapes
            if (index > length(pts)) {
                warning("grob drawing produces more shapes than grob coords
(recycling coords)")
                index <- (index - 1) %% length(pts) + 1
            }
            ## Individual shape may consist of more than one set of
            ## coordinates (e.g., single path consists of distinct shapes)
            shapeIndex <- names(pts) %in% index
            ## Fallback if 'pts' does not have names to identify shapes
            if (!any(shapeIndex)) {
                shapeIndex <- index
            }
            bbox <- coordsBBox(pts, shapeIndex)
        }
        ## Temporary viewport for calculations, so do NOT record on grid DL
        ## Also, ensure NO mask and NO clip
        ## (at least initially) for resolution of pattern
        ## Also, set fill to "transparent"
        ## (to avoid this viewport picking up the fill being resolved)
        pushViewport(viewport(bbox$left, bbox$bottom, bbox$width, bbox$height,
                              default.units="in",
                              just=c("left", "bottom"),
                              clip="off", mask="none",
                              gp=gpar(fill="transparent")),
                     recording=FALSE)
        pattern <- resolvePattern(fill)
        popViewport(recording=FALSE)
        pattern
    } else {
        warning("Pattern fill applied to object with no inside")
        ## Set fill to transparent
        "transparent"
    }
}

## This will handle grobs (with a list of patterns)
resolveFill.GridGrobPatternList <- function(fill, ...) {
    ## All predrawing has been done
    pts <- grobPoints(attr(fill, "grob"), closed=TRUE, return=TRUE)
    if (!isEmptyCoords(pts)) {
        resolvedFills <- vector("list", length(pts))
        for (i in seq_along(pts)) {
            ## Recycle patterns if necessary
            which <- (i - 1) %% length(fill) + 1
            ## Only resolve if not already resolved
            if (inherits(fill[[which]], "GridResolvedPattern")) {
                resolvedFills[[i]] <- fill[[which]]
                next
            }
            if (fill[[which]]$group || length(pts) == 1) {
                ## Pattern is relative to bounding box of all shapes
                bbox <- coordsBBox(pts)
            } else {
                ## Pattern is relative to bounding box of individual shapes
                ## Individual shape may consist of more than one set of
                ## coordinates (e.g., single path consists of distinct shapes)
                shapeIndex <- names(pts) %in% i
                ## Fallback if 'pts' does not have names to identify shapes
                if (!any(shapeIndex)) {
                    shapeIndex <- i
                }
                bbox <- coordsBBox(pts, shapeIndex)
            }
            ## Temporary viewport for calculations, so do NOT record on grid DL
            ## Also, ensure NO mask and NO clip
            ## (at least initially) for resolution of pattern
            ## Also, set fill to "transparent"
            ## (to avoid this viewport picking up the fill being resolved)
            pushViewport(viewport(bbox$left, bbox$bottom,
                                  bbox$width, bbox$height,
                                  default.units="in",
                                  just=c("left", "bottom"),
                                  clip="off", mask="none",
                                  gp=gpar(fill="transparent")),
                         recording=FALSE)
            pattern <- resolvePattern(fill[[which]])
            popViewport(recording=FALSE)
            resolvedFills[[i]] <- pattern
        }
        class(resolvedFills) <- c("GridResolvedPatternList",
                                  class(fill))
        resolvedFills
    } else {
        warning("Pattern fill applied to object with no inside")
        ## Set fill to transparent
        "transparent"
    }
}

resolveGTreeFill <- function(fill, pts) {
    if (!isEmptyCoords(pts)) {
        ## Pattern is relative to bounding box of all shapes
        bbox <- coordsBBox(pts)
        ## Temporary viewport for calculations, so do NOT record on grid DL
        ## Also, ensure NO mask and NO clip
        ## (at least initially) for resolution of pattern
        ## Also, set fill to "transparent"
        ## (to avoid this viewport picking up the fill being resolved)
        pushViewport(viewport(bbox$left, bbox$bottom,
                              bbox$width, bbox$height,
                              default.units="in",
                              just=c("left", "bottom"),
                              clip="off", mask="none",
                              gp=gpar(fill="transparent")),
                     recording=FALSE)
        pattern <- resolvePattern(fill)
        popViewport(recording=FALSE)
        pattern
    } 
}

## This will handle gTrees (with a single pattern)
## This should ONLY be called if fill$group is TRUE
## (see recordGTreeForPatternResolution())
resolveFill.GridGTreePattern <- function(fill, index=1, ...) {
    ## All predrawing has been done
    pts <- grobPoints(attr(fill, "grob"), closed=TRUE, return=TRUE)
    resolveGTreeFill(fill, pts)
}

## This will handle gTrees (with a list of patterns)
## For each fill ...
##   if fill$group resolve relative to gTree bbox
##   else pass through
## (similar to resolveFill.GridPatternList() for viewports)
resolveFill.GridGTreePatternList <- function(fill, ...) {
    ## All predrawing has been done
    pts <- grobPoints(attr(fill, "grob"), closed=TRUE, return=TRUE)
    resolveOneFill <- function(x) {
        if (x$group) {
            resolveGTreeFill(x, pts)
        } else {
            x
        }
    }
    resolvedFills <- lapply(fill, resolveOneFill)
    class(resolvedFills) <- class(fill)
    resolvedFills
}

resolvePattern <- function(pattern) {
    UseMethod("resolvePattern")
}

resolvePattern.GridLinearGradient <- function(pattern) {
    p1 <- deviceLoc(pattern$x1, pattern$y1, valueOnly=TRUE, device=TRUE)
    p2 <- deviceLoc(pattern$x2, pattern$y2, valueOnly=TRUE, device=TRUE)
    index <- .setPattern(.linearGradientPattern(pattern$colours,
                                                pattern$stops,
                                                p1$x, p1$y, p2$x, p2$y,
                                                extend=pattern$extend))
    resolvedPattern(pattern, index)
}

resolvePattern.GridRadialGradient <- function(pattern) {
    c1 <- deviceLoc(pattern$cx1, pattern$cy1, valueOnly=TRUE, device=TRUE)
    r1 <- min(sqrt(sum(unlist(deviceDim(unit(0, "in"), pattern$r1,
                                        valueOnly=TRUE, device=TRUE))^2)),
              sqrt(sum(unlist(deviceDim(pattern$r1, unit(0, "in"), 
                                        valueOnly=TRUE, device=TRUE))^2)))
    c2 <- deviceLoc(pattern$cx2, pattern$cy2, valueOnly=TRUE, device=TRUE)
    r2 <- min(sqrt(sum(unlist(deviceDim(unit(0, "in"), pattern$r2,
                                        valueOnly=TRUE, device=TRUE))^2)),
              sqrt(sum(unlist(deviceDim(pattern$r2, unit(0, "in"), 
                                        valueOnly=TRUE, device=TRUE))^2)))
    index <- .setPattern(.radialGradientPattern(pattern$colours,
                                                pattern$stops,
                                                c1$x, c1$y, r1,
                                                c2$x, c2$y, r2,
                                                extend=pattern$extend))
    resolvedPattern(pattern, index)
}

resolvePattern.GridTilingPattern <- function(pattern) {
    xy <- deviceLoc(pattern$x, pattern$y, valueOnly=TRUE, device=TRUE)
    wh <- deviceDim(pattern$width, pattern$height, valueOnly=TRUE, device=TRUE)
    left <- xy$x - pattern$hjust*wh$w
    bottom <- xy$y - pattern$vjust*wh$h
    index <- .setPattern(.tilingPattern(pattern$f,
                                        left, bottom, wh$w, wh$h,
                                        extend=pattern$extend))
    resolvedPattern(pattern, index)
}

## Used when "grab"ing the display list to "demote"
## a resolved pattern
unresolveFill <- function(fill) {
    UseMethod("unresolveFill")
}

## Simple fills include an R colour (integer or string) or NA
## These just pass through
unresolveFill.default <- function(fill) {
    fill
}

unresolveFill.GridPattern <- function(fill) {
    unresolvePattern(fill)
}

unresolvePattern <- function(pattern) {
    UseMethod("unresolvePattern")
}
    
## Unresolved patterns just pass through
unresolvePattern.GridPattern <- function(pattern) {
    pattern
}

unresolvePattern.GridResolvedPattern <- function(pattern) {
    pattern$ref <- NULL
    class(pattern) <-
        class(pattern)[!(class(pattern) %in% "GridResolvedPattern")]
    pattern
}

