
## Composite one or more grobs, using a compositing operator,
## THEN draw the result
## (normal drawing just composites each grob [in fact each shape!] with OVER)

##########################
## Support functions

finaliseGroup <- function(x) {
    source <- function() {
        cvp <- current.viewport()
        ## Push a viewport (with the current viewport layout and scales)
        ## WITH NO MASK to ensure that a group begins with no soft mask.
        ##
        ## "Before execution of the transparency group XObject's
        ## content stream, the current blend mode in the graphics
        ## state is initialized to Normal, the current stroking and
        ## nonstroking alpha constants to 1.0, and the current soft
        ## mask to None."
        ##
        ## https://www.adobe.com/content/dam/acom/en/devnet/pdf/pdfs/pdf_reference_archives/PDFReference.pdf">PDF</a>
        ##
        ## Current viewport layout and scales are preserved so that
        ## locations and dimensions of viewports and grobs in x$src
        ## retain their original meaning.
        pushViewport(viewport(mask="none",
                              layout=cvp$layout,
                              xscale=cvp$xscale, yscale=cvp$yscale),
                     recording=FALSE)
        grid.draw(x$src, recording=FALSE)
        popViewport(recording=FALSE)
    }
    if (is.grob(x$dst)) {
        destination <- function() {
            cvp <- current.viewport()
            pushViewport(viewport(mask="none",
                                  layout=cvp$layout,
                                  xscale=cvp$xscale, yscale=cvp$yscale),
                         recording=FALSE)
            grid.draw(x$dst, recording=FALSE)
            popViewport(recording=FALSE)
        }
    } else { ## NULL ("transparent") destination
        destination <- NULL
    }
    list(src=source, op=x$op, dst=destination)
}

## group mappings are stored in 'grid' state in component 18 (17 zero-based)
## (see grid.h)
groupIndex <- 18

## Record group definition (in 'grid' state)
recordGroup <- function(name, ref, coords) {
    devState <- get(".GRID.STATE", envir=.GridEvalEnv)[[dev.cur() - 1]]
    devStateGroups <- devState[[groupIndex]]
    cvp <- current.viewport()
    if (coords) {
        closedPoints <- groupPoints(x, TRUE)
        openPoints <- groupPoints(x, FALSE)
    } else {
        closedPoints <- emptyGrobCoords(name)
        openPoints <- emptyGrobCoords(name)
    }
    group <- list(ref=ref,
                  ## Record location, size, angle for re-use in
                  ## different viewport
                  xy=deviceLoc(unit(resolveHJust(cvp$just, cvp$hjust), "npc"),
                               unit(resolveVJust(cvp$just, cvp$vjust), "npc"),
                               valueOnly=TRUE, device=TRUE),
                  xyin=deviceLoc(unit(resolveHJust(cvp$just, cvp$hjust), "npc"),
                                 unit(resolveVJust(cvp$just, cvp$vjust), "npc"),
                                 valueOnly=TRUE, device=FALSE),
                  wh=c(convertX(unit(1, "npc"), "in", valueOnly=TRUE),
                       convertY(unit(1, "npc"), "in", valueOnly=TRUE)),
                  r=current.rotation(),
                  closedPoints=closedPoints,
                  openPoints=openPoints)
    if (is.null(devStateGroups)) {
        grps <- list(group)
        names(grps) <- name
        devStateGroups <- grps
    } else {
        devStateGroups[[name]] <- group
    }
    grid.Call.graphics(C_setGridState,
                       as.integer(groupIndex - 1),
                       devStateGroups)
}

lookupGroup <- function(name) {
    ## Resolve group definition to ref (using 'grid' state)
    devState <- get(".GRID.STATE", envir=.GridEvalEnv)[[dev.cur() - 1]]
    devState[[groupIndex]][[name]]
}

##########################
## Transforms (for grid.use())

groupTranslate <- function(dx=0, dy=0) {
    translate <- diag(3)
    translate[3, 1] <- dx
    translate[3, 2] <- dy
    translate
}

defnTranslate <- function(group, inverse=FALSE, device=TRUE, ...) {
    if (device) {
        xy <- group$xy
    } else {
        xy <- group$xyin
    }
    if (inverse) {
        groupTranslate(-xy$x, -xy$y)
    } else {
        groupTranslate(xy$x, xy$y)
    }
}

useTranslate <- function(group, inverse=FALSE, device=TRUE, ...) {
    cvp <- current.viewport()
    xy <- deviceLoc(unit(resolveHJust(cvp$just, cvp$hjust), "npc"),
                    unit(resolveVJust(cvp$just, cvp$vjust), "npc"),
                    valueOnly=TRUE, device=device)
    if (inverse) {
        groupTranslate(-xy$x, -xy$y)
    } else {
        groupTranslate(xy$x, xy$y)
    }
}

viewportTranslate <- function(group, device=TRUE, ...) {
    defnTranslate(group, inverse=TRUE, device=device) %*%
        useTranslate(group, device=device)
}

groupRotate <- function(r=0, device=TRUE) {
    ## Account for devices that have origin at top-left
    if (device && !.devUp()) r <- -r
    rotate <- diag(3)
    theta <- r/180*pi
    costheta <- cos(theta)
    sintheta <- sin(theta)
    rotate[1, 1] <- costheta
    rotate[1, 2] <- sintheta
    rotate[2, 1] <- -sintheta
    rotate[2, 2] <- costheta
    rotate    
}

defnRotate <- function(group, inverse=FALSE, device=TRUE, ...) {
    if (inverse) {
        groupRotate(-group$r, device)
    } else {
        groupRotate(group$r, device)
    }
}

useRotate <- function(group, inverse=FALSE, device=TRUE, ...) {
    r <- current.rotation()
    if (inverse) {
        groupRotate(-r, device)
    } else {
        groupRotate(r, device)
    }    
}

viewportRotate <- function(group, device=TRUE, ...) {
    r <- current.rotation()
    if (r != group$r) {
        defnTranslate(group, inverse=TRUE, device=device) %*%
            groupRotate(r - group$r, device=device) %*%
            defnTranslate(group, device=device)
    } else {
        diag(3)
    }
}

groupScale <- function(sx=1, sy=1) {
    scale <- diag(3)
    scale[1, 1] <- sx
    scale[2, 2] <- sy
    scale    
}

defnScale <- function(group, inverse=FALSE, ...) {
    diag(1)
}

useScale <- function(group, inverse=FALSE, ...) {
    wh <- c(convertX(unit(1, "npc"), "in", valueOnly=TRUE),
            convertY(unit(1, "npc"), "in", valueOnly=TRUE))
    if (inverse) {
        groupScale(group$wh[1]/wh[1], group$wh[2]/wh[2])
    } else {
        groupScale(wh[1]/group$wh[1], wh[2]/group$wh[2])
    }
}

viewportScale <- function(group, device=TRUE, ...) {
    defnTranslate(group, inverse=TRUE, device=device) %*%
        useScale(group) %*%
        defnTranslate(group, device=device)
}

groupShear <- function(sx=0, sy=0, device=TRUE) {
    shear <- diag(3)
    shear[1, 2] <- sy
    shear[2, 1] <- sx
    shear        
}

zeroShear <- function(device=TRUE) {
    groupShear(device=device)
}

groupFlip <- function(flipX=FALSE, flipY=FALSE) {
    flip <- diag(3)
    if (flipX)
        flip[1, 1] <- -1
    if (flipY)
        flip[2, 2] <- -1
    flip
}

noFlip <- function() {
    groupFlip()
}

viewportTransform <- function(group,
                              shear=groupShear(),
                              flip=groupFlip(),
                              device=TRUE,
                              ...) {
    r <- current.rotation()
    ## Account for devices that have origin at top-left
    if (device && !.devUp()) {
        shear[1, 2] <- -shear[1, 2]
        shear[2, 1] <- -shear[2, 1]
    }
    defnTranslate(group, inverse=TRUE, device=device) %*%
        flip %*%
        useScale(group) %*%
        shear %*%
        groupRotate(r - group$r, device=TRUE) %*%
        useTranslate(group, device=device)
}

##########################
## Simple interface:  define and use group in one
drawDetails.GridGroup <- function(x, recording) {
    grp <- finaliseGroup(x)
    ref <- .defineGroup(grp$src, grp$op, grp$dst)
    ## Record group to allow later reuse
    recordGroup(x$name, ref, x$coords)
    if (is.null(ref))
        warning("Group definition failed")
    else 
        .useGroup(ref, NULL)
}

groupGrob <- function(src,
                      op = "over",
                      dst = NULL,
                      coords = TRUE,
                      name = NULL, gp=gpar(), vp=NULL) {
    
    if (!is.grob(src))
        stop("Invalid source")
    if (!(is.grob(dst) || is.null(dst)))
        stop("Invalid destination")
    ## Check valid 'op'
    .opIndex(op)
    group <- gTree(src=src, op=op, dst=dst, coords=coords,
                   name=name, gp=gp, vp=vp, cl="GridGroup")
    group
}

grid.group <- function(src,
                       op = "over",
                       dst = NULL,
                       coords = TRUE, 
                       name = NULL, gp=gpar(), vp=NULL) {
    grid.draw(groupGrob(src, op, dst, coords, name, gp, vp))
}

##########################
## More complex interface:  separate define group and use group

drawDetails.GridDefine <- function(x, recording) {
    group <- finaliseGroup(x)
    ref <- .defineGroup(group$src, group$op, group$dst)
    recordGroup(x$name, ref, x$coords)
}
    
defineGrob <- function(src,
                       op = "over",
                       dst = NULL,
                       coords = TRUE,
                       name = NULL, gp=gpar(), vp=NULL) {
    if (!is.grob(src))
        stop("Invalid source")
    if (!(is.grob(dst) || is.null(dst)))
        stop("Invalid destination")
    ## Check valid 'op'
    .opIndex(op)
    group <- gTree(src=src, op=op, dst=dst, coords=coords,
                   name=name, gp=gp, vp=vp, cl="GridDefine")
    group
}

grid.define <- function(src,
                        op = "over",
                        dst = NULL,
                        coords = TRUE,
                        name = NULL, gp=gpar(), vp=NULL) {
    grid.draw(defineGrob(src, op, dst, coords, name, gp, vp))
}

drawDetails.GridUse <- function(x, recording) {
    group <- lookupGroup(x$group)
    if (is.null(group))
        warning(paste0("Unknown group: ", x$group))
    else {
        transform <- x$transform(group)
        if (!is.matrix(transform) ||
            !is.numeric(transform) ||
            !all(dim(transform) == 3)) {
            warning("Invalid transform (nothing drawn)")
            return()
        }
        .useGroup(group$ref, transform)
    }
}

useGrob <- function(group, transform=viewportTransform,
                    name=NULL, gp=gpar(), vp=NULL) {
    if (!is.function(transform))
        stop("Invalid transform")
    grp <- gTree(group=as.character(group), transform=transform,
                 name=name, gp=gp, vp=vp, cl="GridUse")
    grp
}

grid.use <- function(group, transform=viewportTransform,
                     name=NULL, gp=gpar(), vp=NULL) {
    grid.draw(useGrob(group, transform, name, gp, vp))
}
                        
################################
## Other grob methods

groupPoints <- function(x, closed, ...) {
    if (is.null(x$dst))
        children <- gList(x$src)
    else
        children <- gList(x$src, x$dst)
    grobPoints(gTree(children=children), closed, ...)
}
    
grobCoords.GridGroup <- function(x, closed, ...) {
    if (is.null(x$dst))
        children <- gList(x$src)
    else
        children <- gList(x$src, x$dst)
    grobCoords(gTree(children=children, gp=x$gp, vp=x$vp),
               closed, ...)
}

## NOTE that we still create a gTree so that grobPoints.gTree(),
## via grobPoints.gList(), will still call grobCoords() on the
## "child" src and dst
grobPoints.GridGroup <- function(x, closed, ...) {
    groupPoints(x, closed, ...)
}

## A group definition does not draw anything so no coords
grobCoords.GridDefine <- function(x, closed, ...) {
    emptyGrobCoords
}

grobPoints.GridDefine <- function(x, closed, ...) {
    emptyGrobCoords
}

## A group use retrieves points from group definition
## and applies transformation to them
transformCoords <- function(coords, transform) {
    UseMethod("transformCoords")
}

transformCoords.GridCoords <- function(coords, transform) {
    new <- cbind(coords$x, coords$y, 1) %*% transform
    gridCoords(new[, 1], new[, 2])
}

transformCoords.GridGrobCoords <- function(coords, transform) {
    new <- lapply(coords, transformCoords, transform)
    gridGrobCoords(new, attr(coords, "name"), attr(coords, "rule"))
}

transformCoords.GridGTreeCoords <- function(coords, transform) {
    new <- lapply(coords, transformCoords, transform)
    gridGTreeCoords(new, attr(coords, "name"))
}

usePoints <- function(x, closed, ...) {
    group <- lookupGroup(x$group)
    if (is.null(group)) {
        warning(paste0("Unknown group: ", x$group))
        emptyGrobCoords
    } else {
        transform <- x$transform(group, device=FALSE)
        if (!is.matrix(transform) ||
            !is.numeric(transform) ||
            !all(dim(transform) == 3)) {
            warning("Invalid transform")
            emptyGrobCoords
        }
        ## Apply inverse of viewport transform
        ## (because grobCoords() are supposed to be relative to
        ##  current viewport NOT device)
        transform <- transform %*% solve(current.transform())
        if (closed) {
            transformCoords(group$closedPoints, transform)
        } else {
            transformCoords(group$openPoints, transform)
        }
    }
}

grobCoords.GridUseChild <- function(x, closed, ...) {
    usePoints(x, closed, ...)
}

grobCoords.GridUse <- function(x, closed, ...) {
    ## Create gTree to get automatic enforcement of 'gp' and 'vp'
    ## BUT avoid infinite loop by setting temporary class
    ## on GridUse grob
    class(x) <- c("GridUseChild", class(x))
    grobCoords(gTree(children=gList(x), gp=x$gp, vp=x$vp),
               closed, ...)
}

grobPoints.GridUse <- function(x, closed, ...) {
    usePoints(x, closed, ...)
}
