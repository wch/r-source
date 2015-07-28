#  File src/library/grid/R/debug.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1995-2013 The R Core Team
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

### Label grobs in a scene


labelGrob <- function(grob, recurse, curdepth, depth, labelfun, ...) {
    UseMethod("labelGrob")
}

# The default grob label needs to do some calculations
# on sizes so need a drawDetails method to get the
# calculations right
drawDetails.groblabel <- function(x, ...) {
    gw <- convertWidth(grobWidth(x$grob), "inches", valueOnly=TRUE)
    gh <- convertHeight(grobHeight(x$grob), "inches", valueOnly=TRUE)
    grid.rect(grobX(x$grob, "west"), grobY(x$grob, "south"),
              unit(gw, "inches"), unit(gh, "inches"),
              just=c("left", "bottom"), gp=x$gp)
    tw <- convertWidth(stringWidth(x$grob$name), "inches", valueOnly=TRUE)
    th <- convertHeight(stringHeight(x$grob$name), "inches", valueOnly=TRUE)
    eps <- .01
    # If grob is REALLY short, draw horiz at normal cex
    if (gh < eps) {
        rot <- 0
        cex <- 1
    # If grob is REALLY thin, draw vertical at normal cex
    } else if (gw < eps) {
        rot <- 90
        cex <- 1
    } else {
        gratio <- gh/gw
        if (gratio > 1 && tw > gw) {
            rot <- 90
            wratio <- th/gw
            hratio <- tw/gh
        } else {
            rot <- 0
            wratio <- tw/gw
            hratio <- th/gh
        }
        if (wratio > 1 || hratio > 1) {
            cex <- 1/max(wratio, hratio)
        } else {
            cex <- 1
        }
    }
    if (is.null(x$gp)) {
        x$gp <- gpar(cex=cex)
    } else {
        if (is.null(x$gp$cex))
            x$gp$cex <- cex
    }
    if (is.null(x$otherArgs$rot))
        x$otherArgs$rot <- rot
    do.call("grid.text", c(list(label=x$grob$name,
                                x=grobX(x$grob, "north"),
                                y=grobY(x$grob, "west"),
                                gp=x$gp),
                           x$otherArgs))
}

grobLabel <- function(grob,
                      gp=gpar(col=rgb(1, 0, 0, .5),
                        fill=rgb(1, 0, 0, .2)),
                      ...) {
    grob(grob=grob, gp=gp, otherArgs=list(...),
         cl="groblabel")
}

labelGrob.grob <- function(grob, recurse, curdepth, depth, labelfun, ...) {
    if (is.null(depth) || curdepth %in% depth) {
        gTree(children=gList(grob,
                labelfun(grob, ...)),
              # Name new gTree same as old grob so that
              # setGrob() approach works below
              # (when 'gPath' is specified)
              name=grob$name)
    } else {
        grob
    }
}

labelGrob.gTree <- function(grob, recurse, curdepth, depth, labelfun, ...) {
    if (recurse) {
        newChildren <- do.call("gList",
                               lapply(grob$children,
                                      labelGrob,
                                      recurse, curdepth + 1, depth,
                                      labelfun, ...))
        grob <- setChildren(grob, newChildren)
    }
    if (is.null(depth) || curdepth %in% depth) {
        gTree(children=gList(grob,
                labelfun(grob, ...)),
              name=grob$name)
    } else {
        grob
    }
}

showGrob <- function(x=NULL,
                     gPath=NULL, strict=FALSE, grep=FALSE,
                     recurse=TRUE, depth=NULL,
                     labelfun=grobLabel, ...) {
    if (is.null(x)) {
        # Label all or part of current scene
        # The grid display list is NOT affected
        # To remove labels use grid.redraw()
        if (is.null(gPath)) {
            # Show the current scene
            dl <- grid.Call(L_getDisplayList)[1L : grid.Call(L_getDLindex)]
            grid.newpage(recording=FALSE)
            # -1 because first element on DL is ROOT viewport
            lapply(dl[-1],
                   function(y) {
                       # Modify the grob to add a label
                       if (is.grob(y))
                           y <- labelGrob(y, recurse, 1, depth, labelfun, ...)
                       # Draw either the original object or the modified grob
                       grid.draw(y, recording=FALSE)
                   })
        } else {
            # Only label the bit of the current scene specified by gPath
            grobToLabel <- grid.get(gPath, strict=strict, grep=grep)
            # NOTE: have to 'wrap' because otherwise the grobs in the
            # captured scene have been altered
            scene <- grid.grab(wrap=TRUE)
            modScene <- setGrob(scene, gPath,
                                labelGrob(grobToLabel, recurse, 1, depth,
                                          labelfun, ...),
                                strict=strict, grep=grep)
            grid.newpage(recording=FALSE)
            grid.draw(modScene, recording=FALSE)
        }
    } else {
        # Assume grob is not current scene so start a new page
        grid.newpage()
        grid.draw(x)
        showGrob(NULL, gPath, strict, grep, recurse, depth, labelfun, ...)
    }
    invisible()
}

#############
# Labelling viewports in a scene
#############

# FIXME:  some of this code for vpLists and vpStacks and vpTrees
# assumes that the components of a vpList or vpStack or the
# vpTree parent can ONLY be a viewport (when in fact they can
# also be a vpList, vpStack, or vpTree!)

# Label a viewport
# Get physical aspect ratio of vp to determine whether to rotate
# Shrink text to fit in vp
# (Assumes that we are currently occupying 'vp'
#  so that conversions are correct)
labelVP <- function(vp, col) {
    vw <- convertWidth(unit(1, "npc"), "inches", valueOnly=TRUE)
    vh <- convertHeight(unit(1, "npc"), "inches", valueOnly=TRUE)
    tw <- convertWidth(stringWidth(vp$name), "inches", valueOnly=TRUE)
    th <- convertHeight(stringHeight(vp$name), "inches", valueOnly=TRUE)
    eps <- .01
    # If viewport is REALLY short, draw horiz at normal cex
    if (vh < eps) {
        rot <- 0
        cex <- 1
    # If viewport is REALLY thin, draw vertical at normal cex
    } else if (vw < eps) {
        rot <- 90
        cex <- 1
    } else {
        vratio <- vh/vw
        if (vratio > 1 && tw > vw) {
            rot <- 90
            wratio <- th/vw
            hratio <- tw/vh
        } else {
            rot <- 0
            wratio <- tw/vw
            hratio <- th/vh
        }
        if (wratio > 1 || hratio > 1) {
            cex <- 1/max(wratio, hratio)
        } else {
            cex <- 1
        }
    }
    # Violate any clipping that is in effect
    pushViewport(viewport(clip="off"))
    grid.text(vp$name, rot=rot, gp=gpar(col=col, cex=cex))
    upViewport()
}

# Draw a "viewport"
drawVP <- function(vp, curDepth, depth, col, fill, label) {
    UseMethod("drawVP")
}

drawVP.viewport <- function(vp, curDepth, depth, col, fill, label) {
    if (vp$name != "ROOT" &&
        (is.null(depth) || curDepth %in% depth)) {
        pushViewport(vp)
        colIndex <- (curDepth - 1) %% length(col) + 1
        fillIndex <- (curDepth - 1) %% length(fill) + 1
        grid.rect(gp=gpar(col=col[colIndex], fill=fill[fillIndex]))
        if (label)
            labelVP(vp, col[colIndex])
        upViewport()
    }
}

drawVP.vpPath <- function(vp, curDepth, depth, col, fill, label) {
    if (is.null(depth) || curDepth %in% depth) {
        downViewport(vp)
        colIndex <- (curDepth - 1) %% length(col) + 1
        fillIndex <- (curDepth - 1) %% length(fill) + 1
        grid.rect(gp=gpar(col=col[colIndex], fill=fill[fillIndex]))
        if (label)
            labelVP(vp, col[colIndex])
        upViewport(depth(vp))
    }
}

drawVP.vpList <- function(vp, curDepth, depth, col, fill, label) {
    lapply(vp, drawVP, curDepth, depth, col, fill, label)
}

drawVP.vpStack <- function(vp, curDepth, depth, col, fill, label) {
    d <- depth(vp)
    for (i in 1:length(vp)) {
        this <- vp[[i]]
        drawVP(this, curDepth, depth, col, fill, label)
        curDepth <- curDepth + depth(this)
        pushViewport(this)
    }
    upViewport(d)
}

drawVP.vpTree <- function(vp, curDepth, depth, col, fill, label) {
    if (vp$parent$name == "ROOT") {
        lapply(vp$children, drawVP, curDepth, depth, col, fill, label)
    } else {
        pushViewport(vp$parent)
        if (is.null(depth) || curDepth %in% depth) {
            colIndex <- (curDepth - 1) %% length(col) + 1
            fillIndex <- (curDepth - 1) %% length(fill) + 1
            grid.rect(gp=gpar(col=col[colIndex], fill=fill[fillIndex]))
            if (label) {
                drawLabel <- is.null(vp$children) ||
                             (!is.null(depth) &&
                              curDepth == max(depth))
                if (drawLabel)
                    labelVP(vp$parent, col[colIndex])
            }
        }
        lapply(vp$children, drawVP, curDepth + 1, depth, col, fill, label)
        upViewport()
    }
}

# Draw all viewports in same viewport
showVP <- function(vp, newpage, cvpt, depth, col, fill,
                   label) {
    # If we've started a new page, we'll need the old
    # viewport tree to navigate within
    if (newpage) {
        pushViewport(cvpt)
        # "-1" for "ROOT"
        upViewport(depth(cvpt) - 1)
    }
    # Work off a vpTree, so convert vp if it's a vpPath
    showingPath <- inherits(vp, "vpPath")
    if (showingPath) {
        path <- vp
        downViewport(path)
        vp <- current.vpTree(all=FALSE)
        upViewport(1)
    }
    drawVP(vp, 1, depth, col, fill, label)
    if (showingPath)
        # "-1" because we went down the path then back up 1 originally
        upViewport(depth(path) - 1)
    invisible()
}

# Convert a "viewport" to a set of vpPaths
leafPaths <- function(vp) {
    UseMethod("leafPaths")
}

leafPaths.viewport <- function(vp) {
    if (vp$name == "ROOT")
        NULL
    else
        vp$name
}

leafPaths.vpList <- function(vp) {
    unlist(lapply(vp, leafPaths))
}

leafPaths.vpStack <- function(vp) {
    pathList <- lapply(vp, leafPaths)
    for (i in 1:length(pathList)) {
        if (i > 1) {
            pathList[[i]] <- paste(pathList[[i - 1]],
                                   pathList[[i]],
                                   sep=.grid.pathSep)
        }
    }
    unlist(pathList)
}

leafPaths.vpTree <- function(vp) {
    if (is.null(vp$children)) {
        if (vp$parent$name == "ROOT")
            NULL
        else
            vp$parent$name
    } else {
        pathList <- lapply(vp$children, leafPaths)
        if (vp$parent$name == "ROOT") {
            unlist(pathList)
        } else {
            paste(vp$parent$name,
                  unlist(pathList),
                  sep=.grid.pathSep)
        }
    }
}

leafPaths.vpPath <- function(vp) {
    as.character(vp)
}

# Draw a vpPath
drawPath <- function(path, depth, col, fill, label) {
    n <- depth(path)
    for (i in 1:n) {
        downViewport(path[i])
        if (is.null(depth) || i %in% depth) {
            colIndex <- (i - 1) %% length(col) + 1
            fillIndex <- (i - 1) %% length(fill) + 1
            grid.rect(gp=gpar(col=col[colIndex], fill=fill[fillIndex]))
            if (label) {
                if (is.null(depth))
                    drawLabel <- i == n
                else
                    drawLabel <- i == min(n, max(depth))
                if (drawLabel)
                    labelVP(current.viewport(), col[colIndex])
            }
        }
    }
    upViewport(n)
}

# Draw each leaf in separate viewports
# FIXME: allow control over number of rows and cols
# NOTE: this does NOT leave its viewports hanging around after
showVPmatrix <- function(vp, cvpt, depth, col, fill,
                         label, # Only the leaf viewports are labelled
                         nrow, ncol) {
    # Work off a vpPath, so convert vp if it's a "viewport"
    if (is.viewport(vp)) {
        paths <- leafPaths(vp)
    } else {
        # Should not happen
        stop("how did we get here?")
    }
    firstPath <- 0
    while (length(paths) - firstPath > 0) {
        if (firstPath > 0)
            grid.newpage()
        pushViewport(viewport(layout=grid.layout(nrow, ncol)))
        for (i in 1:nrow) {
            for (j in 1:ncol) {
                theLeaf <- firstPath + (i - 1)*nrow + j
                if (theLeaf <= length(paths)) {
                    thePath <- vpPath(paths[theLeaf])
                    pushViewport(viewport(layout.pos.row=i,
                                          layout.pos.col=j))
                    grid.rect(gp=gpar(col="grey80"))
                    # We may need the old vpTree to navigate within
                    # if 'vp' is a vpStack, or something similar, that
                    # contains a vpPath
                    if (!is.null(cvpt$children)) {
                        pushViewport(cvpt$children)
                        upViewport(depth(cvpt) - 1)
                    }
                    # Now push the viewport we are showing
                    pushViewport(vp)
                    upViewport(depth(vp))
                    # Now go to the particular viewport we
                    # are going to show
                    drawPath(thePath, depth, col, fill, label)
                    # Pop our placement within the layout
                    popViewport()
                }
            }
        }
        popViewport()
        firstPath <- firstPath + nrow*ncol
    }
}

showViewport <- function(vp=NULL, recurse=TRUE, depth=NULL,
                         newpage=FALSE, leaves=FALSE,
                         col=rgb(0, 0, 1, .2), fill=rgb(0, 0, 1, .1),
                         label=TRUE, nrow=3, ncol=nrow) {
    cvpt <- current.vpTree()
    if (is.null(vp))
        vp <- cvpt
    if (newpage == FALSE && leaves == TRUE)
        stop("must start new page if showing leaves separately")
    if (newpage) {
        grid.newpage()
    }
    if (!recurse)
        depth <- 1
    if (leaves) {
        # Special case of showing vpPath (i.e., only one viewport)
        # Ignores nrow & ncol
        if (inherits(vp, "vpPath"))
            showVP(vp, TRUE, cvpt, depth, col, fill, label)
        else
            showVPmatrix(vp, cvpt, depth, col, fill, label, nrow, ncol)
    } else {
        showVP(vp, newpage, cvpt, depth, col, fill, label)
    }
    invisible()
}
