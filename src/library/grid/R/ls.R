#  File src/library/grid/R/ls.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1995-2012 The R Core Team
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


# Code for listing objects in various grid "namespaces"
# (gTrees, vpTrees, and the grid display list)

# Return a "gridListing" object,
# ... either ...
# "gridVectorListing", which is just character vector,
#     "grobListing", or "vpListing", or "vpNameListing", or
#     "vpPopListing", or "vpUpListing",
# ... or ...
# "gridListListing", which is list of "gridListing" objects,
#      "grobListListing", or "vpListListing", ...
# ... or ...
# "gridTreeListing", which is list of parent "gridVectorListing" object
#                    plus children "gridListing" object,
#      "gTreeListing", or "vpTreeListing", or "vpNameTreeListing"
#      (vpStack or vpTree produces a "vpTreeListing").
#      (vpPath [depth > 1] produces a "vpNameTreeListing").
#
# "vpListListing", and all "gridTreeListing" objects have a "depth" attribute

# The print method will print these in some format, but by having
# a separate object, others can capture the result and format the
# printing themselves.

grid.ls <- function(x=NULL, grobs=TRUE, viewports=FALSE, fullNames=FALSE,
                    recursive=TRUE, print=TRUE, flatten=TRUE, ...) {
    # If 'x' is NULL, list the grobs on the DL
    if (is.null(x)) {
        listing <- gridListDL(grobs=grobs, viewports=viewports,
                              fullNames=fullNames, recursive=recursive)
    } else {
        listing <- gridList(x, grobs=grobs, viewports=viewports,
                            fullNames=fullNames, recursive=recursive)
    }
    if (flatten) {
        listing <- flattenListing(listing)
    }
    if (is.logical(print)) {
        if (print) {
            print(listing)
        }
    } else if (is.function(print)) {
        print(listing, ...)
    } else {
        stop("invalid 'print' argument")
    }
    invisible(listing)
}

gridListDL <- function(x, grobs=TRUE, viewports=FALSE,
                       fullNames=FALSE, recursive=TRUE) {
    display.list <- grid.Call(L_getDisplayList)
    dl.index <- grid.Call(L_getDLindex)
    result <- lapply(display.list[1L:dl.index], gridList,
                     grobs=grobs, viewports=viewports,
                     fullNames=fullNames, recursive=recursive)
    names(result) <- NULL
    class(result) <- c("gridListListing", "gridListing")
    result
}

gridList <- function(x, ...) {
    UseMethod("gridList")
}

gridList.default <- function(x, grobs=TRUE, viewports=FALSE,
                             fullNames=FALSE, recursive=TRUE) {
    if (is.null(x)) {
        # This handles empty slots in the display list
        result <- character()
        class(result) <- "gridListing"
    } else {
        stop("invalid object in 'listing'")
    }
    result
}

# Grob methods
gridList.grob <- function(x, grobs=TRUE, viewports=FALSE,
                          fullNames=FALSE, recursive=TRUE) {
    if (grobs) {
        if (fullNames) {
            result <- as.character(x)
        } else {
            result <- x$name
        }
        class(result) <- c("grobListing", "gridVectorListing", "gridListing")
    } else {
        result <- character()
        class(result) <- "gridListing"
    }
    if (viewports) {
        # Call makeContext() to get x$vp at drawing time
        x <- makeContext(x)
    }
    if (viewports && !is.null(x$vp)) {
        # Bit dodgy this bit
        # Emulates an "upViewport" on the DL
        n <- depth(x$vp)
        class(n) <- "up"
        result <- list(gridList(x$vp,
                               grobs=grobs, viewports=viewports,
                               fullNames=fullNames,
                               recursive=recursive),
                       result,
                       gridList(n,
                               grobs=grobs, viewports=viewports,
                               fullNames=fullNames,
                               recursive=recursive))
        class(result) <- c("gridListListing", "gridListing")
    }
    result
}

gridList.gList <- function(x, grobs=TRUE, viewports=FALSE,
                           fullNames=FALSE, recursive=TRUE) {
    # Allow for grobs=FALSE but viewports=TRUE
    if (grobs || viewports) {
        if (length(x) == 0L) {
            result <- character()
            class(result) <- "gridListing"
        } else {
            result <- lapply(x, gridList,
                             grobs=grobs, viewports=viewports,
                             fullNames=fullNames, recursive=recursive)
            class(result) <- c("gListListing", "gridListListing",
                               "gridListing")
        }
    } else {
        result <- character()
        class(result) <- "gridListing"
    }
    result
}

gridList.gTree <- function(x, grobs=TRUE, viewports=FALSE,
                           fullNames=FALSE, recursive=TRUE) {
    if (fullNames) {
        name <- as.character(x)
    } else {
        name <- x$name
    }
    class(name) <- c("grobListing", "gridVectorListing", "gridListing")
    if (viewports) {
        # Call makeContext() to get x$vp and x$childrenvp at drawing time
        x <- makeContext(x)
    }
    if (recursive) {
        # Allow for grobs=FALSE but viewports=TRUE
        result <- gridList(x$children[x$childrenOrder],
                           grobs=grobs, viewports=viewports,
                           fullNames=fullNames, recursive=recursive)
        if (viewports && !is.null(x$childrenvp)) {
            # Bit dodgy this bit
            # Emulates an "upViewport" on the DL
            n <- depth(x$childrenvp)
            class(n) <- "up"
            result <- list(gridList(x$childrenvp,
                                    grobs=grobs, viewports=viewports,
                                    fullNames=fullNames,
                                    recursive=recursive),
                           gridList(n,
                                    grobs=grobs, viewports=viewports,
                                    fullNames=fullNames,
                                    recursive=recursive),
                           result)
            class(result) <- c("gridListListing", "gridListing")
        }
        if (grobs) {
            result <- list(parent=name,
                           children=result)
            class(result) <- c("gTreeListing", "gridTreeListing",
                               "gridListing")
        } else if (!viewports) {
            result <- character()
            class(result) <- "gridListing"
        }
    } else {
        if (grobs) {
            result <- name
        } else {
            result <- character()
            class(result) <- "gridListing"
        }
    }
    if (viewports && !is.null(x$vp)) {
        # Bit dodgy this bit
        # Emulates an "upViewport" on the DL
        n <- depth(x$vp)
        class(n) <- "up"
        result <- list(gridList(x$vp,
                                grobs=grobs, viewports=viewports,
                                fullNames=fullNames,
                                recursive=recursive),
                       result,
                       gridList(n,
                                grobs=grobs, viewports=viewports,
                                fullNames=fullNames,
                                recursive=recursive))
        class(result) <- c("gridListListing", "gridListing")
    }
    result
}

# Viewport methods
gridList.viewport <- function(x, grobs=TRUE, viewports=FALSE,
                              fullNames=FALSE, recursive=TRUE) {
    if (viewports) {
        if (fullNames) {
            result <- as.character(x)
        } else {
            result <- x$name
        }
        class(result) <- c("vpListing", "gridVectorListing", "gridListing")
    } else {
        result <- character()
        class(result) <- "gridListing"
    }
    result
}

# ... are arugments to gridList
listvpListElement <- function(x, ...) {
    n <- depth(x)
    class(n) <- "up"
    result <- list(gridList(x, ...),
                   gridList(n, ...))
    class(result) <- c("gridListListing", "gridListing")
    result
}

gridList.vpList <- function(x, grobs=TRUE, viewports=FALSE,
                            fullNames=FALSE, recursive=TRUE) {
    if (viewports) {
        if (length(x) == 0L) {
            result <- character()
            class(result) <- "gridListing"
        } else if (length(x) == 1L) {
            result <- gridList(x[[1L]],
                              grobs=grobs, viewports=viewports,
                              fullNames=fullNames,
                              recursive=recursive)
        } else {
            result <- c(lapply(x[-length(x)], listvpListElement,
                               grobs=grobs, viewports=viewports,
                               fullNames=fullNames,
                               recursive=recursive),
                        list(gridList(x[[length(x)]],
                                     grobs=grobs, viewports=viewports,
                                     fullNames=fullNames,
                                     recursive=recursive)))
            attr(result, "depth") <- depth(x[[length(x)]])
            class(result) <- c("vpListListing", "gridListListing",
                               "gridListing")
        }
    } else {
        result <- character()
        class(result) <- "gridListing"
    }
    result
}

gridList.vpStack <- function(x, grobs=TRUE, viewports=FALSE,
                             fullNames=FALSE, recursive=TRUE) {
    if (viewports) {
        if (length(x) == 0L) {
            result <- character()
            class(result) <- "gridListing"
        } else if (length(x) == 1L || !recursive) {
            result <- gridList(x[[1L]],
                               grobs=grobs, viewports=viewports,
                               fullNames=fullNames, recursive=recursive)
        } else {
            theRest <- x[-1L]
            class(theRest) <- "vpStack"
            result <- gridList(theRest,
                               grobs=grobs, viewports=viewports,
                               fullNames=fullNames,
                               recursive=recursive)
            result <- list(parent=gridList(x[[1L]],
                             grobs=grobs, viewports=viewports,
                             fullNames=fullNames,
                             recursive=recursive),
                           children=result)
            attr(result, "depth") <- depth(x)
            class(result) <- c("vpTreeListing", "gridTreeListing",
                               "gridListing")
        }
    } else {
        result <- character()
        class(result) <- "gridListing"
    }
    result
}

gridList.vpTree <- function(x, grobs=TRUE, viewports=FALSE,
                            fullNames=FALSE, recursive=TRUE) {
    if (viewports) {
        if (recursive) {
            result <- gridList(x$children,
                               grobs=grobs, viewports=viewports,
                               fullNames=fullNames, recursive=recursive)
            # Parent can only be a plain viewport
            result <- list(parent=gridList(x$parent,
                             grobs=grobs, viewports=viewports,
                             fullNames=fullNames,
                             recursive=recursive),
                           children=result)
            attr(result, "depth") <- depth(x$children) + 1
            class(result) <- c("vpTreeListing", "gridTreeListing",
                               "gridListing")
        } else {
            result <- gridList(x$parent,
                               grobs=grobs, viewports=viewports,
                               fullNames=fullNames, recursive=recursive)
        }
    } else {
        result <- character()
        class(result) <- "gridListing"
    }
    result
}

# This handles downViewports in the display list
gridList.vpPath <- function(x, grobs=TRUE, viewports=FALSE,
                            fullNames=FALSE, recursive=TRUE) {
    if (viewports) {
        # Have to account for top-level downViewports that are
        # non-strict (i.e., they could navigate down quite a long way)
        # In particular, when the vpPath navigates down more
        # levels than there are names in the vpPath
        recordedDepth <- attr(x, "depth")
        if (!is.null(recordedDepth) && recordedDepth != depth(x)) {
            # In this case, need to prepend a fake path on the front
            # so that subsequent upViewport()s will work
            x <- vpPathFromVector(c(rep("...", recordedDepth - depth(x)),
                                    explodePath(as.character(x))))
        }
        # This would be simpler if paths were kept as vectors
        # but that redesign is a bit of an undertaking
        if (depth(x) == 1) {
            if (fullNames) {
                result <- paste0("downViewport[", x$name, "]")
            } else {
                result <- x$name
            }
            class(result) <- c("vpNameListing", "gridVectorListing",
                               "gridListing")
        } else if (depth(x) == 2) {
            result <- gridList(vpPath(x$name),
                               grobs=grobs, viewports=viewports,
                               fullNames=fullNames,
                               recursive=recursive)
            result <- list(parent=gridList(vpPath(x$path),
                             grobs=grobs, viewports=viewports,
                             fullNames=fullNames,
                             recursive=recursive),
                           children=result)
            attr(result, "depth") <- depth(x)
            # Inherit updateVPDepth and updateVPPath methods
            # from vpTreeListing
            class(result) <- c("vpNameTreeListing", "vpTreeListing",
                               "gridTreeListing", "gridListing")
        } else {
            path <- explodePath(x$path)
            result <- gridList(vpPathFromVector(c(path[-1L], x$name)),
                               grobs=grobs, viewports=viewports,
                               fullNames=fullNames,
                               recursive=recursive)
            result <- list(parent=gridList(vpPath(path[1L]),
                             grobs=grobs, viewports=viewports,
                             fullNames=fullNames,
                             recursive=recursive),
                           children=result)
            attr(result, "depth") <- depth(x)
            # Inherit updateVPDepth and updateVPPath methods
            # from vpTreeListing
            class(result) <- c("vpNameTreeListing", "vpTreeListing",
                               "gridTreeListing", "gridListing")
        }
    } else {
        result <- character()
        class(result) <- "gridListing"
    }
    result
}

# This handles popViewports in the display list
gridList.pop <- function(x, grobs=TRUE, viewports=FALSE,
                         fullNames=FALSE, recursive=TRUE) {
    if (viewports) {
        result <- as.character(x)
        if (fullNames) {
            result <- paste0("popViewport[", result, "]")
        }
        class(result) <- c("vpPopListing", "gridVectorListing", "gridListing")
    } else {
        result <- character()
        class(result) <- "gridListing"
    }
    result
}

# This handles upViewports in the display list
gridList.up <- function(x, grobs=TRUE, viewports=FALSE,
                        fullNames=FALSE, recursive=TRUE) {
    if (viewports) {
        result <- as.character(x)
        if (fullNames) {
            result <- paste0("upViewport[", result, "]")
        }
        class(result) <- c("vpUpListing", "gridVectorListing", "gridListing")
    } else {
        result <- character()
        class(result) <- "gridListing"
    }
    result
}

######################
# flatten methods for gridListing objects
######################

incDepth <- function(depth, n=1) {
    depth + n
}

decrDepth <- function(depth, x) {
    n <- as.numeric(gsub("^.+\\[", "",
                         gsub("\\]$", "",
                              as.character(x))))
    depth - n
}

# updateDepth modifies depth from sibling to sibling
# (flatListing methods take care of parent to child updates of depth)
updateGDepth <- function(x, gdepth) {
    UseMethod("updateGDepth")
}

updateGDepth.default <- function(x, gdepth) {
    gdepth
}

updateVPDepth <- function(x, vpdepth) {
    UseMethod("updateVPDepth")
}

updateVPDepth.default <- function(x, vpdepth) {
    vpdepth
}

updateVPDepth.vpListing <- function(x, vpdepth) {
    incDepth(vpdepth)
}

updateVPDepth.vpNameListing <- function(x, vpdepth) {
    incDepth(vpdepth)
}

updateVPDepth.vpListListing <- function(x, vpdepth) {
    incDepth(vpdepth, attr(x, "depth"))
}

updateVPDepth.vpUpListing <- function(x, vpdepth) {
    decrDepth(vpdepth, x)
}

updateVPDepth.vpPopListing <- function(x, vpdepth) {
    decrDepth(vpdepth, x)
}

updateVPDepth.vpTreeListing <- function(x, vpdepth) {
    incDepth(vpdepth, attr(x, "depth"))
}

incPath <- function(oldpath, addition) {
    if (nchar(oldpath) > 0) {
        paste0(oldpath, .grid.pathSep, as.character(addition))
    } else {
        as.character(addition)
    }
}

decrPath <- function(oldpath, x) {
    bits <- strsplit(oldpath, .grid.pathSep)[[1L]]
    n <- as.numeric(gsub("^.+\\[", "",
                         gsub("\\]$", "",
                              as.character(x))))
    if ((m <- (length(bits) - n)) == 0L) {
        ""
    } else {
	paste(bits[seq_len(m)], collapse=.grid.pathSep)
    }
}

updateGPath <- function(x, gpath) {
    UseMethod("updateGPath")
}

updateGPath.default <- function(x, gpath) {
    gpath
}

updateVPPath <- function(x, vppath) {
    UseMethod("updateVPPath")
}

updateVPPath.default <- function(x, vppath) {
    vppath
}

updateVPPath.vpListing <- function(x, vppath) {
    incPath(vppath, x)
}

updateVPPath.vpNameListing <- function(x, vppath) {
    incPath(vppath, x)
}

updateVPPath.vpListListing <- function(x, vppath) {
    incPath(vppath, x[[length(x)]])
}

updateVPPath.vpUpListing <- function(x, vppath) {
    decrPath(vppath, x)
}

updateVPPath.vpPopListing <- function(x, vppath) {
    decrPath(vppath, x)
}

updateVPPath.vpTreeListing <- function(x, vppath) {
    incPath(vppath,
            paste0(updateVPPath(x$parent, ""), .grid.pathSep,
                   updateVPPath(x$children, "")))
}

flatListing <- function(x, gDepth=0, vpDepth=0, gPath="", vpPath="") {
    UseMethod("flatListing")
}

flatListing.gridListing <- function(x, gDepth=0, vpDepth=0,
                                    gPath="", vpPath="") {
    if (length(x)) {
        list(name=as.character(x),
             gDepth=gDepth,
             vpDepth=vpDepth,
             gPath=gPath,
             vpPath=vpPath,
             type=class(x)[1L])
    } else {
        list(name=character(),
             gDepth=numeric(),
             vpDepth=numeric(),
             gPath=character(),
             vpPath=character(),
             type=character())
    }
}

flatListing.gTreeListing <- function(x, gDepth=0, vpDepth=0,
                                     gPath="", vpPath="") {
    # Increase gDepth and gPath
    flatChildren <- flatListing(x$children, incDepth(gDepth, 1), vpDepth,
                                incPath(gPath, x$parent), vpPath)
    list(name=c(as.character(x$parent), flatChildren$name),
         gDepth=c(gDepth, flatChildren$gDepth),
         vpDepth=c(vpDepth, flatChildren$vpDepth),
         gPath=c(gPath, flatChildren$gPath),
         vpPath=c(vpPath, flatChildren$vpPath),
         type=c(class(x)[1L], flatChildren$type))
}

OLDflatListing.vpTreeListing <- function(x, gDepth=0, vpDepth=0,
                                      gPath="", vpPath="") {
    # Increase vpDepth and vpPath
    flatChildren <- flatListing(x$children, gDepth, incDepth(vpDepth, 1),
                                gPath, incPath(vpPath, x$parent))
    list(name=c(as.character(x$parent), flatChildren$name),
         gDepth=c(gDepth, flatChildren$gDepth),
         vpDepth=c(vpDepth, flatChildren$vpDepth),
         gPath=c(gPath, flatChildren$gPath),
         vpPath=c(vpPath, flatChildren$vpPath),
         type=c(class(x)[1L], flatChildren$type))
}

flatListing.vpTreeListing <- function(x, gDepth=0, vpDepth=0,
                                      gPath="", vpPath="") {
    flatParent <- flatListing(x$parent, gDepth, vpDepth,
                              gPath, vpPath)
    depth <- attr(x$parent, "depth")
    if (is.null(depth)) {
        depth <- 1
    }
    # Increase vpDepth and vpPath
    flatChildren <- flatListing(x$children, gDepth, incDepth(vpDepth, depth),
                                gPath, updateVPPath(x$parent, vpPath))
    list(name=c(flatParent$name, flatChildren$name),
         gDepth=c(flatParent$gDepth, flatChildren$gDepth),
         vpDepth=c(flatParent$vpDepth, flatChildren$vpDepth),
         gPath=c(flatParent$gPath, flatChildren$gPath),
         vpPath=c(flatParent$vpPath, flatChildren$vpPath),
         type=c(flatParent$type, flatChildren$type))
}

flatListing.vpNameTreeListing <- function(x, gDepth=0, vpDepth=0,
                                      gPath="", vpPath="") {
    # Increase vpDepth and vpPath
    flatChildren <- flatListing(x$children, gDepth, incDepth(vpDepth, 1),
                                gPath, incPath(vpPath, x$parent))
    list(name=c(as.character(x$parent), flatChildren$name),
         gDepth=c(gDepth, flatChildren$gDepth),
         vpDepth=c(vpDepth, flatChildren$vpDepth),
         gPath=c(gPath, flatChildren$gPath),
         vpPath=c(vpPath, flatChildren$vpPath),
         type=c(class(x)[1L], flatChildren$type))
}

flatListing.gridListListing <- function(x, gDepth=0, vpDepth=0,
                                        gPath="", vpPath="") {
    n <- length(x)
    listListing <- list(name=character(),
                        gDepth=numeric(),
                        vpDepth=numeric(),
                        gPath=character(),
                        vpPath=character(),
                        type=character())
    for (i in 1L:n) {
        componentListing <- flatListing(x[[i]], gDepth, vpDepth,
                                        gPath, vpPath)
        listListing$name <- c(listListing$name,
                              componentListing$name)
        listListing$gDepth <- c(listListing$gDepth,
                                componentListing$gDepth)
        listListing$vpDepth <- c(listListing$vpDepth,
                                 componentListing$vpDepth)
        listListing$gPath <- c(listListing$gPath,
                               componentListing$gPath)
        listListing$vpPath <- c(listListing$vpPath,
                                componentListing$vpPath)
        listListing$type <- c(listListing$type,
                              componentListing$type)
        gPath <- updateGPath(x[[i]], gPath)
        vpPath <- updateVPPath(x[[i]], vpPath)
        gDepth <- updateGDepth(x[[i]], gDepth)
        vpDepth <- updateVPDepth(x[[i]], vpDepth)
    }
    listListing
}

flattenListing <- function(x) {
    listing <- flatListing(x)
    class(listing) <- "flatGridListing"
    listing
}

print.flatGridListing <- function(x, ...) {
    nestedListing(x, ...)
    invisible(x)
}

######################
# Print functions for flatGridListings
######################

nestedListing <- function(x, gindent="  ", vpindent=gindent) {

    makePrefix <- function(indent, depth) {
        indents <- rep(indent, length(depth))
        indents <- mapply(rep, indents, depth)
        sapply(indents, paste, collapse="")
    }

    if (!inherits(x, "flatGridListing"))
        stop("invalid listing")
    cat(paste0(makePrefix(gindent, x$gDepth),
               makePrefix(vpindent, x$vpDepth),
               x$name),
        sep = "\n")
}

pathListing <- function(x, gvpSep=" | ", gAlign=TRUE) {

    appendToPrefix <- function(path, name) {
        emptyPath <- nchar(path) == 0
        ifelse(emptyPath,
               name,
               paste(path, name, sep = .grid.pathSep))
    }

    padPrefix <- function(path, maxLen) {
        numSpaces <- maxLen - nchar(path)
        if (length(path) == 1L) {
            paste0(path, paste(rep.int(" ", numSpaces), collapse=""))
        } else {
            padding <- rep(" ", length(path))
            padding <- mapply(rep.int, padding, numSpaces)
            paste0(path, sapply(padding, paste, collapse=""))
        }
    }

    if (!inherits(x, "flatGridListing"))
        stop("invalid 'listing'")
    vpListings <- seq_along(x$name) %in% grep("^vp", x$type)
    paths <- x$vpPath
    # Only if viewport listings
    if (sum(vpListings) > 0) {
        paths[vpListings] <- appendToPrefix(paths[vpListings],
                                            x$name[vpListings])
        # If viewports are shown, then allow extra space before grobs
        maxLen <- max(nchar(paths[vpListings]))
    }
    else
	maxLen <- max(nchar(paths))

    # Only if grob listings
    if (sum(!vpListings) > 0) {
        if (gAlign) {
            paths[!vpListings] <- padPrefix(paths[!vpListings], maxLen)
        }
        paths[!vpListings] <- paste0(paths[!vpListings],
				     gvpSep,
				     appendToPrefix(x$gPath[!vpListings],
						    x$name[!vpListings]))
    }
    cat(paths, sep = "\n")
}

grobPathListing <- function(x, ...) {
    subset <- grep("^g", x$type)
    if (length(subset)) {
        cl <- class(x)
        subListing <- lapply(x, "[", subset)
        class(subListing) <- cl
        pathListing(subListing, ...)
    }
}

