
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
#      "gTreeListing", or "vpTreeListing", 
#      (vpStack or vpStack produces a "vpTreeListing").
#
# "vpListListing", and all "gridTreeListing" objects have a "depth" attribute

# The print method will print these in some format, but by having
# a separate object, others can capture the result and format the
# printing themselves.

grid.ls <- function(x=NULL, grobs=TRUE, viewports=FALSE, fullNames=FALSE,
                    recursive=TRUE, print=TRUE, flatten=TRUE, ...) {
    listing <- gridList(x, grobs=grobs, viewports=viewports,
                        fullNames=fullNames, recursive=recursive)
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
        stop(paste("Invalid", sQuote("print"), "argument"))
    }
    invisible(listing)
}

gridList <- function(x, ...) {
    UseMethod("gridList")
}

# The default method lists the grobs on the DL
gridList.default <- function(x, grobs=TRUE, viewports=FALSE,
                             fullNames=FALSE, recursive=TRUE) {
    if (is.null(x)) {
        display.list <- grid.Call("L_getDisplayList")
        dl.index <- grid.Call("L_getDLindex")
        result <- lapply(display.list[1:dl.index], gridList,
                         grobs=grobs, viewports=viewports,
                         fullNames=fullNames, recursive=recursive)
        names(result) <- NULL
        class(result) <- c("gridListListing", "gridListing")
    } else if (is.null(x)) {
        # This handles empty slots in the display list
        result <- character()
        class(result) <- "gridListing"
    } else {
        stop("Invalid object in listing")
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
        if (length(x) == 0) {
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
    if (recursive) {
        # Allow for grobs=FALSE but viewports=TRUE
        result <- gridList(x$children,
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
        if (length(x) == 0) {
            result <- character()
            class(result) <- "gridListing"
        } else if (length(x) == 1) {
            result <- gridList(x[[1]],
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
        if (length(x) == 0) {
            result <- character()
            class(result) <- "gridListing"
        } else if (length(x) == 1 || !recursive) {
            result <- gridList(x[[1]], 
                               grobs=grobs, viewports=viewports,
                               fullNames=fullNames, recursive=recursive)
        } else {
            theRest <- x[-1]
            class(theRest) <- "vpStack"
            result <- gridList(theRest, 
                               grobs=grobs, viewports=viewports,
                               fullNames=fullNames,
                               recursive=recursive)
            result <- list(parent=gridList(x[[1]],
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
        # This would be simpler if paths were kept as vectors
        # but that redesign is a bit of an undertaking
        if (depth(x) == 1) {
            if (fullNames) {
                result <- paste("downViewport[", x$name, "]", sep="")
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
            class(result) <- c("vpTreeListing", "gridTreeListing",
                               "gridListing")
        } else {
            path <- explodePath(x$path)
            result <- gridList(vpPathFromVector(c(path[-1], x$name)), 
                               grobs=grobs, viewports=viewports,
                               fullNames=fullNames,
                               recursive=recursive)
            result <- list(parent=gridList(vpPath(path[1]),
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

# This handles popViewports in the display list
gridList.pop <- function(x, grobs=TRUE, viewports=FALSE,
                         fullNames=FALSE, recursive=TRUE) {
    if (viewports) {
        result <- as.character(x)
        if (fullNames) {
            result <- paste("popViewport[", result, "]", sep="")
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
            result <- paste("upViewport[", result, "]", sep="")
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
        paste(oldpath, .grid.pathSep,
              as.character(addition), sep="")
    } else {
        as.character(addition)
    }
}

decrPath <- function(oldpath, x) {
    bits <- strsplit(oldpath, .grid.pathSep)[[1]]
    n <- as.numeric(gsub("^.+\\[", "",
                         gsub("\\]$", "",
                              as.character(x))))
    if ((length(bits) - n) == 0) {
        ""
    } else {
        paste(bits[1:(length(bits) - n)], collapse=.grid.pathSep)
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
            paste(x$parent, .grid.pathSep,
                  updateVPPath(x$children, ""), sep=""))
}

flatListing <- function(x, gDepth=0, vpDepth=0, gPath="", vpPath="") {
    UseMethod("flatListing")
}

flatListing.gridListing <- function(x, gDepth=0, vpDepth=0,
                                    gPath="", vpPath="") {
    if (length(x) > 0) {
        list(name=as.character(x),
             gDepth=gDepth,
             vpDepth=vpDepth,
             gPath=gPath,
             vpPath=vpPath,
             type=class(x)[1])
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
         type=c(class(x)[1], flatChildren$type))
}

flatListing.vpTreeListing <- function(x, gDepth=0, vpDepth=0,
                                      gPath="", vpPath="") {
    # Increase vpDepth and vpPath
    flatChildren <- flatListing(x$children, gDepth, incDepth(vpDepth, 1),
                                gPath, incPath(vpPath, x$parent))
    list(name=c(as.character(x$parent), flatChildren$name),
         gDepth=c(gDepth, flatChildren$gDepth),
         vpDepth=c(vpDepth, flatChildren$vpDepth),
         gPath=c(gPath, flatChildren$gPath),
         vpPath=c(vpPath, flatChildren$vpPath),
         type=c(class(x)[1], flatChildren$type))
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
    for (i in 1:n) {
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
        stop("Invalid listing")
    cat(paste(makePrefix(gindent, x$gDepth),
              makePrefix(vpindent, x$vpDepth),
              x$name, sep=""),
        sep="\n")
}

pathListing <- function(x, gvpSep=" | ", gAlign=TRUE) {

    appendToPrefix <- function(path, name) {
        emptyPath <- nchar(path) == 0
        ifelse(emptyPath,
               name,
               paste(path, name, sep=.grid.pathSep))
    }
    
    padPrefix <- function(path, maxLen) {
        numSpaces <- maxLen - nchar(path)
        padding <- rep(" ", length(path))
        padding <- mapply(rep, padding, numSpaces)
        paste(path, sapply(padding, paste, collapse=""), sep="")
    }

    if (!inherits(x, "flatGridListing"))
        stop("Invalid listing")
    vpListings <- (1:length(x$name)) %in% grep("^vp", x$type)
    paths <- x$vpPath
    maxLen <- max(nchar(paths))
    # Only if viewport listings
    if (sum(vpListings) > 0) {
        paths[vpListings] <- appendToPrefix(paths[vpListings],
                                            x$name[vpListings])
        # If viewports are shown, then allow extra space before grobs
        maxLen <- max(nchar(paths[vpListings]))
    }
    # Only if grob listings
    if (sum(!vpListings) > 0) {
        if (gAlign) {
            paths[!vpListings] <- padPrefix(paths[!vpListings], maxLen)
        }
        paths[!vpListings] <- paste(paths[!vpListings],
                                    gvpSep,
                                    appendToPrefix(x$gPath[!vpListings],
                                                   x$name[!vpListings]),
                                    sep="")
    }
    cat(paths, sep="\n")
}

grobPathListing <- function(x, ...) {
    subset <- grep("^g", x$type)
    if (length(subset) > 0) {
        cl <- class(x)
        subListing <- lapply(x, "[", subset)
        class(subListing) <- cl
        pathListing(subListing, ...)
    }
}

