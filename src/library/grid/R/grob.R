######################################
# Grid graphical objects
#######################################

################
# CLASS DEFN
################
# A "virtual" class "gDesc" underlies both "grob" and "gPath"

initGrobAutoName <- function() {
  index <- 0
  function() {
    index <<- index + 1
    paste("GRID.GROB.", index, sep="")
  }
}

grobAutoName <- initGrobAutoName()

################
# CLASS DEFN
################
# A grob has a name, a gp, and a vp
# grob inherits from gDesc
checkvpSlot <- function(vp) {
  # vp can be a viewport, a viewport name, or a viewport path
  if (!is.null(vp))
    if (!inherits(vp, "viewport") &&
        !inherits(vp, "vpPath") &&
        !is.character(vp))
      stop("Invalid vp slot")
  # For interactive use, allow user to specify
  # vpPath directly (i.e., w/o calling vpPath)
  if (is.character(vp))
    vp <- vpPathDirect(vp)
  vp
}

checkNameSlot <- function(name) {
  # Supply a default name if one is not given
  if (is.null(name))
    grobAutoName()
  else
    as.character(name)
}

checkgpSlot <- function(gp) {
  # gp must be a gpar
  if (!is.null(gp))
    if (!inherits(gp, "gpar"))
      stop("Invalid gp slot")
}

validDetails <- function(x) {
  UseMethod("validDetails")
}

validDetails.grob <- function(x) {
  x
}

validGrob <- function(x, ...) {
  UseMethod("validGrob")
}

validGrob.grob <- function(x, ...) {
  # Validate class-specific slots
  x <- validDetails(x)
  # Validate standard grob slots
  x$name <- checkNameSlot(x$name)
  checkgpSlot(x$gp)
  if (!is.null(x$vp))
    x$vp <- checkvpSlot(x$vp)
  return(x)
}

# This actually creates a new class derived from grob
# and returns an instance of that new class, all in one step
grob <- function(..., name=NULL, gp=NULL, vp=NULL, cl=NULL) {
  g <- list(..., name=name, gp=gp, vp=vp)
  if (!is.null(cl) &&
      !is.character(cl))
    stop("Invalid grob class")
  class(g) <- c(cl, "grob", "gDesc")
  validGrob(g)
}

grid.grob <- function(list.struct, cl=NULL, draw=TRUE) {
  warning("grid.grob() is deprecated; please use grob() instead")
  g <- do.call("grob", c(list.struct, cl=cl))
  if (draw)
    grid.draw(g)
  invisible(g)
}

is.grob <- function(x) {
  inherits(x, "grob")
}

as.character.grob <- function(x, ...) {
  paste(class(x)[1], "[", x$name, "]", sep="")
}

print.grob <- function(x, ...) {
  cat(as.character(x), "\n")
}

################
# gPath CLASS DEFN
################
# gPath is a concatenated list of names specifying a path to a grob
# Functions for creating "paths" of viewport names

gPathFromVector <- function(names) {
  n <- length(names)
  if (n < 1)
    stop("A grob path must contain at least one grob name")
  if (!all(is.character(names)))
    stop("Invalid grob name(s)")
  path <- list(path=if (n==1) NULL else
               paste(names[1:(n-1)], collapse=.grid.pathSep),
               name=names[n],
               n=n)
  class(path) <- c("gPath", "path")
  path
}

gPath <- function(...) {
  names <- c(...)
  gPathFromVector(names)
}

# Create gPath from string with embedded .grid.pathSep(s)
gPathDirect <- function(path) {
  names <- unlist(strsplit(path, .grid.pathSep))
  gPathFromVector(names)
}

################
# gList CLASS DEFN
################
# Just a list of grobs
okGListelt <- function(x) {
  is.grob(x) || is.null(x)
}

gList <- function(...) {
  gl <- list(...)
  if (length(gl) == 0 ||
      all(sapply(gl, okGListelt, simplify=TRUE))) {
    class(gl) <- c("gList")
    return(gl)
  } else {
    stop("Only grobs allowed in gList")
  }
}

addToGList <- function(x, gList) {
  UseMethod("addToGList")
}

addToGList.grob <- function(x, gList) {
  if (is.null(gList))
    gList(x)
  else {
    gList[[length(gList) + 1]] <- x
    return(gList)
  }
}

addToGList.gList <- function(x, gList) {
  gl <- c(gList, x)
  class(gl) <- "gList"
  return(gl)
}

as.character.gList <- function(x, ...) {
  paste("(", paste(lapply(x, as.character), collapse=", "), ")", sep="")
}

print.gList <- function(x, ...) {
  cat(as.character(x), "\n")
}

################
# gTree CLASS DEFN
################
# gTree extends grob
# A gTree has additional children slot
childName <- function(x) {
  x$name
}

setChildren <- function(x, children) {
  if (!inherits(x, "gTree"))
    stop("Can only set children for a gTree")
  if (!is.null(children) &&
      !inherits(children, "gList"))
    stop("Children must be a gList")
  # Thin out NULL children
  if (!is.null(children)) {
    cl <- class(children)
    children <- children[!sapply(children, is.null)]
    class(children) <- cl
  }
  if (length(children) > 0) {
    x$children <- children
    childNames <- sapply(children, childName)
    names(x$children) <- childNames
    x$childrenOrder <- childNames
  } else {
    x$children <- gList()
    x$childrenOrder <- character()
  }
  x
}

childNames <- function(gTree) {
  if (!inherits(gTree, "gTree"))
    stop("It is only valid to get children from a gTree")
  gTree$childrenOrder
}

validGrob.gTree <- function(x, childrenvp, ...) {
  # Validate class-specific slots
  x <- validDetails(x)
  # Validate standard grob slots
  x$name <- checkNameSlot(x$name)
  checkgpSlot(x$gp)
  if (!is.null(x$vp))
    x$vp <- checkvpSlot(x$vp)
  # Only add childrenvp here so that gTree slots can
  # be validated before childrenvp get made
  # (making of childrenvp and children likely to depend
  #  on gTree slots)
  if (!is.null(childrenvp))
    x$childrenvp <- checkvpSlot(childrenvp)
  return(x)
}

gTree <- function(..., name=NULL, gp=NULL, vp=NULL,
                  children=NULL, childrenvp=NULL,
                  cl=NULL) {
  gt <- list(..., name=name, gp=gp, vp=vp)
  if (!is.null(cl) &&
      !is.character(cl))
    stop("Invalid gTree class")
  class(gt) <- c(cl, "gTree", "grob", "gDesc")
  gt <- validGrob(gt, childrenvp)
  gt <- setChildren(gt, children)
  return(gt)
}

################
# Getting just the names of the top-level grobs on the DL
################
getName <- function(elt) {
  if (inherits(elt, "grob"))
    elt$name
  else
    ""
}

getNames <- function() {
  dl <- grid.Call("L_getDisplayList")[1:grid.Call("L_getDLindex")]
  names <- sapply(dl, getName)
  names[nchar(names) != 0]
}

################
# Getting/adding/removing/editing (children of [children of ...]) a gTree
################

# NOTE:  In order to cut down on repeated code, some of these
# (i.e., all but get and set) are inefficient and call get/set
# to do their work.  If speed becomes an issue, may have to
# revert to individual support for each function with highly
# repetitive code

# Get a grob from the display list
grid.get <- function(gPath, strict=FALSE, grep=FALSE, global=FALSE,
                     allDevices=FALSE) {
  if (allDevices)
    stop("allDevices not yet implemented")
  if (is.character(gPath))
    gPath <- gPathDirect(gPath)
  if (!inherits(gPath, "gPath"))
    stop("Invalid path")
  if (!is.logical(grep))
    stop("Invalid grep value")
  grep <- rep(grep, length=depth(gPath))
  getDLfromGPath(gPath, strict, grep, global)
}

# Get a child (of a child, of a child, ...) of a grob
getGrob <- function(gTree, gPath, strict=FALSE,
                    grep=FALSE, global=FALSE) {
  if (!inherits(gTree, "gTree"))
    stop("It is only valid to get a child from a gTree")
  if (is.character(gPath))
    gPath <- gPathDirect(gPath)
  if (!inherits(gPath, "gPath"))
    stop("Invalid path")
  if (depth(gPath) == 1 && strict) {
    gTree$children[[gPath$name]]
  } else {
    if (!is.logical(grep))
      stop("Invalid grep value")
    grep <- rep(grep, length=depth(gPath))
    getGTree(gTree, NULL, gPath, strict, grep, global)
  }
}

# Set a grob on the display list
# nor is it valid to specify a global destination (i.e., no global arg)
grid.set <- function(gPath, newGrob, strict=FALSE, grep=FALSE,
                     redraw=TRUE) {
  if (is.character(gPath))
    gPath <- gPathDirect(gPath)
  if (!inherits(gPath, "gPath"))
    stop("Invalid path")
  if (!is.logical(grep))
    stop("Invalid grep value")
  grep <- rep(grep, length=depth(gPath))
  result <- setDLfromGPath(gPath, newGrob, strict, grep)
  # result$index will be non-zero if matched the gPath
  if (result$index) {
    # Get the current DL index
    dl.index <- grid.Call("L_getDLindex")
    # Destructively modify the DL elt
    grid.Call("L_setDLindex", as.integer(result$index))
    grid.Call("L_setDLelt", result$grob)
    # Reset the DL index
    grid.Call("L_setDLindex", as.integer(dl.index))
    if (redraw)
      draw.all()
  } else {
    stop("gPath does not specify a valid child")
  }
}

# Set a grob
# nor is it valid to specify a global destination (i.e., no global arg)
setGrob <- function(gTree, gPath, newGrob, strict=FALSE, grep=FALSE) {
  if (!inherits(gTree, "gTree"))
    stop("It is only valid to set a child of a gTree")
  if (!inherits(newGrob, "grob"))
    stop("It is only valid to set a grob as child of a gTree")
  if (is.character(gPath))
    gPath <- gPathDirect(gPath)
  if (!inherits(gPath, "gPath"))
    stop("Invalid path")
  if (!is.logical(grep))
    stop("Invalid grep value")
  grep <- rep(grep, length=depth(gPath))
  if (depth(gPath) == 1 && strict) {
    # gPath must specify an existing child
    if (old.pos <- nameMatch(gPath$name, gTree$childrenOrder, grep)) {
      # newGrob name must match existing name
      if (match(gTree$childrenOrder[old.pos], newGrob$name, nomatch=FALSE)) {
        gTree$children[[newGrob$name]] <- newGrob
      } else {
        stop(paste("New grob name (", newGrob$name,
                   ") does not match gPath (", gPath, ")", sep=""))
      }
    } else {
      stop("gPath does not specify a valid child")
    }
  } else {
    gTree <- setGTree(gTree, NULL, gPath, newGrob, strict, grep)
    if (is.null(gTree))
      stop("gPath does not specify a valid child")
  }
  gTree
}

# Add a grob to a grob on the display list
grid.add <- function(gPath, child, strict=FALSE,
                     grep=FALSE, global=FALSE, allDevices=FALSE,
                     redraw=TRUE) {
  if (allDevices)
    stop("allDevices not yet implemented")
  if (is.character(gPath))
    gPath <- gPathDirect(gPath)
  if (!inherits(gPath, "gPath"))
    stop("Invalid path")
  if (!is.logical(grep))
    stop("Invalid grep value")
  grep <- rep(grep, length=depth(gPath))
  addDLfromGPath(gPath, child, strict, grep, global, redraw)
}

# Add a grob to a gTree (or a child of a (child of a ...) gTree)
addGrob <- function(gTree, child, gPath=NULL, strict=FALSE,
                    grep=FALSE, global=FALSE) {
  if (!inherits(child, "grob"))
    stop("It is only valid to add a grob to a gTree")
  if (is.null(gPath)) {
    addToGTree(gTree, child)
  } else {
    if (is.character(gPath))
      gPath <- gPathDirect(gPath)
    # Only makes sense to specify a gPath for a gTree
    if (!inherits(gTree, "gTree"))
      stop("It is only valid to add a child to a gTree")
    if (!is.logical(grep))
      stop("Invalid grep value")
    grep <- rep(grep, length=depth(gPath))
    addGTree(gTree, child, NULL, gPath, strict, grep, global)
  }
}

# Remove a grob (or child of ...) from the display list
grid.remove <- function(gPath, warn=TRUE, strict=FALSE,
                        grep=FALSE, global=FALSE, allDevices=FALSE,
                        redraw=TRUE) {
  if (allDevices)
    stop("allDevices not yet implemented")
  if (is.character(gPath))
    gPath <- gPathDirect(gPath)
  if (!inherits(gPath, "gPath"))
    stop("Invalid path")
  if (!is.logical(grep))
    stop("Invalid grep value")
  grep <- rep(grep, length=depth(gPath))
  if (depth(gPath) == 1) {
    removeNameFromDL(gPath$name, strict, grep, global, warn, redraw)
  } else {
    name <- gPath$name
    gPath <- gPathDirect(gPath$path)
    greppath <- grep[-length(grep)]
    grepname <- grep[length(grep)]
    removeDLFromGPath(gPath, name, strict, greppath, grepname,
                      global, warn, redraw)
  }
}

# Remove a child from a (child of ...) gTree
removeGrob <- function(gTree, gPath, strict=FALSE,
                       grep=FALSE, global=FALSE, warn=TRUE) {
  if (!inherits(gTree, "gTree"))
    stop("It is only valid to remove a child from a gTree")
  if (is.character(gPath))
    gPath <- gPathDirect(gPath)
  if (!inherits(gPath, "gPath"))
    stop("Invalid path")
  if (!is.logical(grep))
    stop("Invalid grep value")
  grep <- rep(grep, length=depth(gPath))
  if (depth(gPath) == 1) {
    # result will be NULL if no match
    result <- removeName(gTree, gPath$name, strict, grep, global, warn)
    if (is.null(result))
      gTree
    else
      result
  } else {
    name <- gPath$name
    gPath <- gPathDirect(gPath$path)
    greppath <- grep[-length(grep)]
    grepname <- grep[length(grep)]
    # result will be NULL if no match
    result <- removeGTree(gTree, name, NULL, gPath, strict, greppath, grepname,
                          global, warn)
    if (is.null(result))
      gTree
    else
      result
  }
}

# Edit a grob on the display list
grid.edit <- function(gPath, ..., strict=FALSE,
                      grep=FALSE, global=FALSE, allDevices=FALSE,
                      redraw=TRUE) {
  if (allDevices)
    stop("allDevices not yet implemented")
  if (is.character(gPath))
    gPath <- gPathDirect(gPath)
  if (!inherits(gPath, "gPath"))
    stop("Invalid path")
  if (!is.logical(grep))
    stop("Invalid grep value")
  grep <- rep(grep, length=depth(gPath))
  specs <- list(...)
  editDLfromGPath(gPath, specs, strict, grep, global, redraw)
}

# Edit a (child of a ...) grob
editGrob <- function(grob, gPath=NULL, ..., strict=FALSE,
                     grep=FALSE, global=FALSE) {
  specs <- list(...)
  if (is.null(gPath)) {
    editThisGrob(grob, specs)
  } else {
    if (is.character(gPath))
      gPath <- gPathDirect(gPath)
    # Only makes sense to specify a gPath for a gTree
    if (!inherits(grob, "gTree"))
      stop("It is only valid to edit a child of a gTree")
    if (!is.logical(grep))
      stop("Invalid grep value")
    grep <- rep(grep, length=depth(gPath))
    editGTree(grob, specs, NULL, gPath, strict, grep, global)
  }
}

#########
# Generic "hook" to allow customised action on edit
#########
editDetails <- function(x, specs) {
  UseMethod("editDetails")
}

editDetails.default <- function(x, specs) {
  # Do nothing BUT return object being edited
  x
}

editDetails.gTree <- function(x, specs) {
  # Disallow editing children or childrenOrder slots directly
  if (any(match(specs, c("children", "childrenOrder"), nomatch=FALSE)))
    stop("It is invalid to directly edit the children or childrenOrder slot")
  x
}

#########
# Helper functions for getting/adding/removing/editing grobs
#
# ASSUME down here that the grep argument has been replicated
# up to the length of the gPath argument
#########

# Find a "match" between a path$name and a grob$name
nameMatch <- function(pathName, grobName, grep) {
  if (grep) {
    pos <- grep(pathName, grobName)
    (length(pos) > 0 && pos == 1)
  } else {
    match(pathName, grobName, nomatch=FALSE)
  }
}

# Return the position of path$name in vector of names
# Return FALSE if not found
namePos <- function(pathName, names, grep) {
  if (grep) {
    pos <- grep(pathName, names)
    if (length(pos) == 0)
      pos <- FALSE
  } else {
    pos <- match(pathName, names, nomatch=FALSE)
  }
  pos
}

partialPathMatch <- function(pathsofar, path, strict=FALSE, grep) {
  if (strict) {
    if (!any(grep))
      length(grep(paste("^", pathsofar, sep=""), path)) > 0
    else {
      pathSoFarElts <- explodePath(pathsofar)
      pathElts <- explodePath(path)
      ok <- TRUE
      npsfe <- length(pathSoFarElts)
      index <- 1
      while (ok & index <= npsfe) {
        if (grep[index])
          ok <- (grep(pathSoFarElts[index], pathElts[index]) == 1)
        else
          ok <- match(pathSoFarElts[index], pathElts[index], nomatch=FALSE)
        index <- index + 1
      }
      ok
    }
  } else {
    # If we're not doing strict matching then anything from a full
    # path match to absolutely no match means a partial match
    # (i.e., keep looking)
    TRUE
  }
}

fullPathMatch <- function(pathsofar, gPath, strict, grep) {
  if (is.null(pathsofar))
    match <- (depth(gPath) == 1)
  else {
    path <- gPath$path
    if (!any(grep))
      if (strict)
        match <- match(pathsofar, path, nomatch=FALSE)
      else
        match <- (length(grep(paste(path, "$", sep=""), pathsofar)) > 0)
    else {
      pathSoFarElts <- explodePath(pathsofar)
      pathElts <- explodePath(path)
      npsfe <- length(pathSoFarElts)
      npe <- length(pathElts)
      if (npe > npsfe) {
        match <- FALSE
      } else { 
        match <- TRUE
        index <- 1
        if (strict) {# pathSoFar same length as gPath
        } else {# pathSoFar could be longer than gPath
          pathSoFarElts <- pathSoFarElts[(npsfe - npe + 1):npsfe]
        }
        while (match && index <= npe) {
          if (grep[index])
            match <- (length(grep(pathElts[index], pathSoFarElts[index])) > 0)
          else
            match <- match(pathSoFarElts[index], pathElts[index],
                           nomatch=FALSE)
          index <- index + 1
        }
      }
    }
  }
  match
}

#####
##### Get support
#####

# Add a grob to a result
growResult <- function(result, x) {
  UseMethod("growResult")
}

# Should only be when result is NULL
growResult.default <- function(result, x) {
  if (!is.null(result))
    stop("Invalid result")
  x
}

growResult.grob <- function(result, x) {
  if (is.grob(x))
    gList(result, x)
  else
    # x should be a gList
    addToGList(result, x)
}

growResult.gList <- function(result, x) {
  addToGList(x, result)
}

# A gPath may specify the child of a gTree
# (or the child of a child of a gTree, or ...)
getGrobFromGPath <- function(grob, pathsofar, gPath, strict,
                             grep, global) {
  UseMethod("getGrobFromGPath")
}

# If it's not a grob then fail
# Handles case when traversing DL
getGrobFromGPath.default <- function(grob, pathsofar, gPath, strict,
                                     grep, global) {
  NULL
}

getGrobFromGPath.grob <- function(grob, pathsofar, gPath, strict,
                                  grep, global) {
  if (depth(gPath) > 1)
    NULL
  else {
    if (nameMatch(gPath$name, grob$name, grep))
      grob
    else
      NULL
  }
}

getGTree <- function(gTree, pathsofar, gPath, strict, grep, global) {
  # Try to find pathsofar at start of gPath
  # NOTE: may be called directly with pathsofar=NULL
  if (is.null(pathsofar) ||
      (!strict && depth(gPath) == 1) ||
      partialPathMatch(pathsofar, gPath$path, strict, grep)) {
    found <- FALSE
    index <- 1
    grob <- NULL
    # Search children for match
    while (index <= length(gTree$childrenOrder) &&
           (!found || global)) {
      childName <- gTree$childrenOrder[index]
      child <- gTree$children[[childName]]
      # Special case when strict is FALSE and depth(gPath) is 1
      # Just check for gPath$name amongst children and recurse if no match
      if (!strict && depth(gPath) == 1) {
        if (nameMatch(gPath$name, childName, grep)) {
          grob <- growResult(grob, child)
          found <- TRUE
        } else {
          if (is.null(pathsofar))
            newpathsofar <- child$name
          else
            newpathsofar <- paste(pathsofar, .grid.pathSep, childName, sep="")
          if (!is.null(newChild <- getGrobFromGPath(child, newpathsofar,
                                                    gPath, strict,
                                                    grep, global))) {
            grob <- growResult(grob, newChild)
            found <- TRUE
          }
        }
      } else {
        # Only check for match with child if have full match with pathsofar
        # If it's a complete match, look for gPath$name amongst child
        # NOTE: may be called directly with pathsofar=NULL
        if (fullPathMatch(pathsofar, gPath, strict, grep)) {
          if (nameMatch(gPath$name, childName, grep[depth(gPath)])) {
            grob <- growResult(grob, child)
            found <- TRUE
          }
        # Otherwise recurse down child
        } else {
          # NOTE: may be called directly with pathsofar=NULL
          if (is.null(pathsofar))
            newpathsofar <- child$name
          else
            newpathsofar <- paste(pathsofar, .grid.pathSep, childName, sep="")
          if (!is.null(newChild <- getGrobFromGPath(child, newpathsofar,
                                                    gPath, strict,
                                                    grep, global))) {
            grob <- growResult(grob, newChild)
            found <- TRUE
          }
        }
      }
      index <- index + 1
    }
    if (found)
      grob
    else
      NULL
  } else {
    NULL
  }
}

getGrobFromGPath.gTree <- function(grob, pathsofar, gPath, strict,
                                   grep, global) {
  if (depth(gPath) == 1) {
    if (nameMatch(gPath$name, grob$name, grep))
      grob
    else
      if (strict)
        NULL
      else
        getGTree(grob,
                 if (is.null(pathsofar)) grob$name else pathsofar,
                 gPath, strict, grep, global)
  } else {
    getGTree(grob,
             if (is.null(pathsofar)) grob$name else pathsofar,
             gPath, strict, grep, global)
  }
}

getDLfromGPath <- function(gPath, strict, grep, global) {
  dl.index <- grid.Call("L_getDLindex")
  result <- NULL
  index <- 1
  while (index < dl.index &&
         (is.null(result) || global)) {
    grob <- getGrobFromGPath(grid.Call("L_getDLelt",
                                       as.integer(index)),
                             NULL, gPath, strict,
                             grep, global)
    if (!is.null(grob))
      result <- growResult(result, grob)
    index <- index + 1
  }
  result
}

#####
##### Set support
#####
# A gPath may specify the child of a gTree
# (or the child of a child of a gTree, or ...)
setGrobFromGPath <- function(grob, pathsofar, gPath, newGrob, strict, grep) {
  UseMethod("setGrobFromGPath")
}

# Ignore DL elements which are not grobs
setGrobFromGPath.default <- function(grob, pathsofar, gPath, newGrob,
                                     strict, grep) {
  NULL
}

setGrobFromGPath.grob <- function(grob, pathsofar, gPath, newGrob,
                                  strict, grep) {
  if (depth(gPath) > 1)
    NULL
  else {
    if (nameMatch(gPath$name, grob$name, grep))
      if (match(grob$name, newGrob$name, nomatch=FALSE))
        newGrob
      else
        NULL
    else
      NULL
  }
}

# Try to match gPath in gTree children
# Return NULL if cant' find match
# Return modified gTree if can find match
setGTree <- function(gTree, pathsofar, gPath, newGrob, strict, grep) {
  # Try to find pathsofar at start of gPath
  # NOTE: may be called directly with pathsofar=NULL
  if (is.null(pathsofar) ||
      (!strict && depth(gPath) == 1) ||
      partialPathMatch(pathsofar, gPath$path, strict, grep)) {
    found <- FALSE
    index <- 1
    # Search children for match
    while (index <= length(gTree$childrenOrder) && !found) {
      childName <- gTree$childrenOrder[index]
      child <- gTree$children[[childName]]
      # Special case when strict is FALSE and depth(gPath) is 1
      # Just check for gPath$name amongst children and recurse if no match
      if (!strict && depth(gPath) == 1) {
        if (nameMatch(gPath$name, childName, grep)) {
          if (match(childName, newGrob$name, nomatch=FALSE)) {
            gTree$children[[newGrob$name]] <- newGrob
            found <- TRUE
          } else {
            stop("The new grob must have the same name as the old grob")
          }
        } else {
          if (is.null(pathsofar))
            newpathsofar <- child$name
          else
            newpathsofar <- paste(pathsofar, .grid.pathSep, childName, sep="")
          if (!is.null(newChild <- setGrobFromGPath(child, newpathsofar,
                                                    gPath, newGrob,
                                                    strict, grep))) {
            gTree$children[[childName]] <- newChild
            found <- TRUE
          }
        }
      } else {
        # Only check for match with child if have full match with pathsofar
        # If it's a complete match, look for gPath$name amongst child
        # NOTE: may be called directly with pathsofar=NULL
        if (fullPathMatch(pathsofar, gPath, strict, grep)) {
          if (nameMatch(gPath$name, childName, grep[depth(gPath)])) {
            if (match(childName, newGrob$name, nomatch=FALSE)) {
              gTree$children[[newGrob$name]] <- newGrob
              found <- TRUE
            }
          }
        # Otherwise recurse down child
        } else {
          # NOTE: may be called directly with pathsofar=NULL
          if (is.null(pathsofar))
            newpathsofar <- child$name
          else
            newpathsofar <- paste(pathsofar, .grid.pathSep, childName, sep="")
          if (!is.null(newChild <- setGrobFromGPath(child, newpathsofar,
                                                    gPath, newGrob,
                                                    strict, grep))) {
            gTree$children[[childName]] <- newChild
            found <- TRUE
          }
        }
      }
      index <- index + 1
    }
    if (found)
      gTree
    else
      NULL
  } else {
    NULL
  }
}

setGrobFromGPath.gTree <- function(grob, pathsofar, gPath, newGrob,
                                   strict, grep) {
  if (depth(gPath) == 1) {
    if (nameMatch(gPath$name, grob$name, grep))
      if (match(grob$name, newGrob$name, nomatch=FALSE))
        newGrob
      else
        stop("The new grob must have the same name as the old grob")
    else
      if (strict)
        NULL
      else
        setGTree(grob,
                 if (is.null(pathsofar)) grob$name else pathsofar,
                 gPath, newGrob, strict, grep)
  } else {
    setGTree(grob,
             # Initialise pathsofar if first time through
             if (is.null(pathsofar)) grob$name else pathsofar,
             gPath, newGrob, strict, grep)
  }
}

setDLfromGPath <- function(gPath, newGrob, strict, grep) {
  dl.index <- grid.Call("L_getDLindex")
  index <- 1
  result <- list(index=0, grob=NULL)
  while (index < dl.index &&
         result$index == 0) {
    result$grob <- setGrobFromGPath(grid.Call("L_getDLelt",
                                              as.integer(index)),
                                    NULL, gPath, newGrob, strict, grep)
    if (!is.null(result$grob))
      result$index <- index
    index <- index + 1
  }
  result
}

#####
##### Edit support
#####
editThisGrob <- function(grob, specs) {
  for (i in names(specs))
    if (nchar(i) > 0)
      # Handle gp as special case
      if (match(i, "gp", nomatch=0))
        # Handle NULL as special case
        if (is.null(specs[[i]]))
          grob[i] <- list(gp=NULL)
        else
          grob$gp <- mod.gpar(grob$gp, specs$gp)
      # If there is no slot with the argument name, just ignore that argument
      else if (match(i, names(grob), nomatch=0)) 
        # Handle NULL as special case
        if (is.null(specs[[i]]))
          grob[i] <- eval(substitute(list(i=NULL)))
        else
          grob[[i]] <- specs[[i]]
      else
        warning(paste("Slot", i, "not found"))
  # Check grob slots are ok before trying to do anything with them
  # in editDetails
  # grob$childrenvp may be non-NULL for a gTree
  grob <- validGrob(grob, grob$childrenvp)
  editDetails(grob, specs)
}

# A gPath may specify the child of a gTree
# (or the child of a child of a gTree, or ...)
editGrobFromGPath <- function(grob, specs, pathsofar, gPath, strict,
                              grep, global) {
  UseMethod("editGrobFromGPath")
}

# If it's not a grob then fail
# Handles case when traversing DL
editGrobFromGPath.default <- function(grob, specs,
                                      pathsofar, gPath, strict,
                                      grep, global) {
  NULL
}

editGrobFromGPath.grob <- function(grob, specs,
                                   pathsofar, gPath, strict,
                                   grep, global) {
  if (depth(gPath) > 1)
    NULL
  else {
    if (nameMatch(gPath$name, grob$name, grep))
      editThisGrob(grob, specs)
    else
      NULL
  }
}

editGTree <- function(gTree, specs, pathsofar, gPath, strict,
                      grep, global) {
  # Try to find pathsofar at start of gPath
  # NOTE: may be called directly with pathsofar=NULL
  if (is.null(pathsofar) ||
      (!strict && depth(gPath) == 1) ||
      partialPathMatch(pathsofar, gPath$path, strict, grep)) {
    found <- FALSE
    index <- 1
    # Search children for match
    while (index <= length(gTree$childrenOrder) &&
           (!found || global)) {
      childName <- gTree$childrenOrder[index]
      child <- gTree$children[[childName]]
      # Special case when strict is FALSE and depth(gPath) is 1
      # Just check for gPath$name amongst children and recurse if no match
      if (!strict && depth(gPath) == 1) {
        if (nameMatch(gPath$name, childName, grep)) {
          gTree$children[[childName]] <- editThisGrob(child, specs)
          found <- TRUE
        } else {
          if (is.null(pathsofar))
            newpathsofar <- child$name
          else
            newpathsofar <- paste(pathsofar, .grid.pathSep, childName, sep="")
          if (!is.null(newChild <- editGrobFromGPath(child, specs,
                                                     newpathsofar,
                                                     gPath, strict,
                                                     grep, global))) {
            gTree$children[[childName]] <- newChild
            found <- TRUE
          }
        }
      } else {
        # Only check for match with child if have full match with pathsofar
        # If it's a complete match, look for gPath$name amongst child
        # NOTE: may be called directly with pathsofar=NULL
        if (fullPathMatch(pathsofar, gPath, strict, grep)) {
          if (nameMatch(gPath$name, childName, grep[depth(gPath)])) {
            gTree$children[[childName]] <- editThisGrob(child, specs)
            found <- TRUE
          }
        # Otherwise recurse down child
        } else {
          # NOTE: may be called directly with pathsofar=NULL
          if (is.null(pathsofar))
            newpathsofar <- child$name
          else
            newpathsofar <- paste(pathsofar, .grid.pathSep, childName, sep="")
          if (!is.null(newChild <- editGrobFromGPath(child, specs,
                                                     newpathsofar,
                                                     gPath, strict,
                                                     grep, global))) {
            gTree$children[[childName]] <- newChild
            found <- TRUE
          }
        }
      }
      index <- index + 1
    }
    if (found)
      gTree
    else
      NULL
  } else {
    NULL
  }
}

editGrobFromGPath.gTree <- function(grob, specs,
                                    pathsofar, gPath, strict,
                                    grep, global) {
  if (depth(gPath) == 1) {
    if (nameMatch(gPath$name, grob$name, grep))
      editThisGrob(grob, specs)
    else
      if (strict)
        NULL
      else
        editGTree(grob, specs,
                  if (is.null(pathsofar)) grob$name else pathsofar,
                  gPath, strict, grep, global)
  } else {
    editGTree(grob, specs,
              if (is.null(pathsofar)) grob$name else pathsofar,
              gPath, strict, grep, global)
  }
}

editDLfromGPath <- function(gPath, specs, strict, grep, global, redraw) {
  dl.index <- grid.Call("L_getDLindex")
  index <- 1
  grob <- NULL
  found <- FALSE
  while (index < dl.index &&
         (is.null(grob) || global)) {
    grob <- editGrobFromGPath(grid.Call("L_getDLelt",
                                        as.integer(index)),
                              specs,
                              NULL, gPath, strict, grep, global)
    if (!is.null(grob)) {
      # Destructively modify the DL elt
      grid.Call("L_setDLindex", as.integer(index))
      grid.Call("L_setDLelt", grob)
      # Reset the DL index
      grid.Call("L_setDLindex", as.integer(dl.index))
      found <- TRUE
    }
    index <- index + 1
  }
  if (!found) 
    stop(paste("gPath (", gPath, ") not found"))
  else if (redraw)
    draw.all()
}
  
#####
##### Add support
#####

# Assume that child is a grob
addToGTree <- function(gTree, child) {
  if (!inherits(gTree, "gTree"))
    stop("It is only valid to add a child to a gTree")
  gTree$children[[child$name]] <- child
  # Handle case where child name already exists (so will be overwritten)
  if (old.pos <- match(child$name, gTree$childrenOrder, nomatch=0))
    gTree$childrenOrder <- gTree$childrenOrder[-old.pos]
  gTree$childrenOrder <- c(gTree$childrenOrder, child$name)
  gTree
}

# A gPath may specify the child of a gTree
# (or the child of a child of a gTree, or ...)
addGrobFromGPath <- function(grob, child, pathsofar, gPath, strict,
                             grep, global) {
  UseMethod("addGrobFromGPath")
}

# If it's not a grob then fail
# Handles case when traversing DL
addGrobFromGPath.default <- function(grob, child,
                                     pathsofar, gPath, strict,
                                     grep, global) {
  NULL
}

# If no match then fail
# If match then error!
addGrobFromGPath.grob <- function(grob, child,
                                  pathsofar, gPath, strict,
                                  grep, global) {
  if (depth(gPath) > 1)
    NULL
  else {
    if (nameMatch(gPath$name, grob$name, grep))
      stop("It is only valid to add a child to a gTree")
    else
      NULL
  }
}

# In this function, the grob being added is called "grob"
# (in all others it is called "child"
addGTree <- function(gTree, grob, pathsofar, gPath, strict,
                     grep, global) {
  # Try to find pathsofar at start of gPath
  # NOTE: may be called directly with pathsofar=NULL
  if (is.null(pathsofar) ||
      (!strict && depth(gPath) == 1) ||
      partialPathMatch(pathsofar, gPath$path, strict, grep)) {
    found <- FALSE
    index <- 1
    # Search children for match
    while (index <= length(gTree$childrenOrder) &&
           (!found || global)) {
      childName <- gTree$childrenOrder[index]
      child <- gTree$children[[childName]]
      # Special case when strict is FALSE and depth(gPath) is 1
      # Just check for gPath$name amongst children and recurse if no match
      if (!strict && depth(gPath) == 1) {
        if (nameMatch(gPath$name, childName, grep)) {
          gTree$children[[childName]] <- addToGTree(child, grob)
          found <- TRUE
        } else {
          if (is.null(pathsofar))
            newpathsofar <- child$name
          else
            newpathsofar <- paste(pathsofar, .grid.pathSep, childName, sep="")
          if (!is.null(newChild <- addGrobFromGPath(child, grob,
                                                    newpathsofar,
                                                    gPath, strict,
                                                    grep, global))) {
            gTree$children[[childName]] <- newChild
            found <- TRUE
          }
        }
      } else {
        # Only check for match with child if have full match with pathsofar
        # If it's a complete match, look for gPath$name amongst child
        # NOTE: may be called directly with pathsofar=NULL
        if (fullPathMatch(pathsofar, gPath, strict, grep)) {
          if (nameMatch(gPath$name, childName, grep[depth(gPath)])) {
            gTree$children[[childName]] <- addToGTree(child, grob)
            found <- TRUE
          }
        # Otherwise recurse down child
        } else {
          # NOTE: may be called directly with pathsofar=NULL
          if (is.null(pathsofar))
            newpathsofar <- child$name
          else
            newpathsofar <- paste(pathsofar, .grid.pathSep, childName, sep="")
          if (!is.null(newChild <- addGrobFromGPath(child, grob,
                                                    newpathsofar,
                                                    gPath, strict,
                                                    grep, global))) {
            gTree$children[[childName]] <- newChild
            found <- TRUE
          }
        }
      }
      index <- index + 1
    }
    if (found)
      gTree
    else
      NULL
  } else {
    NULL
  }
}

addGrobFromGPath.gTree <- function(grob, child,
                                   pathsofar, gPath, strict,
                                   grep, global) {
  if (depth(gPath) == 1) {
    if (nameMatch(gPath$name, grob$name, grep))
      addToGTree(grob, child)
    else
      if (strict)
        NULL
      else
        addGTree(grob, child,
                 if (is.null(pathsofar)) grob$name else pathsofar,
                 gPath, strict, grep, global)
  } else {
    addGTree(grob, child,
             if (is.null(pathsofar)) grob$name else pathsofar,
             gPath, strict, grep, global)
  }
}

addDLfromGPath <- function(gPath, child, strict, grep, global, redraw) {
  dl.index <- grid.Call("L_getDLindex")
  index <- 1
  grob <- NULL
  found <- FALSE
  while (index < dl.index &&
         (is.null(grob) || global)) {
    grob <- addGrobFromGPath(grid.Call("L_getDLelt",
                                       as.integer(index)),
                             child,
                             NULL, gPath, strict, grep, global)
    if (!is.null(grob)) {
      # Destructively modify the DL elt
      grid.Call("L_setDLindex", as.integer(index))
      grid.Call("L_setDLelt", grob)
      # Reset the DL index
      grid.Call("L_setDLindex", as.integer(dl.index))
      found <- TRUE
    }
    index <- index + 1
  }
  if (!found) 
    stop(paste("gPath (", gPath, ") not found"))
  else if (redraw)
    draw.all()
}
  
#####
##### Remove support
#####

removeFromGTree <- function(gTree, name, grep) {
  if (!inherits(gTree, "gTree"))
    stop("It is only valid to remove a child from a gTree")
  if (grep) {
    old.pos <- grep(name, gTree$childrenOrder)
    if (length(old.pos) == 0)
      old.pos <- 0
  } else {
    old.pos <- match(name, gTree$childrenOrder, nomatch=0)
  }
  if (old.pos > 0) {
    # name might be a regexp so use real name
    gTree$children[[gTree$childrenOrder[old.pos]]] <- NULL
    gTree$childrenOrder <- gTree$childrenOrder[-old.pos]
    gTree
  } else {
    NULL
  }
}

# A gPath may specify the child of a gTree
# (or the child of a child of a gTree, or ...)
removeGrobFromGPath <- function(grob, name, pathsofar, gPath, strict,
                                grep, grepname, global, warn) {
  UseMethod("removeGrobFromGPath")
}

# If it's not a grob then fail
# Handles case when traversing DL
removeGrobFromGPath.default <- function(grob, name,
                                        pathsofar, gPath, strict,
                                        grep, grepname, global, warn) {
  NULL
}

# ALWAYS fail
# (either no match or match but grob has no children!)
removeGrobFromGPath.grob <- function(grob, name,
                                     pathsofar, gPath, strict,
                                     grep, grepname, global, warn) {
  NULL
}

removeGTree <- function(gTree, name, pathsofar, gPath, strict,
                        grep, grepname, global, warn) {
  # Try to find pathsofar at start of gPath
  # NOTE: may be called directly with pathsofar=NULL
  if (is.null(pathsofar) ||
      (!strict && depth(gPath) == 1) ||
      partialPathMatch(pathsofar, gPath$path, strict, grep)) {
    found <- FALSE
    index <- 1
    # Search children for match
    while (index <= length(gTree$childrenOrder) &&
           (!found || global)) {
      childName <- gTree$childrenOrder[index]
      child <- gTree$children[[childName]]
      # Special case when strict is FALSE and depth(gPath) is 1
      # Just check for gPath$name amongst children and recurse if no match
      if (!strict && depth(gPath) == 1) {
        # NOTE: child has to be a gTree if we hope to find a child in it!
        if (inherits(child, "gTree") &&
            nameMatch(gPath$name, childName, grep)) {
          newchild <- removeFromGTree(child, name, grepname)
          if (!is.null(newchild)) {
            gTree$children[[childName]] <- newchild
            found <- TRUE
          }
        } else {
          if (is.null(pathsofar))
            newpathsofar <- child$name
          else
            newpathsofar <- paste(pathsofar, .grid.pathSep, childName, sep="")
          if (!is.null(newChild <- removeGrobFromGPath(child, name,
                                                       newpathsofar,
                                                       gPath, strict,
                                                       grep, global, warn))) {
            gTree$children[[childName]] <- newChild
            found <- TRUE
          }
        }
      } else {
        # Only check for match with child if have full match with pathsofar
        # If it's a complete match, look for gPath$name amongst child
        # NOTE: may be called directly with pathsofar=NULL
        if (fullPathMatch(pathsofar, gPath, strict, grep)) {
          # NOTE: child has to be a gTree if we hope to find a child in it!
          if (inherits(child, "gTree") &&
              nameMatch(gPath$name, childName, grep[depth(gPath)])) {
            newchild <- removeFromGTree(child, name, grepname)
            if (!is.null(newchild)) {
              gTree$children[[childName]] <- newchild
              found <- TRUE
            }
          }
        # Otherwise recurse down child
        } else {
          # NOTE: may be called directly with pathsofar=NULL
          if (is.null(pathsofar))
            newpathsofar <- child$name
          else
            newpathsofar <- paste(pathsofar, .grid.pathSep, childName, sep="")
          if (!is.null(newChild <- removeGrobFromGPath(child, name,
                                                       newpathsofar,
                                                       gPath, strict,
                                                       grep, global, warn))) {
            gTree$children[[childName]] <- newChild
            found <- TRUE
          }
        }
      }
      index <- index + 1
    }
    if (found)
      gTree
    else
      NULL
  } else {
    NULL
  }
}

removeGrobFromGPath.gTree <- function(grob, name,
                                      pathsofar, gPath, strict,
                                      grep, grepname, global, warn) {
  if (depth(gPath) == 1) {
    if (nameMatch(gPath$name, grob$name, grep))
      removeFromGTree(grob, name, grepname)
    else
      if (strict)
        NULL
      else
        removeGTree(grob, name,
                    if (is.null(pathsofar)) grob$name else pathsofar,
                    gPath, strict, grep, grepname, global, warn)
  } else {
    removeGTree(grob, name,
                if (is.null(pathsofar)) grob$name else pathsofar,
                gPath, strict, grep, grepname, global, warn)
  }
}

removeDLFromGPath <- function(gPath, name, strict, grep, grepname, global,
                              warn, redraw) {
  dl.index <- grid.Call("L_getDLindex")
  index <- 1
  grob <- NULL
  found <- FALSE
  while (index < dl.index &&
         (is.null(grob) || global)) {
    grob <- removeGrobFromGPath(grid.Call("L_getDLelt", as.integer(index)),
                                name,
                                NULL, gPath, strict, grep, grepname,
                                global, warn)
    if (!is.null(grob)) {
      # Destructively modify the DL elt
      grid.Call("L_setDLindex", as.integer(index))
      grid.Call("L_setDLelt", grob)
      # Reset the DL index
      grid.Call("L_setDLindex", as.integer(dl.index))
      found <- TRUE
    }
    index <- index + 1
  }
  if (!found) 
    stop(paste("gPath (", gPath, ") not found"))
  else if (redraw)
    draw.all()
}
  
#####
##### Remove NAME support
#####

# NEVER called when strict=TRUE
removeGrobFromName <- function(grob, name, grep, global, warn) {
  UseMethod("removeGrobFromName")
}

removeGrobFromName.grob <- function(grob, name, grep, global, warn) {
  NULL
}

removeName <- function(gTree, name, strict, grep, global, warn) {
  found <- FALSE
  index <- 1
  # Search children for match
  while (index <= length(gTree$childrenOrder) &&
         (!found || global)) {
    childName <- gTree$childrenOrder[index]
    child <- gTree$children[[childName]]
    # Just check for name amongst children and recurse if no match
    if (nameMatch(name, childName, grep)) {
      # name might be a regexp, so get real name
      gTree$children[[gTree$childrenOrder[index]]] <- NULL
      gTree$childrenOrder <- gTree$childrenOrder[-index]
      found <- TRUE
    } else if (strict) {
      NULL
    } else {
      if (!is.null(newChild <- removeGrobFromName(child, name, 
                                                  grep, global, warn))) {
        gTree$children[[childName]] <- newChild
        found <- TRUE
      }
    }
    index <- index + 1
  }
  if (found)
    gTree
  else
    NULL
}

removeGrobFromName.gTree <- function(grob, name, grep, global, warn) {
  if (old.pos <- namePos(name, grob$childrenOrder, grep)) {
    # name might be a regexp, so get real name
    grob$children[[grob$childrenOrder[old.pos]]] <- NULL
    grob$childrenOrder <- grob$childrenOrder[-old.pos]
    grob
  } else {
    removeName(grob, name, FALSE, grep, global, warn)
  } 
}

removeNameFromDL <- function(name, strict, grep, global, warn, redraw) {
  dl.index <- grid.Call("L_getDLindex")
  index <- 1
  grob <- NULL
  found <- FALSE
  while (index < dl.index &&
         (is.null(grob) || global)) {
    grob <- grid.Call("L_getDLelt", as.integer(index))
    if (inherits(grob, "grob")) {
      # If match top-level grob, remove it from DL
      if (nameMatch(name, grob$name, grep)) {
        # Destructively modify the DL elt
        grid.Call("L_setDLindex", as.integer(index))
        grid.Call("L_setDLelt", NULL)
        # Reset the DL index
        grid.Call("L_setDLindex", as.integer(dl.index))
        found <- TRUE
      # Otherwise search down it for match
      } else {
        if (!strict) {
          grob <- removeGrobFromName(grob, name, grep, global, warn)
          if (!is.null(grob)) {
            # Destructively modify the DL elt
            grid.Call("L_setDLindex", as.integer(index))
            grid.Call("L_setDLelt", grob)
            # Reset the DL index
            grid.Call("L_setDLindex", as.integer(dl.index))
            found <- TRUE
          }
        }
      }
    } else {
      grob <- NULL
    }
    index <- index + 1
  }
  if (!found) {
    if (warn)
      stop(paste("gPath (", name, ") not found"))
  } else if (redraw)
    draw.all()
}

################
# Finding a grob from a grob name
################
findgrob <- function(x, name) {
  UseMethod("findgrob")
}

findgrob.default <- function(x, name) {
  NULL
}

findgrob.grob <- function(x, name) {
  if (match(name, x$name, nomatch=FALSE))
    x
  else
    NULL
}

findGrobinDL <- function(name) {
  dl.index <- grid.Call("L_getDLindex")
  result <- NULL
  index <- 1
  while (index < dl.index && is.null(result)) {
    result <- findgrob(grid.Call("L_getDLelt", as.integer(index)), name)
    index <- index + 1
  }
  if (is.null(result))
    stop("Grob ", name, " not found")
  result
}

findGrobinChildren <- function(name, children) {
  nc <- length(children)
  result <- NULL
  index <- 1
  while (index <= nc && is.null(result)) {
    result <- findgrob(children[[index]], name)
    index <- index + 1
  }
  if (is.null(result))
    stop("Grob ", name, " not found")
  result
}

################
# grid.draw
################
# Use generic function "draw" rather than generic function "print"
# because want graphics functions to produce graphics output
# without having to be evaluated at the command-line AND without having
# to necessarily produce a single graphical object as the return value
# (i.e., so that simple procedural code can be written just for its
# side-effects).
# For example, so that the following code will draw
# a rectangle AND a line:
#   temp <- function() { grid.lines(); grid.rect() }
#   temp()
grid.draw <- function(x, recording=TRUE) {
  UseMethod("grid.draw")
}

grid.draw.default <- function(x, recording) {
  # Allow for "holes" in the DL if a grob has been removed
  if (!is.null(x))
    stop("Invalid element in the display list")
}

grid.draw.viewport <- function(x, recording) {
  pushViewport(x, recording=FALSE)
}

grid.draw.vpPath <- function(x, recording) {
  # Assumes strict=FALSE, BUT in order to get onto
  # display list it must have worked => strict same as non-strict
  downViewport(x, recording=FALSE)
}

grid.draw.pop <- function(x, recording) {
  popViewport(x, recording=FALSE)
}

grid.draw.up <- function(x, recording) {
  upViewport(x, recording=FALSE)
}

pushgrobvp <- function(vp) {
  UseMethod("pushgrobvp")
}

pushgrobvp.viewport <- function(vp) {
  pushViewport(vp, recording=FALSE)
}

pushgrobvp.vpPath <- function(vp) {
  downViewport(vp, strict=TRUE, recording=FALSE)
}

popgrobvp <- function(vp) {
  UseMethod("popgrobvp")
}

popgrobvp.viewport <- function(vp) {
  # NOTE that the grob's vp may be a vpStack/List/Tree
  popViewport(depth(vp), recording=FALSE)
}

popgrobvp.vpPath <- function(vp) {
  upViewport(depth(vp), recording=FALSE)
}

preDraw <- function(x) {
  UseMethod("preDraw")
}

pushvpgp <- function(x) {
  if (!is.null(x$vp))
    pushgrobvp(x$vp)
  if (!is.null(x$gp)) {
    set.gpar(x$gp)
  }
}

preDraw.grob <- function(x) {
  # automatically push/pop the viewport and set/unset the gpar
  pushvpgp(x)
  preDrawDetails(x)
}

preDraw.gTree <- function(x) {
  # Make this gTree the "current grob" for evaluation of
  # grobwidth/height units via gPath
  # Do this as a .Call.graphics to get it onto the base display list
  grid.Call.graphics("L_setCurrentGrob", x)
  # automatically push/pop the viewport 
  pushvpgp(x)
  # Push then "up" childrenvp
  if (!is.null(x$childrenvp)) {
    # Save any x$gp gpar settings
    tempgp <- grid.Call("L_getGPar")
    pushViewport(x$childrenvp, recording=FALSE)
    upViewport(depth(x$childrenvp), recording=FALSE)
    # reset the x$gp gpar settings
    # The upViewport above may have overwritten them with
    # the previous vp$gp settings
    grid.Call.graphics("L_setGPar", tempgp)
  }
  preDrawDetails(x)
}

postDraw <- function(x) {
  UseMethod("postDraw")
}

postDraw.grob <- function(x) {
  postDrawDetails(x)
  if (!is.null(x$vp))
    popgrobvp(x$vp)
}

drawGrob <- function(x) {
  # Temporarily turn off the grid DL so that
  # nested calls to drawing code do not get recorded
  dlon <- grid.Call("L_setDLon", FALSE)
  # If get error or user-interrupt, need to reset state
  # Need to turn grid DL back on (if it was on)
  on.exit(grid.Call("L_setDLon", dlon))
  # Save current gpar
  tempgpar <- grid.Call("L_getGPar")
  # If get error or user-interrupt, need to reset state
  # Need to restore current grob (gtree predraw sets current grob)
  # Need to restore gpar settings (set by gtree itself and/or its vp)
  # This does not need to be a grid.Call.graphics() because
  # we are nested within a recordGraphics()
  # Do not call set.gpar because set.gpar accumulates cex
  on.exit(grid.Call("L_setGPar", tempgpar), add=TRUE)
  preDraw(x)
  # Do any class-specific drawing
  drawDetails(x, recording=FALSE)
  postDraw(x)
}

grid.draw.grob <- function(x, recording=TRUE) {
  engineDLon <- grid.Call("L_getEngineDLon")
  if (engineDLon)   
    recordGraphics(drawGrob(x),
                   list(x=x),
                   getNamespace("grid"))
  else
    drawGrob(x)
  if (recording)
    record(x)
  invisible()
}

drawGList <- function(x) {
  # Temporarily turn off the grid DL so that
  # nested calls to drawing code do not get recorded
  dlon <- grid.Call("L_setDLon", FALSE)
  # If get error or user-interrupt, need to reset state
  # Need to turn grid DL back on (if it was on)
  on.exit(grid.Call("L_setDLon", dlon))
  lapply(x, grid.draw, recording=FALSE)
}

grid.draw.gList <- function(x, recording=TRUE) {
  engineDLon <- grid.Call("L_getEngineDLon")
  if (engineDLon) 
    recordGraphics(drawGList(x),
                   list(x=x),
                   getNamespace("grid"))
  else
    drawGList(x)
  invisible()
}

drawGTree <- function(x) {
  # Temporarily turn off the grid DL so that
  # nested calls to drawing code do not get recorded
  dlon <- grid.Call("L_setDLon", FALSE)
  # If get error or user-interrupt, need to reset state
  # Need to turn grid DL back on (if it was on)
  on.exit(grid.Call("L_setDLon", dlon))
  # Save current grob and current gpar
  tempgrob <- grid.Call("L_getCurrentGrob")
  tempgpar <- grid.Call("L_getGPar")
  # If get error or user-interrupt, need to reset state
  # Need to restore current grob (gtree predraw sets current grob)
  # Need to restore gpar settings (set by gtree itself and/or its vp)
  # This does not need to be a grid.Call.graphics() because
  # we are nested within a recordGraphics()
  # Do not call set.gpar because set.gpar accumulates cex
  on.exit({ grid.Call("L_setGPar", tempgpar)
            grid.Call("L_setCurrentGrob", tempgrob)
          }, add=TRUE)
  preDraw(x)
  # Do any class-specific drawing
  drawDetails(x, recording=FALSE)
  # Draw all children
  grid.draw(x$children, recording=FALSE)
  postDraw(x)
}

grid.draw.gTree <- function(x, recording=TRUE) {
  engineDLon <- grid.Call("L_getEngineDLon")
  if (engineDLon) 
    recordGraphics(drawGTree(x),
                   list(x=x),
                   getNamespace("grid"))
  else 
    drawGTree(x)
  if (recording)
    record(x)
  invisible()
}

draw.all <- function() {
  grid.newpage(recording=FALSE)
  dl.index <- grid.Call("L_getDLindex")
  if (dl.index > 1)
    # Start at 2 because first element is viewport[ROOT]
    for (i in 2:dl.index) {
      grid.draw(grid.Call("L_getDLelt", as.integer(i - 1)),
                recording=FALSE)
    }
}

draw.details <- function(x, recording) {
  .Deprecated("drawDetails")
  UseMethod("drawDetails")
}

preDrawDetails <- function(x) {
  UseMethod("preDrawDetails")
}

preDrawDetails.grob <- function(x) {
}

postDrawDetails <- function(x) {
  UseMethod("postDrawDetails")
}

postDrawDetails.grob <- function(x) {
}

drawDetails <- function(x, recording) {
  UseMethod("drawDetails")
}

drawDetails.grob <- function(x, recording) {
}

grid.copy <- function(grob) {
  warning("This function is redundant and will disappear in future versions.")
  grob
}

