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

validGrob <- function(x) {
  x$name <- checkNameSlot(x$name)
  checkgpSlot(x$gp)
  x$vp <- checkvpSlot(x$vp)
  # Validate other grob slots
  x <- validDetails(x)
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

print.grob <- function(x, ...) {
  print(paste(class(x)[1], "[", x$name, "]", sep=""))
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
  return(gl)
}

################
# gTree CLASS DEFN
################
# gTree extends grob
# A gTree has additional children slot
gTree <- function(..., name=NULL, gp=NULL, vp=NULL,
                  children=NULL, childrenvp=NULL,
                  cl=NULL) {
  gt <- list(..., name=name, gp=gp, vp=vp, childrenvp=childrenvp)
  if (!is.null(children) &&
      !inherits(children, "gList"))
    stop("Children must be a gList")
  gt$children <- list()
  gt$childrenOrder <- character()
  if (!is.null(cl) &&
      !is.character(cl))
    stop("Invalid gTree class")
  class(gt) <- c(cl, "gTree", "grob", "gDesc")
  # Make sure basic slots are ok before trying to add children
  gt$name <- checkNameSlot(gt$name)
  checkgpSlot(gt$gp)
  gt$vp <- checkvpSlot(gt$vp)
  checkvpSlot(gt$childrenvp)
  # Add children before calling validGrob!
  if (length(children) > 0)
    for (i in 1:length(children))
      if (!is.null(children[[i]]))
        gt <- addGrob(gt, children[[i]])
  gt <- validGrob(gt)
  return(gt)
}

################
# Getting/adding/removing/editing (children of [children of ...]) a gTree
################

# NOTE:  In order to cut down on repeated code, some of these
# (i.e., all but get and set) are inefficient and call get/set
# to do their work.  If speed becomes an issue, may have to
# revert to individual support for each function with highly
# repetitive code

# Public API for childNames
gTreeChildren <- function(gTree) {
  childNames(gTree)
}

childNames <- function(gTree) {
  if (!inherits(gTree, "gTree"))
    stop("It is only valid to get children from a gTree")
  gTree$childrenOrder
}

# Get a grob from the display list
grid.get <- function(gPath, strict=FALSE, grep=FALSE, global=FALSE,
                     allDevices=FALSE) {
  if (any(grep, global, allDevices))
    stop("grep, global, and allDevices not yet implemented")
  if (is.character(gPath))
    gPath <- gPathDirect(gPath)
  if (!inherits(gPath, "gPath"))
    stop("Invalid path")
  getDLfromGPath(gPath, strict)
}

# Get a child (of a child, of a child, ...) of a grob
getGrob <- function(gTree, gPath, strict=FALSE,
                    grep=FALSE, global=FALSE) {
  if (grep || global)
    stop("grep and global options not yet implemented")
  if (!inherits(gTree, "gTree"))
    stop("It is only valid to get a child from a gTree")
  if (is.character(gPath))
    gPath <- gPathDirect(gPath)
  if (!inherits(gPath, "gPath"))
    stop("Invalid path")
  if (depth(gPath) == 1 && strict) {
    gTree$children[[gPath$name]]
  } else {
    getGTree(gTree, NULL, gPath, strict)
  }
}

# Set a grob on the display list
# It is NOT valid to specify an ambiguous gPath (i.e., no grep arg)
# nor is it valid to specify a global destination (i.e., no global arg)
grid.set <- function(gPath, newGrob, strict=FALSE,
                     redraw=TRUE) {
  if (is.character(gPath))
    gPath <- gPathDirect(gPath)
  if (!inherits(gPath, "gPath"))
    stop("Invalid path")
  result <- setDLfromGPath(gPath, newGrob, strict)
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
# It is NOT valid to specify an ambiguous gPath (i.e., no grep arg)
# nor is it valid to specify a global destination (i.e., no global arg)
setGrob <- function(gTree, gPath, newGrob, strict=FALSE) {
  if (!inherits(gTree, "gTree"))
    stop("It is only valid to set a child of a gTree")
  if (!inherits(newGrob, "grob"))
    stop("It is only valid to set a grob as child of a gTree")
  if (is.character(gPath))
    gPath <- gPathDirect(gPath)
  if (!inherits(gPath, "gPath"))
    stop("Invalid path")
  if (depth(gPath) == 1 && strict) {
    # gPath must specify an existing child
    if (match(gPath$name, gTree$childrenOrder, nomatch=FALSE)) {
      # newGrob name must match gPath
      if (match(gPath$name, newGrob$name, nomatch=FALSE)) {
        gTree$children[[newGrob$name]] <- newGrob
      } else {
        stop(paste("New grob name (", newGrob$name,
                   ") does not match gPath (", gPath, ")", sep=""))
      }
    } else {
      stop("gPath does not specify a valid child")
    }
  } else {
    gTree <- setGTree(gTree, NULL, gPath, newGrob, strict)
    if (is.null(gTree))
      stop("gPath does not specify a valid child")
  }
  gTree
}

# Add a grob to a grob on the display list
grid.add <- function(gPath, child, strict=FALSE,
                     grep=FALSE, global=FALSE, allDevices=FALSE,
                     redraw=TRUE) {
  if (any(grep, global, allDevices))
    stop("grep, global, and allDevices not yet implemented")
  gTree <- grid.get(gPath, strict)
  if (!inherits(gTree, "gTree"))
    stop("It is only valid to add a child to a gTree")
  if (!inherits(child, "grob"))
    stop("It is only valid to add a grob to a gTree")
  gTree$children[[child$name]] <- child
  # Handle case where child name already exists (so will be overwritten)
  if (old.pos <- match(child$name, gTree$childrenOrder, nomatch=0))
    gTree$childrenOrder <- gTree$childrenOrder[-old.pos]
  gTree$childrenOrder <- c(gTree$childrenOrder, child$name)
  grid.set(gPath, gTree, strict, redraw)
}

# Add a grob to a gTree (or a child of a (child of a ...) gTree)
addGrob <- function(gTree, child, gPath=NULL, strict=FALSE,
                    grep=FALSE, global=FALSE) {
  if (grep || global)
    stop("grep and global options not yet implemented")
  if (!is.null(gPath)) {
    origTree <- gTree
    gTree <- getGrob(gTree, gPath, strict)
  }
  if (!inherits(gTree, "gTree"))
    stop("It is only valid to add a child to a gTree")
  if (!inherits(child, "grob"))
    stop("It is only valid to add a grob to a gTree")
  gTree$children[[child$name]] <- child
  # Handle case where child name already exists (so will be overwritten)
  if (old.pos <- match(child$name, gTree$childrenOrder, nomatch=0))
    gTree$childrenOrder <- gTree$childrenOrder[-old.pos]
  gTree$childrenOrder <- c(gTree$childrenOrder, child$name)
  if (!is.null(gPath))
    gTree <- setGrob(origTree, gPath, gTree, strict)
  gTree
}

# Remove a grob (or child of ...) from the display list
grid.remove <- function(gPath, warn=TRUE, strict=FALSE,
                        grep=FALSE, global=FALSE, allDevices=FALSE,
                        redraw=TRUE) {
  if (any(grep, global, allDevices))
    stop("grep, global, and allDevices not yet implemented")
  if (is.character(gPath))
    gPath <- gPathDirect(gPath)
  if (!inherits(gPath, "gPath"))
    stop("Invalid path")
  if (depth(gPath) == 1) {
    removeGrobFromDL(gPath, warn, strict, redraw)
  } else {
    gTree <- grid.get(gPath$path, strict)
    if (!inherits(gTree, "gTree"))
      stop("It is only valid to remove a child from a gTree")
    # Handle case where child name already exists (so will be overwritten)
    if (old.pos <- match(gPath$name, gTree$childrenOrder, nomatch=0)) {
      gTree$childrenOrder <- gTree$childrenOrder[-old.pos]
      gTree$children[[gPath$name]] <- NULL
    } else {
      if (warn)
        stop(paste("gPath (", gPath, ") not found"))
    }
    grid.set(gPath$path, gTree, strict, redraw)
  }
}

# Remove a child from a (child of ...) gTree
removeGrob <- function(gTree, gPath, strict=FALSE,
                       grep=FALSE, global=FALSE, warn=TRUE) {
  if (grep || global)
    stop("grep and global options not yet implemented")
  if (!inherits(gTree, "gTree"))
    stop("It is only valid to remove a child from a gTree")
  if (is.character(gPath))
    gPath <- gPathDirect(gPath)
  if (!inherits(gPath, "gPath"))
    stop("Invalid path")
  if (depth(gPath) == 1) {
    if (strict) {
      if (old.pos <- match(gPath$name, gTree$childrenOrder, nomatch=0)) {
        gTree$childrenOrder <- gTree$childrenOrder[-old.pos]
        gTree$children[[gPath$name]] <- NULL
      } else {
        if (warn)
          stop(paste("gPath (", gPath, ") not found"))
      }
    } else {
      result <- removeGrobfromGPath(gTree, gPath)
      if (is.null(result)) {
        if (warn)
          stop(paste("gPath (", gPath, ") not found"))
      } else {
        gTree <- result
      }
    }
  } else {
    orig.gTree <- gTree
    gTree <- getGrob(gTree, gPath$path, strict)
    if (!inherits(gTree, "gTree"))
      stop("It is only valid to remove a child from a gTree")
    if (old.pos <- match(gPath$name, gTree$childrenOrder, nomatch=0)) {
      gTree$childrenOrder <- gTree$childrenOrder[-old.pos]
      gTree$children[[gPath$name]] <- NULL
      gTree <- setGrob(orig.gTree, gPath$path, gTree, strict)
    } else {
      if (warn)
        stop(paste("gPath (", gPath, ") not found"))
      gTree <- orig.gTree
    }
  }
  gTree
}

# Edit a grob on the display list
grid.edit <- function(gPath, ..., strict=FALSE,
                      grep=FALSE, global=FALSE, allDevices=FALSE,
                      redraw=TRUE) {
  if (any(grep, global, allDevices))
    stop("grep, global, and allDevices not yet implemented")
  if (is.character(gPath))
    gPath <- gPathDirect(gPath)
  if (!inherits(gPath, "gPath"))
    stop("Invalid path")
  specs <- list(...)
  grob <- getDLfromGPath(gPath, strict)
  if (is.null(grob)) {
    stop(paste("gPath (", gPath, ") not found"))
  } else {
    result <- setDLfromGPath(gPath, editThisGrob(grob, specs), strict)
    # Get the current DL index
    dl.index <- grid.Call("L_getDLindex")
    # Destructively modify the DL elt
    grid.Call("L_setDLindex", as.integer(result$index))
    grid.Call("L_setDLelt", result$grob)
    # Reset the DL index
    grid.Call("L_setDLindex", as.integer(dl.index))
    if (redraw)
      draw.all()
  }
}

# Edit a (child of a ...) grob
editGrob <- function(grob, gPath=NULL, ..., strict=FALSE,
                     grep=FALSE, global=FALSE) {
  if (grep || global)
    stop("grep and global options not yet implemented")
  specs <- list(...)
  if (is.null(gPath)) {
    editThisGrob(grob, specs)
  } else {
    if (is.character(gPath))
      gPath <- gPathDirect(gPath)
    # Only makes sense to specify a gPath for a gTree
    if (!inherits(grob, "gTree"))
      stop("It is only valid to edit a child of a gTree")
    setGrob(grob, gPath,
            editThisGrob(getGTree(grob, NULL, gPath, strict), specs))
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
#########

# A gPath may specify the child of a gTree
# (or the child of a child of a gTree, or ...)
getGrobFromGPath <- function(grob, pathsofar, gPath, strict) {
  UseMethod("getGrobFromGPath")
}

# If it's not a grob then fail
# Handles case when traversing DL
getGrobFromGPath.default <- function(grob, pathsofar, gPath, strict) {
  NULL
}

getGrobFromGPath.grob <- function(grob, pathsofar, gPath, strict) {
  if (depth(gPath) > 1)
    NULL
  else {
    if (match(gPath$name, grob$name, nomatch=0))
      grob
    else
      NULL
  }
}

partialPathMatch <- function(pathsofar, path, strict=FALSE) {
  if (strict) {
    length(grep(paste("^", pathsofar, sep=""), gPath$path)) > 0
  } else {
    # If we're not doing strict matching then anything from a full
    # path match to absolutely no match means a partial match
    # (i.e., keep looking)
    TRUE
  }
}

getGTree <- function(gTree, pathsofar, gPath, strict) {
  # Try to find pathsofar at start of gPath
  # NOTE: may be called directly with pathsofar=NULL
  if (is.null(pathsofar) ||
      (!strict && depth(gPath) == 1) ||
      partialPathMatch(pathsofar, gPath$path, strict)) {
    found <- FALSE
    index <- 1
    # Search children for match
    while (index <= length(gTree$childrenOrder) && !found) {
      childName <- gTree$childrenOrder[index]
      child <- gTree$children[[childName]]
      # Special case when strict is FALSE and depth(gPath) is 1
      # Just check for gPath$name amongst children and recurse if no match
      if (!strict && depth(gPath) == 1) {
        if (match(gPath$name, childName, nomatch=0)) {
          grob <- child
          found <- TRUE
        } else {
          if (is.null(pathsofar))
            newpathsofar <- child$name
          else
            newpathsofar <- paste(pathsofar, .grid.pathSep, childName, sep="")
          if (!is.null(newChild <- getGrobFromGPath(child, newpathsofar,
                                                    gPath, strict))) {
            grob <- newChild
            found <- TRUE
          }
        }
      } else {
        # Only check for match with child if have full match with pathsofar
        # If it's a complete match, look for gPath$name amongst child
        # NOTE: may be called directly with pathsofar=NULL
        if (is.null(pathsofar))
          fullmatch <- depth(gPath) == 1
        else
          if (strict)
            fullmatch <- match(pathsofar, gPath$path, nomatch=FALSE)
          else
            fullmatch <-
              length(grep(paste(gPath$path, "$", sep=""), pathsofar)) > 0
        if (fullmatch) {
          if (match(gPath$name, childName, nomatch=0)) {
            grob <- child
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
                                                    gPath, strict))) {
            grob <- newChild
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

getGrobFromGPath.gTree <- function(grob, pathsofar, gPath, strict) {
  if (depth(gPath) == 1) {
    if (match(gPath$name, grob$name, nomatch=FALSE))
      grob
    else
      if (strict)
        NULL
      else
        getGTree(grob,
                 if (is.null(pathsofar)) grob$name else pathsofar,
                 gPath, strict)
  } else {
    getGTree(grob,
             if (is.null(pathsofar)) grob$name else pathsofar,
             gPath, strict)
  }
}

getDLfromGPath <- function(gPath, strict) {
  dl.index <- grid.Call("L_getDLindex")
  result <- NULL
  index <- 1
  while (index < dl.index && is.null(result)) {
    result <- getGrobFromGPath(grid.Call("L_getDLelt",
                                         as.integer(index)),
                               NULL, gPath, strict)
    index <- index + 1
  }
  result
}

# A gPath may specify the child of a gTree
# (or the child of a child of a gTree, or ...)
setGrobFromGPath <- function(grob, pathsofar, gPath, newGrob, strict) {
  UseMethod("setGrobFromGPath")
}

# Ignore DL elements which are not grobs
setGrobFromGPath.default <- function(grob, pathsofar, gPath, newGrob, strict) {
  NULL
}

setGrobFromGPath.grob <- function(grob, pathsofar, gPath, newGrob, strict) {
  if (depth(gPath) > 1)
    NULL
  else {
    if (match(gPath$name, grob$name, nomatch=FALSE))
      if (match(gPath$name, newGrob$name, nomatch=FALSE))
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
setGTree <- function(gTree, pathsofar, gPath, newGrob, strict) {
  # Try to find pathsofar at start of gPath
  # NOTE: may be called directly with pathsofar=NULL
  if (is.null(pathsofar) ||
      (!strict && depth(gPath) == 1) ||
      partialPathMatch(pathsofar, gPath$path, strict)) {
    found <- FALSE
    index <- 1
    # Search children for match
    while (index <= length(gTree$childrenOrder) && !found) {
      childName <- gTree$childrenOrder[index]
      child <- gTree$children[[childName]]
      # Special case when strict is FALSE and depth(gPath) is 1
      # Just check for gPath$name amongst children and recurse if no match
      if (!strict && depth(gPath) == 1) {
        if (match(gPath$name, childName, nomatch=FALSE)) {
          if (match(gPath$name, newGrob$name, nomatch=FALSE)) {
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
                                                    gPath, newGrob, strict))) {
            gTree$children[[childName]] <- newChild
            found <- TRUE
          }
        }
      } else {
        # Only check for match with child if have full match with pathsofar
        # If it's a complete match, look for gPath$name amongst child
        # NOTE: may be called directly with pathsofar=NULL
        if (is.null(pathsofar))
          fullmatch <- depth(gPath) == 1
        else
          if (strict)
            fullmatch <- match(pathsofar, gPath$path, nomatch=FALSE)
          else
            fullmatch <-
              length(grep(paste(gPath$path, "$", sep=""), pathsofar)) > 0
        if (fullmatch) {
          if (match(gPath$name, childName, nomatch=FALSE)) {
            if (match(gPath$name, newGrob$name, nomatch=FALSE)) {
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
                                                    gPath, newGrob, strict))) {
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

setGrobFromGPath.gTree <- function(grob, pathsofar, gPath, newGrob, strict) {
  if (depth(gPath) == 1) {
    if (match(gPath$name, grob$name, nomatch=FALSE))
      if (match(gPath$name, newGrob$name, nomatch=FALSE))
        newGrob
      else
        stop("The new grob must have the same name as the old grob")
    else
      if (strict)
        NULL
      else
        setGTree(grob,
                 if (is.null(pathsofar)) grob$name else pathsofar,
                 gPath, newGrob, strict)
  } else {
    setGTree(grob,
             # Initialise pathsofar if first time through
             if (is.null(pathsofar)) grob$name else pathsofar,
             gPath, newGrob, strict)
  }
}

setDLfromGPath <- function(gPath, newGrob, strict) {
  dl.index <- grid.Call("L_getDLindex")
  index <- 1
  result <- list(index=0, grob=NULL)
  while (index < dl.index &&
         result$index == 0) {
    result$grob <- setGrobFromGPath(grid.Call("L_getDLelt",
                                              as.integer(index)),
                                    NULL, gPath, newGrob, strict)
    if (!is.null(result$grob))
      result$index <- index
    index <- index + 1
  }
  result
}

editThisGrob <- function(grob, specs) {
  for (i in names(specs))
    if (nchar(i) > 0)
      # If there is no slot with the argument name, just ignore that argument
      if (match(i, names(grob), nomatch=0))
        # Handle NULL as special case
        if (is.null(specs[[i]]))
          grob[i] <- eval(substitute(list(i=NULL)))
        else
          grob[[i]] <- specs[[i]]
      else
        warning(paste("Slot", i, "not found"))
  grob <- editDetails(grob, specs)
  validGrob(grob)
}

# Only called with grob is a gTree, gPath of depth 1, and strict = FALSE
removeGrobfromGPath <- function(grob, gPath) {
  UseMethod("removeGrobfromGPath")
}

removeGrobfromGPath.grob <- function(grob, gPath) {
  NULL
}

removeGrobfromGPath.gTree <- function(grob, gPath) {
  gTree <- NULL
  if (old.pos <- match(gPath$name, grob$childrenOrder, nomatch=0)) {
    grob$childrenOrder <- grob$childrenOrder[-old.pos]
    grob$children[[gPath$name]] <- NULL
    gTree <- grob
  } else {
    found <- FALSE
    index <- 1
    while (index <= length(grob$childrenOrder) && !found) {
      childName <- grob$childrenOrder[index]
      child <- grob$children[[childName]]
      gTree <- removeGrobfromGPath(child, gPath)
      if (!is.null(gTree))
        found <- TRUE
      index <- index + 1
    }
  }
  gTree
}

# Only called with gPath of depth 1
removeGrobFromDL <- function(gPath, warn, strict, redraw) {
  # Look first for top-level match (force strict=TRUE)
  dl.index <- grid.Call("L_getDLindex")
  result <- NULL
  index <- 1
  while (index < dl.index && is.null(result)) {
    result <- getGrobFromGPath(grid.Call("L_getDLelt",
                                         as.integer(index)),
                               NULL, gPath, strict=TRUE)
    index <- index + 1
  }
  # If we have succeeded then we NULL the DL elt
  if (!is.null(result)) {
    grobIndex <- index - 1
    # Get the current DL index
    dl.index <- grid.Call("L_getDLindex")
    # Destructively modify the DL elt
    grid.Call("L_setDLindex", as.integer(grobIndex))
    grid.Call("L_setDLelt", NULL)
    # Reset the DL index
    grid.Call("L_setDLindex", as.integer(dl.index))
    if (redraw)
      draw.all()
  } else {
    # If strict=TRUE then we have failed
    if (strict) {
      if (warn)
        stop(paste("gPath (", gPath, ") not found on display list", sep=""))
      else
        return()
    # If strict=FALSE we can try to look further down the DL
    } else {
      dl.index <- grid.Call("L_getDLindex")
      result <- NULL
      index <- 1
      while (index < dl.index && is.null(result)) {
        result <- getGrobFromGPath(grid.Call("L_getDLelt",
                                             as.integer(index)),
                                   NULL, gPath, strict=FALSE)
        if (!is.null(result))
          result <- removeGrob(grid.Call("L_getDLelt",
                                         as.integer(index)),
                               gPath)
        index <- index + 1
      }
      # If we found nothing, we have failed
      if (is.null(result))
        if (warn)
          stop(paste("gPath (", gPath, ") not found on display list", sep=""))
        else
          return()
      else {
        grobIndex <- index - 1
        # Get the current DL index
        dl.index <- grid.Call("L_getDLindex")
        # Destructively modify the DL elt
        grid.Call("L_setDLindex", as.integer(grobIndex))
        grid.Call("L_setDLelt", result)
        # Reset the DL index
        grid.Call("L_setDLindex", as.integer(dl.index))
        if (redraw)
          draw.all()
      }
    }
  }
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
  # automatically push/pop the viewport and set/unset the gpar
  pushvpgp(x)
  # Push then "up" childrenvp
  if (!is.null(x$childrenvp)) {
    pushViewport(x$childrenvp, recording=FALSE)
    upViewport(depth(x$childrenvp), recording=FALSE)
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

# FIXME:  should be on.exit(set.gpar(tempgpar)) ??
grid.draw.grob <- function(x, recording=TRUE) {
  tempgpar <- get.gpar()
  preDraw(x)
  # Do any class-specific drawing
  temp <- drawDetails(x, recording)
  if (is.grob(temp))
    x <- temp
  postDraw(x)
  set.gpar(tempgpar)
  if (recording)
    record(x)
  invisible()
}

drawChildren <- function(x) {
  lapply(x$children, grid.draw, recording=FALSE)
}

# FIXME:  should be on.exit(set.gpar(tempgpar)) ??
grid.draw.gTree <- function(x, recording=TRUE) {
  tempgrob <- grid.Call("L_getCurrentGrob")
  tempgpar <- get.gpar()
  preDraw(x)
  # Do any class-specific drawing
  drawDetails(x, recording)
  # Draw all children
  drawChildren(x)
  postDraw(x)
  set.gpar(tempgpar)
  # Do this as a .Call.graphics to get it onto the base display list
  grid.Call.graphics("L_setCurrentGrob", tempgrob)
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

