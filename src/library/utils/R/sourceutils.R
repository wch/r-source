#  File src/library/utils/R/sourceutils.R
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

removeSource <- function(fn) {
    stopifnot(is.function(fn))
    if (is.primitive(fn)) return(fn)
    attr(fn, "source") <- NULL
    attr(fn, "srcref") <- NULL
    attr(body(fn), "wholeSrcref") <- NULL
    attr(body(fn), "srcfile") <- NULL    
    
    recurse <- function(part) {
        attr(part, "srcref") <- NULL
        if (is.language(part) && is.recursive(part)) {
            for (i in seq_along(part))
            	part[[i]] <- recurse(part[[i]])
        }
        part
    }
    body(fn) <- recurse(body(fn))
    fn
}

getSrcFilename <- function(x, full.names=FALSE, unique=TRUE) {
    srcref <- getSrcref(x)
    if (is.list(srcref)) 
    	result <- sapply(srcref, getSrcFilename, full.names, unique)
    else {
    	srcfile <- attr(srcref, "srcfile")
    	if (is.null(srcfile)) result <- character()
    	else result <- srcfile$filename
    }
    result <- if (full.names) result
              else basename(result)
    if (unique) unique(result)
    else result
}

getSrcDirectory <- function(x, unique=TRUE) {
    result <- dirname(getSrcFilename(x, full.names=TRUE, unique=unique))
    if (unique) unique(result)
    else result
}

getSrcref <- function(x) {
    if (inherits(x, "srcref")) return(x)
    if (!is.null(srcref <- attr(x, "srcref"))) return(srcref)
    if (is.function(x)) return(getSrcref(body(x)))
    NULL
}

getSrcLocation <- function(x, which=c("line", "column", "byte", "parse"), first=TRUE) {
    srcref <- getSrcref(x)
    if (is.null(srcref)) return(NULL)
    if (is.list(srcref)) sapply(srcref, getSrcLocation, which, first)
    else {
        if (length(srcref) == 6L) srcref <- c(srcref, srcref[c(1L,3L)])
    	which <- match.arg(which)
    	if (first) index <- c(line=1L, column=5L, byte=2L, parse=7L)[which]
    	else       index <- c(line=3L, column=6L, byte=4L, parse=8L)[which] 
    	srcref[index]
    }
 }
