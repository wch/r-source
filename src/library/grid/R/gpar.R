#  File src/library/grid/R/gpar.R
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


# A "gpar" object is a list of graphics parameters
# A graphics parameter is a name-value pair

gpar <- function(...) {
  gp <- validGP(list(...))
  class(gp) <- "gpar"
  gp
}

is.gpar <- function(x) {
  inherits(x, "gpar")
}

print.gpar <- function(x, ...) {
  print(unclass(x), ...)
  invisible(x)
}

validGP <- function(gpars) {
  # Check a (non-NULL) gpar is not of length 0
  check.length <- function(gparname) {
    if (length(gpars[[gparname]]) == 0)
      stop(gettextf("'gpar' element '%s' must not be length 0", gparname),
           domain = NA)
  }
  # Check a gpar is numeric and not NULL
  numnotnull <- function(gparname) {
    if (!is.na(match(gparname, names(gpars)))) {
      if (is.null(gpars[[gparname]]))
        gpars[[gparname]] <<- NULL
      else {
        check.length(gparname)
        gpars[[gparname]] <<- as.numeric(gpars[[gparname]])
      }
    }
  }
  # fontsize, lineheight, cex, lwd should be numeric and not NULL
  numnotnull("fontsize")
  numnotnull("lineheight")
  numnotnull("cex")
  numnotnull("lwd")
  numnotnull("lex")
  # gamma defunct in 2.7.0
  if ("gamma" %in% names(gpars)) {
    warning("'gamma' 'gpar' element is defunct")
    gpars$gamma <- NULL
  }
  numnotnull("alpha")
  # col and fill are converted in C code
  # BUT still want to check length > 0
  if (!is.na(match("col", names(gpars)))) {
      if (is.null(gpars$col))
          gpars$col <- NULL
      else
          check.length("col")
  }
  if (!is.na(match("fill", names(gpars)))) {
      if (is.null(gpars$fill))
          gpars$fill <- NULL
      else
          check.length("fill")
  }
  # lty converted in C code
  # BUT still want to check for NULL and check length > 0
  if (!is.na(match("lty", names(gpars)))) {
    if (is.null(gpars$lty))
      gpars$lty <- NULL
    else
      check.length("lty")
  }
  if (!is.na(match("lineend", names(gpars)))) {
    if (is.null(gpars$lineend))
      gpars$lineend <- NULL
    else
      check.length("lineend")
  }
  if (!is.na(match("linejoin", names(gpars)))) {
    if (is.null(gpars$linejoin))
      gpars$linejoin <- NULL
    else
      check.length("linejoin")
  }
  # linemitre should be larger than 1
  numnotnull("linemitre")
  if (!is.na(match("linemitre", names(gpars)))) {
    if (any(gpars$linemitre < 1))
      stop("invalid 'linemitre' value")
  }
  # alpha should be 0 to 1
  if (!is.na(match("alpha", names(gpars)))) {
    if (any(gpars$alpha < 0 || gpars$alpha > 1))
      stop("invalid 'alpha' value")
  }
  # font should be integer and not NULL
  if (!is.na(match("font", names(gpars)))) {
    if (is.null(gpars$font))
      gpars$font <- NULL
    else {
      check.length("font")
      gpars$font <- as.integer(gpars$font)
    }
  }
  # fontfamily should be character
  if (!is.na(match("fontfamily", names(gpars)))) {
    if (is.null(gpars$fontfamily))
      gpars$fontfamily <- NULL
    else {
      check.length("fontfamily")
      gpars$fontfamily <- as.character(gpars$fontfamily)
    }
  }
  # fontface can be character or integer;  map character to integer
  # store value in font
  # Illegal to specify both font and fontface
  if (!is.na(match("fontface", names(gpars)))) {
    if (!is.na(match("font", names(gpars))))
      stop("must specify only one of 'font' and 'fontface'")
    gpars$font <-
	if (is.null(gpars$fontface)) NULL # remove it
	else {
	    check.length("fontface")
	    if (is.numeric(gpars$fontface))
		as.integer(gpars$fontface)
	    else
		vapply(as.character(gpars$fontface),
		       function(ch) # returns integer
		       switch(ch,
			      plain = 1L,
			      bold  = 2L,
			      italic=, oblique = 3L,
			      bold.italic = 4L,
			      symbol= 5L,
					# These are Hershey variants
			      cyrillic=5L,
			      cyrillic.oblique=6L,
			      EUC   = 7L,
			      stop("invalid fontface ", ch)), 0L)
	}
  }
  gpars
}

# Method for subsetting "gpar" objects
`[.gpar` <- function(x, index, ...) {
    if (length(x) == 0)
        return(gpar())
    maxn <- do.call("max", lapply(x, length))
    newgp <- lapply(x, rep, length.out=maxn)
    newgp <- lapply(X = newgp, FUN = "[", index, ...)
    class(newgp) <- "gpar"
    newgp
}

# possible gpar names
# The order must match the GP_* values in grid.h
.grid.gpar.names <- c("fill", "col", "gamma", "lty", "lwd", "cex",
                      "fontsize", "lineheight", "font", "fontfamily",
                      "alpha", "lineend", "linejoin", "linemitre",
                      "lex",
                      # Keep fontface at the end because it is never
                      # used in C code (it gets mapped to font)
                      "fontface")

set.gpar <- function(gp) {
  if (!is.gpar(gp))
    stop("argument must be a 'gpar' object")
  temp <- grid.Call(L_getGPar)
  # gamma defunct in 2.7.0
  if ("gamma" %in% names(gp)) {
      warning("'gamma' 'gpar' element is defunct")
      gp$gamma <- NULL
  }
  # Special case "cex" (make it cumulative)
  if (match("cex", names(gp), nomatch=0L))
    tempcex <- temp$cex * gp$cex
  else
    tempcex <- temp$cex
  # Special case "alpha" (make it cumulative)
  if (match("alpha", names(gp), nomatch=0L))
    tempalpha <- temp$alpha * gp$alpha
  else
    tempalpha <- temp$alpha
  # Special case "lex" (make it cumulative)
  if (match("lex", names(gp), nomatch=0L))
    templex <- temp$lex * gp$lex
  else
    templex <- temp$lex
  # All other gpars
  temp[names(gp)] <- gp
  temp$cex <- tempcex
  temp$alpha <- tempalpha
  temp$lex <- templex
  # Do this as a .Call.graphics to get it onto the base display list
  grid.Call.graphics(L_setGPar, temp)
}

get.gpar <- function(names=NULL) {
  if (is.null(names)) {
    result <- grid.Call(L_getGPar)
    # drop gamma
    result$gamma <- NULL
  } else {
    if (!is.character(names) ||
        !all(names %in% .grid.gpar.names))
      stop("must specify only valid 'gpar' names")
    # gamma deprecated
    if ("gamma" %in% names) {
      warning("'gamma' 'gpar' element is defunct")
      names <- names[-match("gamma", names)]
    }
    result <- unclass(grid.Call(L_getGPar))[names]
  }
  class(result) <- "gpar"
  result
}

# When editing a gp slot, only update the specified gpars
# Assume gp is NULL or a gpar
# assume newgp is a gpar (and not NULL)
mod.gpar <- function(gp, newgp) {
  if (is.null(gp))
    gp <- newgp
  else
    gp[names(newgp)] <- newgp
  gp
}

