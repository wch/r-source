#  File src/library/grid/R/unit.R
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


# Create an object of class "unit"
# Simple units are of the form 'unit(1, "cm")' or 'unit(1L:3, "cm")' or
# 'unit(c(1,3,6), c("cm", "inch", "npc"))'
# More complicated units are of the form 'unit(1, "string", "a string")'
# or 'unit(1, "grob", a.grob)'
unit <- function(x, units, data=NULL) {
    # Used to throw error if !is.numeric(x), but this way
    # user can specify unit(NA, "npc") rather than
    # having to specify unit(as.numeric(NA), "npc")
    x <- as.numeric(x)
    units <- as.character(units)
    if (length(x) == 0 || length(units) == 0)
        stop("'x' and 'units' must have length > 0")
    valid.unit(x, units, recycle.data(data, FALSE, length(x), units))
}

valid.unit <- function(x, units, data) {
  valid.units <- valid.units(units)
  data <- valid.data(rep(units, length.out=length(x)), data)
  attr(x, "unit") <- units
  attr(x, "valid.unit") <- valid.units
  attr(x, "data") <- data
  class(x) <- "unit"
  x
}

grid.convert <- function(x, unitTo, axisFrom="x", typeFrom="location",
                         axisTo=axisFrom, typeTo=typeFrom,
                         valueOnly=FALSE) {
  .Deprecated("convertUnit")
  convertUnit(x, unitTo, axisFrom, typeFrom, axisTo, typeTo, valueOnly)
}

convertUnit <- function(x, unitTo, axisFrom="x", typeFrom="location",
                        axisTo=axisFrom, typeTo=typeFrom,
                        valueOnly=FALSE) {
  whatfrom <- match(axisFrom, c("x", "y")) - 1L +
    2L*(match(typeFrom, c("location", "dimension")) - 1L)
  whatto <- match(axisTo, c("x", "y")) - 1L +
    2L*(match(typeTo, c("location", "dimension")) - 1L)
  if (!is.unit(x))
    stop("'x' argument must be a unit object")
  if (is.na(whatfrom) || is.na(whatto))
    stop("invalid 'axis' or 'type'")
  value <- grid.Call(L_convert, x, as.integer(whatfrom),
                 as.integer(whatto), valid.units(unitTo))
  if (!valueOnly)
    unit(value, unitTo)
  else
    value
}

grid.convertX <- function(x, unitTo, valueOnly=FALSE) {
  .Deprecated("convertX")
  convertX(x, unitTo, valueOnly)
}

convertX <- function(x, unitTo, valueOnly=FALSE) {
  convertUnit(x, unitTo, "x", "location", "x", "location",
              valueOnly=valueOnly)
}

grid.convertY <- function(x, unitTo, valueOnly=FALSE) {
  .Deprecated("convertY")
  convertY(x, unitTo, valueOnly)
}

convertY <- function(x, unitTo, valueOnly=FALSE) {
  convertUnit(x, unitTo, "y", "location", "y", "location",
              valueOnly=valueOnly)
}

grid.convertWidth <- function(x, unitTo, valueOnly=FALSE) {
  .Deprecated("convertWidth")
  convertWidth(x, unitTo, valueOnly)
}

convertWidth <- function(x, unitTo, valueOnly=FALSE) {
  convertUnit(x, unitTo, "x", "dimension", "x", "dimension",
              valueOnly=valueOnly)
}

grid.convertHeight <- function(x, unitTo, valueOnly=FALSE) {
  .Deprecated("convertHeight")
  convertHeight(x, unitTo, valueOnly)
}

convertHeight <- function(x, unitTo, valueOnly=FALSE) {
  convertUnit(x, unitTo, "y", "dimension", "y", "dimension",
              valueOnly=valueOnly)
}

convertNative <- function(unit, dimension="x", type="location") {
  .Deprecated("convertUnit")
  convertUnit(unit, "native", dimension, type, dimension, type,
              valueOnly=TRUE)
}

# This is like the "convert" functions:  it evaluates units (immediately)
# in the current context
calcStringMetric <- function(text) {
    # .Call rather than .Call.graphics because it is a one-off calculation
    metric <- grid.Call(L_stringMetric, text)
    names(metric) <- c("ascent", "descent", "width")
    metric
}

# NOTE: the order of the strings in these conversion functions must
# match the order of the enums in ../src/grid.h
# AND in ../src/unit.c (see UnitTable)
# NOTE: ../src/unit.c also allows some pseudonyms (e.g., "in" for "inches")
.grid.unit.list <- c("npc", "cm", "inches", "lines",
                     "native", "null", "snpc", "mm",
                     "points", "picas", "bigpts",
                     "dida", "cicero", "scaledpts",
                     "strwidth", "strheight",
                     "strascent", "strdescent",
                     "vplayoutwidth", "vplayoutheight", "char",
                     "grobx", "groby", "grobwidth", "grobheight",
                     "grobascent", "grobdescent",
                     "mylines", "mychar", "mystrwidth", "mystrheight")

stringUnit <- function(unit) {
    unit %in% c("strwidth", "strheight", "strascent", "strdescent")
}

grobUnit <- function(unit) {
    unit %in% c("grobx", "groby", "grobwidth", "grobheight",
                "grobascent", "grobdescent")
}

dataUnit <- function(unit) {
    stringUnit(unit) | grobUnit(unit)
}

recycle.data <- function(data, data.per, max.n, units) {
    # FIRST STEP:  check that data needs to be recycled
    if (any(dataUnit(units))) {
        # VERY IMPORTANT:  Even if there is only one data specified
        # and/or only one data needed, we want this to be a LIST of
        # data values so that a single data and several data can be
        # handled equivalently
        # The test for whether it is only a single value currently
        # consists of a check for mode="character" (i.e., a single
        # string) or mode="expression" (i.e., a single expression)
        # or class="grob" (i.e., a single grob) or class="gPath"
        if (is.character(data) || is.language(data) ||
            is.grob(data) || inherits(data, "gPath"))
            data <- list(data)
        if (data.per)
            n <- max.n
        else
            n <- length(data)
        original <- data
        length(data) <- n
        if (length(original) < length(data)) {
            for (i in (length(original) + 1):length(data)) {
                data[[i]] <- original[[(i - 1) %% length(original) + 1]]
            }
        }
    }
    data
}

# Make sure that and "str*" and "grob*" units have data
valid.data <- function(units, data) {
    n <- length(units)
    str.units <- stringUnit(units)
    if (any(str.units))
        for (i in (1L:n)[str.units])
            if (!(length(data) >= i &&
                  (is.character(data[[i]]) || is.language(data[[i]]))))
                stop("no string supplied for 'strwidth/height' unit")
    # Make sure that a grob has been specified
    grob.units <- grobUnit(units)
    if (any(grob.units))
        for (i in (1L:n)[grob.units]) {
            if (!(length(data) >= i &&
                  (is.grob(data[[i]]) || inherits(data[[i]], "gPath") ||
                   is.character(data[[i]]))))
                stop("no 'grob' supplied for 'grobwidth/height' unit")
            if (is.character(data[[i]]))
                data[[i]] <- gPathDirect(data[[i]])
            if (inherits(data[[i]], "gPath"))
                if (depth(data[[i]]) > 1)
                    stop("'gPath' must have depth 1 in 'grobwidth/height' units")
        }
    # Make sure that where no data is required, the data is NULL
    if (!all(sapply(data[!(str.units | grob.units)], is.null)))
        stop("non-NULL value supplied for plain unit")
    data
}

valid.units <- function(units) {
  .Call(validUnits, units)
}

as.character.unit <- function(x, ...) {
  class(x) <- NULL
  paste0(x, attr(x, "unit"))
}

#########################
# UNIT ARITHMETIC STUFF
#########################

unit.arithmetic <- function(func.name, arg1, arg2=NULL) {
  ua <- list(fname=func.name, arg1=arg1, arg2=arg2)
  class(ua) <- c("unit.arithmetic", "unit")
  ua
}

Ops.unit <- function(e1, e2) {
  ok <- switch(.Generic, "+"=TRUE, "-"=TRUE, "*"=TRUE, FALSE)
  if (!ok)
    stop(gettextf("operator '%s' not meaningful for units", .Generic),
         domain = NA)
  if (.Generic == "*")
    # can only multiply a unit by a scalar
    if (nzchar(.Method[1L])) {
      if (nzchar(.Method[2L]))
        stop("only one operand may be a unit")
      else if (is.numeric(e2))
        # NOTE that we always put the scalar first
        # Use as.numeric to force e2 to be REAL
        unit.arithmetic(.Generic, as.numeric(e2), e1)
      else
        stop("non-unit operand must be numeric")
    } else {
      if (is.numeric(e1))
        # Use as.numeric to force e1 to be REAL
        unit.arithmetic(.Generic, as.numeric(e1), e2)
      else
        stop("non-unit operand must be numeric")
    }
  else
    # Check that both arguments are units
    if (nzchar(.Method[1L]) && nzchar(.Method[2L]))
      unit.arithmetic(.Generic, e1, e2)
    else
      stop("both operands must be units")
}

## <FIXME>
## The na.rm arg is ignored here, and the S3 groupGeneric is
## Summary(x, ...)
## </FIXME>
Summary.unit <- function(..., na.rm=FALSE) {
  # NOTE that this call to unit.c makes sure that arg1 is
  # a single unit object
  x <- unit.c(...)
  ok <- switch(.Generic, "max"=TRUE, "min"=TRUE, "sum"=TRUE, FALSE)
  if (!ok)
    stop(gettextf("'Summary' function '%s' not meaningful for units",
                  .Generic), domain = NA)
  unit.arithmetic(.Generic, x)
}

is.unit.arithmetic <- function(x) {
  inherits(x, "unit.arithmetic")
}

as.character.unit.arithmetic <- function(x, ...) {
  # bit too customised for my liking, but whatever ...
  # NOTE that paste coerces arguments to mode character hence
  # this will recurse.
  fname <- x$fname
  if (fname == "+" || fname == "-" || fname == "*")
    paste0(x$arg1, fname, x$arg2)
  else
    paste0(fname, "(", paste(x$arg1, collapse=", "), ")")
}

unit.pmax <- function(...) {

  select.i <- function(unit, i) {
    `[`(unit, i, top=FALSE)
  }

  x <- list(...)
  numargs <- length(x)
  if (numargs == 0L)
    stop("no arguments where at least one expected")
  # how long will the result be?
  maxlength <- 0L
  for (i in seq_len(numargs))
    if (length(x[[i]]) > maxlength)
      maxlength <- length(x[[i]])
  # maxlength guaranteed >= 1
  result <- max(unit.list.from.list(lapply(x, select.i, 1L)))
  if (maxlength > 1L)
      for (i in 2L:maxlength)
          result <- unit.c(result, max(unit.list.from.list(lapply(x, select.i, i))))
  result
}

unit.pmin <- function(...) {

  select.i <- function(unit, i) {
    `[`(unit, i, top=FALSE)
  }

  x <- list(...)
  numargs <- length(x)
  if (numargs == 0L)
    stop("Zero arguments where at least one expected")
  # how long will the result be?
  maxlength <- 0L
  for (i in seq_len(numargs))
    if (length(x[[i]]) > maxlength)
      maxlength <- length(x[[i]])
  # maxlength guaranteed >= 1
  result <- min(unit.list.from.list(lapply(x, select.i, 1L)))
  if (maxlength > 1L)
      for (i in 2L:maxlength)
          result <- unit.c(result, min(unit.list.from.list(lapply(x, select.i, i))))
  result
}

#########################
# UNIT LISTS
# The idea with these is to allow arbitrary combinations
# of unit objects and unit arithmetic objects
#########################

# create a unit list from a unit, unit.arithmetic, or unit.list object
unit.list <- function(unit) {
  if (is.unit.list(unit))
    unit
  else {
    l <- length(unit)
    result <- vector("list", l)
    for (i in seq_len(l))
      result[[i]] <- unit[i]
    class(result) <- c("unit.list", "unit")
    result
  }
}

is.unit.list <- function(x) {
  inherits(x, "unit.list")
}

as.character.unit.list <- function(x, ...) {
  l <- length(x)
  result <- character(l)
  for (i in seq_len(l))
    result[i] <- as.character(x[[i]])
  result
}

#########################
# These work on any sort of unit object
#########################

is.unit <- function(unit) {
  inherits(unit, "unit")
}

print.unit <- function(x, ...) {
  print(as.character(x), quote=FALSE, ...)
  invisible(x)
}

#########################
# Unit subsetting
#########################

# The idea of the "top" argument is to allow the function to
# know if it has been called from the command-line or from
# a previous (recursive) call to "[.unit" or "[.unit.arithmetic"
# this allows recycling beyond the end of the unit object
# except at the top level

# NOTE that "unit" and "data" attributes will be recycled
`[.unit` <- function(x, index, top=TRUE, ...) {
  this.length <- length(x)
  if (is.logical(index))
    index <- (1L:this.length)[index]
  # Allow for negative integer index
  if (any(index < 0)) {
      if (any(index > 0))
          stop("cannot mix signs of indices")
      else
          index <- (1L:this.length)[index]
  }
  if (top && any(index > this.length))
    stop("index out of bounds ('unit' subsetting)")
  cl <- class(x)
  units <- attr(x, "unit")
  valid.units <- attr(x, "valid.unit")
  data <- attr(x, "data")
  class(x) <- NULL
  # The line below may seem slightly odd, but it should only be
  # used to recycle values when this method is called to
  # subset an argument in a unit.arithmetic object
  x <- x[(index - 1) %% this.length + 1]
  attr(x, "unit") <- units[(index - 1) %% length(units) + 1]
  attr(x, "valid.unit") <- valid.units[(index - 1) %% length(valid.units) + 1]
  data.list <- data[(index - 1) %% length(data) + 1]
  attr(x, "data") <- data.list
  class(x) <- cl
  x
}

# NOTE that units will be recycled to the length of the largest
# of the arguments
`[.unit.arithmetic` <- function(x, index, top=TRUE, ...) {
  this.length <- length(x)
  if (is.logical(index))
    index <- (1L:this.length)[index]
  # Allow for negative integer index
  if (any(index < 0)) {
      if (any(index > 0))
          stop("cannot mix signs of indices")
      else
          index <- (1L:this.length)[index]
  }
  if (top && any(index > this.length))
    stop("index out of bounds (unit arithmetic subsetting)")

  repSummaryUnit <- function(x, n) {
      newUnits <- lapply(seq_len(n), function(z) { get(x$fname)(x$arg1) })
      class(newUnits) <- c("unit.list", "unit")
      newUnits
  }

  switch(x$fname,
         "+"=`[`(x$arg1, (index - 1) %% this.length + 1, top=FALSE) +
             `[`(x$arg2, (index - 1) %% this.length + 1, top=FALSE),
         "-"=`[`(x$arg1, (index - 1) %% this.length + 1, top=FALSE) -
             `[`(x$arg2, (index - 1) %% this.length + 1, top=FALSE),
         # Recycle multiplier if necessary
         "*"=x$arg1[(index - 1) %% length(x$arg1) + 1] *
             `[`(x$arg2, (index - 1) %% this.length + 1, top=FALSE),
         "min"=repSummaryUnit(x, length(index)),
         "max"=repSummaryUnit(x, length(index)),
         "sum"=repSummaryUnit(x, length(index)))
}

`[.unit.list` <- function(x, index, top=TRUE, ...) {
  this.length <- length(x)
  if (is.logical(index))
    index <- (1L:this.length)[index]
  # Allow for negative integer index
  if (any(index < 0)) {
      if (any(index > 0))
          stop("cannot mix signs of indices")
      else
          index <- (1L:this.length)[index]
  }
  if (top && any(index > this.length))
    stop("index out of bounds (unit list subsetting)")
  cl <- class(x)
  result <- unclass(x)[(index - 1) %% this.length + 1]
  class(result) <- cl
  result
}

# Write `[<-.unit` methods too ??

#########################
# str() method
#########################

# Should work fine on atomic units and on unit.list
# The problem arises with unit.arithmetic, which are stored as lists
# but act like vectors
# (e.g., report length greater than number of list components)
str.unit.arithmetic <- function(object, ...) {
    cat("Class 'unit.arithmetic' [1:", length(object), "] ", sep="")
    str(unclass(object), ...)
}

#########################
# "c"ombining unit objects
#########################

# NOTE that I have not written methods for c()
# because method dispatch occurs on the first argument to
# "c" so c(unit(...), ...) would come here, but c(whatever, unit(...), ...)
# would go who-knows-where.
# A particularly nasty example is:  c(1, unit(1, "npc")) which will
# produce the same result as c(1, 1)
# Same problem for trying to control c(<unit>, <unit.arithmetic>)
# versus c(<unit.arithmetic>, <unit>), etc ...

# If any arguments are unit.arithmetic or unit.list, then the result will be
# unit.list
unit.c <- function(...) {
    x <- list(...)
    if (!all(sapply(x, is.unit)))
        stop("it is invalid to combine 'unit' objects with other types")
    listUnit <- function(x) {
        inherits(x, "unit.list") ||
        inherits(x, "unit.arithmetic")
    }
    ual <- any(sapply(x, listUnit))
    if (ual)
        unit.list.from.list(x)
    else {
        values <- unlist(x)
        unitUnits <- function(x) {
            rep(attr(x, "unit"), length.out=length(x))
        }
        units <- unlist(lapply(x, unitUnits))
        unitData <- function(x) {
            data <- attr(x, "data")
            if (is.null(data))
                vector("list", length(x))
            else
                recycle.data(data, TRUE, length(x), unitUnits(x))
        }
        data <- do.call("c", lapply(x, unitData))
        unit(values, units, data=data)
    }
}

unit.list.from.list <- function(x) {
    result <- do.call("c", lapply(x, unit.list))
    class(result) <- c("unit.list", "unit")
    result
}

#########################
# rep'ing unit objects
#########################

rep.unit <- function(x, times=1, length.out=NA, each=1, ...) {
    if (length(x) == 0)
        stop("invalid 'unit' object")

    # Determine an approprite index, then call subsetting code
    repIndex <- rep(seq_along(x), times=times, length.out=length.out, each=each)
    x[repIndex, top=FALSE]
}

# Vestige from when rep() was not generic
unit.rep <- function (x, ...)
{
  warning("'unit.rep' has been deprecated in favour of a unit method for the generic rep function", domain = NA)
  rep(x, ...)
}

#########################
# Length of unit objects
#########################

length.unit <- function(x) {
  length(unclass(x))
}

length.unit.list <- function(x) {
  length(unclass(x))
}

length.unit.arithmetic <- function(x) {
  switch(x$fname,
         "+"=max(length(x$arg1), length(x$arg2)),
         "-"=max(length(x$arg1), length(x$arg2)),
         "*"=max(length(x$arg1), length(x$arg2)),
         "min" = 1L,
         "max" = 1L,
         "sum" = 1L)
}

# Vestige of when length was not generic
unit.length <- function(unit) {
   warning("'unit.length' has been deprecated in favour of a unit method for the generic length function", domain = NA)
   length(unit)
}

#########################
# Convenience functions
#########################

stringWidth <- function(string) {
    n <- length(string)
    if (is.language(string)) {
        data <- vector("list", n)
        for (i in 1L:n)
            data[[i]] <- string[i]
    } else {
        data <- as.list(as.character(string))
    }
    unit(rep(1, n), "strwidth", data=data)
}

stringHeight <- function(string) {
    n <- length(string)
    if (is.language(string)) {
        data <- vector("list", n)
        for (i in 1L:n)
            data[[i]] <- string[i]
    } else {
        data <- as.list(as.character(string))
    }
    unit(rep(1, n), "strheight", data=data)
}

stringAscent <- function(string) {
    n <- length(string)
    if (is.language(string)) {
        data <- vector("list", n)
        for (i in 1L:n)
            data[[i]] <- string[i]
    } else {
        data <- as.list(as.character(string))
    }
    unit(rep(1, n), "strascent", data=data)
}

stringDescent <- function(string) {
    n <- length(string)
    if (is.language(string)) {
        data <- vector("list", n)
        for (i in 1L:n)
            data[[i]] <- string[i]
    } else {
        data <- as.list(as.character(string))
    }
    unit(rep(1, n), "strdescent", data=data)
}

convertTheta <- function(theta) {
    if (is.character(theta))
        # Allow some aliases for common angles
        switch(theta,
               east=0,
               north=90,
               west=180,
               south=270,
               stop("invalid 'theta'"))
    else
        # Ensure theta in [0, 360)
        theta <- as.numeric(theta) %% 360
}

# grobX
grobX <- function(x, theta) {
    UseMethod("grobX", x)
}

grobX.grob <- function(x, theta) {
  unit(convertTheta(theta), "grobx", data=x)
}

grobX.gList <- function(x, theta) {
  unit(rep(convertTheta(theta), length(gList)), "grobx", data=x)
}

grobX.gPath <- function(x, theta) {
  unit(convertTheta(theta), "grobx", data=x)
}

grobX.default <- function(x, theta) {
  unit(convertTheta(theta), "grobx", data=gPathDirect(as.character(x)))
}

# grobY
grobY <- function(x, theta) {
    UseMethod("grobY", x)
}

grobY.grob <- function(x, theta) {
  unit(convertTheta(theta), "groby", data=x)
}

grobY.gList <- function(x, theta) {
  unit(rep(convertTheta(theta), length(gList)), "groby", data=x)
}

grobY.gPath <- function(x, theta) {
  unit(convertTheta(theta), "groby", data=x)
}

grobY.default <- function(x, theta) {
  unit(convertTheta(theta), "groby", data=gPathDirect(as.character(x)))
}

# grobWidth
grobWidth <- function(x) {
  UseMethod("grobWidth")
}

grobWidth.grob <- function(x) {
  unit(1, "grobwidth", data=x)
}

grobWidth.gList <- function(x) {
  unit(rep(1, length(gList)), "grobwidth", data=x)
}

grobWidth.gPath <- function(x) {
  unit(1, "grobwidth", data=x)
}

grobWidth.default <- function(x) {
  unit(1, "grobwidth", data=gPathDirect(as.character(x)))
}

# grobHeight
grobHeight <- function(x) {
  UseMethod("grobHeight")
}

grobHeight.grob <- function(x) {
  unit(1, "grobheight", data=x)
}

grobHeight.gList <- function(x) {
  unit(rep(1, length(gList)), "grobheight", data=x)
}

grobHeight.gPath <- function(x) {
  unit(1, "grobheight", data=x)
}

grobHeight.default <- function(x) {
  unit(1, "grobheight", data=gPathDirect(as.character(x)))
}

# grobAscent
grobAscent <- function(x) {
  UseMethod("grobAscent")
}

grobAscent.grob <- function(x) {
  unit(1, "grobascent", data=x)
}

grobAscent.gList <- function(x) {
  unit(rep(1, length(gList)), "grobascent", data=x)
}

grobAscent.gPath <- function(x) {
  unit(1, "grobascent", data=x)
}

grobAscent.default <- function(x) {
  unit(1, "grobascent", data=gPathDirect(as.character(x)))
}

# grobDescent
grobDescent <- function(x) {
  UseMethod("grobDescent")
}

grobDescent.grob <- function(x) {
  unit(1, "grobdescent", data=x)
}

grobDescent.gList <- function(x) {
  unit(rep(1, length(gList)), "grobdescent", data=x)
}

grobDescent.gPath <- function(x) {
  unit(1, "grobdescent", data=x)
}

grobDescent.default <- function(x) {
  unit(1, "grobdescent", data=gPathDirect(as.character(x)))
}

#########################
# Function to decide which values in a unit are "absolute" (do not depend
# on parent's drawing context or size)
#########################

# Only deals with unit of length() 1
absolute <- function(unit) {
  !is.na(match(attr(unit, "unit"),
               c("cm", "inches", "lines", "null",
                 "mm", "points", "picas", "bigpts",
                 "dida", "cicero", "scaledpts",
                 "strwidth", "strheight", "strascent", "strdescent", "char",
                 "mylines", "mychar", "mystrwidth", "mystrheight")))
}

# OLD absolute.unit
absolute.units <- function(unit) {
  UseMethod("absolute.units")
}

absolute.units.unit <- function(unit) {
  n <- length(unit)
  if (absolute(unit[1L]))
    abs.unit <- unit[1L]
  else
    abs.unit <- unit(1, "null")
  new.unit <- abs.unit
  count <- 1
  while (count < n) {
    count <- count + 1
    new.unit <- unit.c(new.unit, absolute.units(unit[count]))
  }
  new.unit
}

absolute.units.unit.list <- function(unit) {
  cl <- class(unit)
  abs.ul <- lapply(unit, absolute.units)
  class(abs.ul) <- cl
  abs.ul
}

absolute.units.unit.arithmetic <- function(unit) {
  switch(unit$fname,
         "+"=unit.arithmetic("+", absolute.units(unit$arg1),
           absolute.units(unit$arg2)),
         "-"=unit.arithmetic("-", absolute.units(unit$arg1),
           absolute.units(unit$arg2)),
         "*"=unit.arithmetic("*", unit$arg1, absolute.units(unit$arg2)),
         "min"=unit.arithmetic("min", absolute.units(unit$arg1)),
         "max"=unit.arithmetic("max", absolute.units(unit$arg1)),
         "sum"=unit.arithmetic("sum", absolute.units(unit$arg1)))
}


