#  File src/library/grid/R/edit.R
#  Part of the R package, https://www.R-project.org
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
#  https://www.R-project.org/Licenses/

# All args just used as args to editGrob
gEdit <- function(...) {
  edit <- list(...)
  class(edit) <- "gEdit"
  edit
}

is.gEdit <- function(x) {
  inherits(x, "gEdit")
}

applyEdit <- function(x, edit) {
  if (is.null(edit)) {
    x
  } else {
    if (!is.gEdit(edit))
      stop("invalid 'edit' information")
    # Intended to handle whether edit has gPath spec or not
    newx <- do.call("editGrob", c(list(x), edit))
    # If edit was specified for non-existent child, newx will be NULL
    if (is.null(newx))
      x
    else
      newx
  }
}

# A list of gEdit's to apply to the same grob
gEditList <- function(...) {
  edits <- list(...)
  if (!all(sapply(edits, is.gEdit)))
    stop("'gEditList' can only contain 'gEdit' objects")
  class(edits) <- "gEditList"
  edits
}

is.gEditList <- function(x) {
  inherits(x, "gEditList")
}

applyEdits <- function(x, edits) {
  if (is.null(edits)) {
    x
  } else {
    if (is.gEdit(edits))
      applyEdit(x, edits)
    else {
      if (!inherits(edits, "gEditList"))
        stop("invalid 'edit' information")
      for (i in edits)
        x <- applyEdits(x, i)
      x
    }
  }
}

