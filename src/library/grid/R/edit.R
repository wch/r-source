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
      stop("Invalid edit information")
    # Intended to handle whether edit has gPath spec or not
    do.call("editGrob", c(list(x), edit))
  }
}

# A list of gEdit's to apply to the same grob
gEditList <- function(...) {
  edits <- list(...)
  if (!all(sapply(edits, is.gEdit)))
    stop("gEditList can only contain gEdit objects")
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
        stop("Invalid edit information")
      for (i in edits) 
        x <- applyEdits(x, i)
      x
    }
  } 
}

