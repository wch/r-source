\name{select.list}
\alias{select.list}
\title{Select Items from a List}
\description{
  Select item(s) from a character vector.
}
\usage{
select.list(list, preselect = NULL, multiple = FALSE, title = NULL)
}
\arguments{
  \item{list}{character. A list of items.}
  \item{preselect}{a character vector, or \code{NULL}.  If non-null and
    if the string(s) appear in the list, the item(s) are selected
    initially.}
  \item{multiple}{logical: can more than one item be selected?}
  \item{title}{optional character string for window title.}
}
\details{
  This brings up a modal dialog box with a (scrollable) list of items,
  which can be selected by the mouse.  If \code{multiple} is true,
  further items can be selected or deselected by holding the control key
  down whilst selecting, and shift-clicking can be used to select
  ranges.

  Normal termination is via the \code{OK} button or by hitting Enter.
  The selection can be aborted via the \code{Cancel} button or pressing Escape.
}
\value{
  A character vector of selected items.  If \code{multiple} is false and
  no item was selected (or \code{Cancel} was used), \code{""} is
  returned.   If \code{multiple} is true and no item was selected (or
  \code{Cancel} was used) then a character vector of length 0 is returned.
}
\examples{\dontrun{
select.list(sort(.packages(all.available = TRUE)))
}}
\keyword{utilities}
