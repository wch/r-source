\name{tk_select.list}
\alias{tk_select.list}
\title{Select Items from a List}
\description{
  Select item(s) from a character vector using a Tk listbox.
}
\usage{
tk_select.list(list, preselect = NULL, multiple = FALSE, title = NULL)
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
  This is a version of \code{\link{select.list}} implemented as a Tk
  list box plus \code{OK} and \code{Cancel} buttons.  There will be a
  scrollbar if the list is too long to fit comfortably on the screen.

  The dialog box is \emph{modal}, so a selection must be made or
  cancelled before the \R session can proceed.
}
\value{
  A character vector of selected items.  If \code{multiple} is false and
  no item was selected (or \code{Cancel} was used), \code{""} is
  returned.   If \code{multiple} is true and no item was selected (or
  \code{Cancel} was used) then a character vector of length 0 is returned.
}
\seealso{
  \code{\link{select.list}} (a text version except on Windows),
  \code{\link{menu}} (whose \code{graphics=TRUE} mode uses this
  on most Unix-alikes.
}
\keyword{utilities}

