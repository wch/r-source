% File src/library/tcltk/man/tk_select.list.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2013 R Core Team
% Distributed under GPL 2 or later

\name{tk_select.list}
\alias{tk_select.list}
\title{Select Items from a List}
\description{
  Select item(s) from a character vector using a Tk listbox.
}
\usage{
tk_select.list(choices, preselect = NULL, multiple = FALSE,
               title = NULL)
}
\arguments{
  \item{choices}{a character vector of items.}
  \item{preselect}{a character vector, or \code{NULL}.  If non-null and
    if the string(s) appear in the list, the item(s) are selected
    initially.}
  \item{multiple}{logical: can more than one item be selected?}
  \item{title}{optional character string for window title, or
    \code{NULL} for no title.}
}
\details{
  This is a version of \code{\link{select.list}} implemented as a Tk
  list box plus \code{OK} and \code{Cancel} buttons.  There will be a
  scrollbar if the list is too long to fit comfortably on the screen.

  The dialog box is \emph{modal}, so a selection must be made or
  cancelled before the \R session can proceed.  Double-clicking on an
  item is equivalent to selecting it and then clicking \code{OK}.

  If Tk is version 8.5 or later, themed widgets will be used.
}
\value{
  A character vector of selected items.  If \code{multiple} is false and
  no item was selected (or \code{Cancel} was used), \code{""} is
  returned.   If \code{multiple} is true and no item was selected (or
  \code{Cancel} was used) then a character vector of length 0 is returned.
}
\seealso{
  \code{\link{select.list}} (a text version except on Windows and the OS X GUI),
  \code{\link{menu}} (whose \code{graphics = TRUE} mode uses this
  on most Unix-alikes).
}
\keyword{utilities}

