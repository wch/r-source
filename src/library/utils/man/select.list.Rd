% File src/library/utils/man/select.list.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2013 R Core Team
% Distributed under GPL 2 or later

\name{select.list}
\alias{select.list}
\title{Select Items from a List}
\description{
  Select item(s) from a character vector.
}
\usage{
select.list(choices, preselect = NULL, multiple = FALSE,
            title = NULL, graphics = getOption("menu.graphics"))
}
\arguments{
  \item{choices}{a character vector of items.}
  \item{preselect}{a character vector, or \code{NULL}.  If non-null and
    if the string(s) appear in the list, the item(s) are selected
    initially.}
  \item{multiple}{logical: can more than one item be selected?}
  \item{title}{optional character string for window title, or
    \code{NULL} for no title.}
  \item{graphics}{logical: should a graphical widget be used?}
}
\details{
  The normal default is \code{graphics = TRUE}.
#ifdef windows
  This  brings up a modal dialog box with a (scrollable) list
  of items, which can be selected by the mouse.  If \code{multiple} is
  true, further items can be selected or deselected by holding the
  control key down whilst selecting, and shift-clicking can be used to
  select ranges.

  Normal termination is via the \sQuote{OK} button or by hitting Enter or
  double-clicking an item.  Selection can be aborted via the
  \sQuote{Cancel} button or pressing Escape.
#endif
#ifdef unix
  Under the OS X GUI this brings up a modal dialog box
  with a (scrollable) list of items, which can be selected by the mouse.
  On other Unix-like platforms it will use a
  Tcl/Tk listbox widget if possible.
#endif

  If \code{graphics} is FALSE or no graphical widget is available it
  displays a text list from which the user can choose by number(s). The
  \code{multiple = FALSE} case uses \code{\link{menu}}.  Preselection is
  only supported for \code{multiple = TRUE}, where it is indicated by a
  \code{"+"} preceding the item.

  It is an error to use \code{select.list} in a non-interactive session.
}
\value{
  A character vector of selected items.  If \code{multiple} is false and
  no item was selected (or \code{Cancel} was used), \code{""} is
  returned.   If \code{multiple} is true and no item was selected (or
  \code{Cancel} was used) then a character vector of length 0 is returned.
}
\seealso{
  \code{\link{menu}}, \code{\link{tk_select.list}} for a graphical
  version using Tcl/Tk.
}
\examples{\dontrun{
select.list(sort(.packages(all.available = TRUE)))
}}
\keyword{utilities}
