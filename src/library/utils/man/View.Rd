% File src/library/utils/man/View.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2009 R Core Team
% Distributed under GPL 2 or later

\name{View}
\alias{View}
\title{Invoke a Data Viewer}
\description{
  Invoke a spreadsheet-style data viewer on a matrix-like \R object.
}
\usage{
View(x, title)
}
\arguments{
  \item{x}{an \R object which can be coerced to a data frame with
    non-zero numbers of rows and columns.}
  \item{title}{title for viewer window.  Defaults to name of \code{x}
    prefixed by \code{Data:}.}
}
\details{
  Object \code{x} is coerced (if possible) to a data frame, and all
  non-numeric columns are then coerced to character.   The object is
  then viewed in a spreadsheet-like data viewer, a read-only version of
  \code{\link{data.entry}}.

  If there are row names on the data frame that are not \code{1:nrow},
  they are displayed in a separate first column called \code{row.names}.

  Objects with zero columns or zero rows are not accepted.
#ifdef unix

  The array of cells can be navigated by the cursor keys and Home, End,
  Page Up and Page Down (where supported by X11) as well as Enter
  and Tab.
#endif
#ifdef windows

  The array of cells can be navigated \emph{via} the scrollbars and
  by the cursor keys, Home, End, Page Up and Page Down.

  The initial size of the data viewer window is taken from the default
  dimensions of a pager (see \code{\link{Rconsole}}), but adjusted
  downwards to show a whole number of rows and columns.
#endif
}
\value{
  Invisible \code{NULL}.  The functions puts up a window and returns
  immediately: the window can be closed via its controls or menus.
}
\seealso{
  \code{\link{edit.data.frame}},
  \code{\link{data.entry}}.
}
\keyword{utilities}
