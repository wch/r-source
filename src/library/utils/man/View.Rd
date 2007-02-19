\name{View}
\alias{View}
\title{Invoke a Data Viewer}
\description{
  Invoke a Data Viewer on an \R object.
}
\usage{
View(x, title)
}
\arguments{
  \item{x}{an \R object.}
  \item{x}{title for viewer window.  Defaults to name of \code{x}.}
}
\details{
  Object \code{x} is coerced (if possible) to a data frame, and all
  non-numeric columns are then coerced to character.   The object is
  then viewed in a spreadsheet-like data viewer, a read-only version of
  \code{\link{data.entry}}.
}
\value{
  Invisible \code{NULL}.  The functions puts up a window and returns
  immediately: the window can be closed via its menus.
}
\seealso{
  \code{\link{edit.data.frame}},
  \code{\link{data.entry}}.
}
\keyword{utilities}
