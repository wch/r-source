\name{data.frame}
\title{Data Frames}
\usage{
data.frame(\dots, row.names = NULL, col.names = NULL, as.is = FALSE)

as.data.frame(x)
is.data.frame(x)

row.names(data.frame.obj)
print(data.frame.obj)
}
\alias{data.frame}
\alias{as.data.frame}
\alias{is.data.frame}
\alias{row.names}
\alias{print.data.frame}
\arguments{
  \item{\dots}{these arguments are of either the form \code{value} or
    \code{tag=value}.  Component names are created based on the tag (if
    present) or the deparsed argument itself.}
  \item{row.names}{a character vector giving the row names for the data
    frame.}
  \item{col.names}{a character vector giving names for the variables in
    the data frame.}
  \item{as.is}{a logical value indicating whether character variables
    should be left ``as is'' or converted to factors.}
}
\value{
  A data frame.  Data frames are the fundamental data structure used by
  most of R's modeling software.  They are tightly coupled collections
  of variables which share many of the properties of matrices.  The main
  difference being that the columns of a data frame may be of differing
  types (numeric, factor and character).

  \code{as.data.frame} attempts to coerce its argument to be a data
  frame.

  \code{is.data.frame} returns \code{TRUE} if its argument is a data
  frame and \code{FALSE} otherwise.
}
\seealso{
  \code{\link{read.table}}.
}
\keyword{classes}
\keyword{methods}
