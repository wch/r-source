\name{dataframeHelpers}
\title{Data Frame Auxiliary Functions}
\alias{row.names}
\alias{row.names.data.frame}
\alias{row.names.default}
\alias{row.names<-}
\alias{row.names<-.data.frame}
\alias{row.names<-.default}
%- methods:
\alias{plot.data.frame}
%
\alias{xpdrows.data.frame}
\description{
  These functions create or manipulate data frames, tightly coupled
  collections of variables which share many of the properties of
  matrices and of lists, used as the fundamental data structure by most
  of \R's modeling software.
}
\usage{
row.names(x)
row.names(x) <- value

\method{plot}{data.frame}(x, \dots)

xpdrows.data.frame(x, old.rows, new.rows)
}
\arguments{
  \item{x}{object of class \code{data.frame}.}
  \item{value}{a vector with the same length as the number of rows of
    \code{x}, to be coerced to character.  Duplicated or missing values
    are not allowed.}
  \item{\dots}{further arguments to \code{\link{stripchart}},
    \code{\link{plot.default}} or \code{\link{pairs}}.}
  \item{old.rows, new.rows}{row names for old and new rows.}
}
\value{
  \code{row.names} can be used to set and retrieve the row names of a
  data frame, similarly to \code{\link{rownames}} for arrays (and
  it is a generic function that calls \code{rownames} for an
  array argument).}

  \code{plot.data.frame}, a method of the \code{\link{plot}} generic,
  uses \code{\link{stripchart}} for \emph{one} variable,
  \code{\link{plot.default}} (scatterplot) for \emph{two} variables, and
  \code{\link{pairs}} (scatterplot matrix) otherwise.

  \code{xpdrows.data.frame} is an auxiliary function which expands the
  rows of a data frame.  It is used by the data frame methods of
  \code{[<-} and \code{[[<-} (which perform subscripted assignments on a
  data frame), and not intended to be called directly.
}
\references{
  Chambers, J. M. (1992)
  \emph{Data for models.}
  Chapter 3 of \emph{Statistical Models in S}
  eds J. M. Chambers and T. J. Hastie, Wadsworth \& Brooks/Cole.
}
\seealso{
  \code{\link{data.frame}}
}
\keyword{classes}
\keyword{methods}
