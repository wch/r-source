\name{subset}
\alias{subset}
\alias{subset.default}
\alias{subset.matrix}
\alias{subset.data.frame}
\title{Subsetting Vectors, Matrices and Data Frames}
\description{
  Return subsets of vectors, matrices or data frames which meet conditions.
}
\usage{
subset(x, \dots)

\method{subset}{default}(x, subset, \dots)

\method{subset}{matrix}(x, subset, drop = FALSE, \dots)

\method{subset}{data.frame}(x, subset, select, drop = FALSE, \dots)
}
\arguments{
  \item{x}{object to be subsetted.}
  \item{subset}{logical expression indicating elements or rows to keep:
    missing values are taken as false.}
  \item{select}{expression, indicating columns to select from a
    data frame.}
  \item{drop}{passed on to \code{[} indexing operator.}
  \item{\dots}{further arguments to be passed to or from other methods.}
}
\details{
  This is a generic function, with methods supplied for matrices, data
  frames and vectors (including lists).  Packages and users can add
  further methods.
  
  For ordinary vectors, the result is simply
  \code{x[subset & !is.na(subset)]}.

  For data frames, the \code{subset} argument works on the rows.  Note
  that \code{subset} will be evaluated in the data frame, so columns can
  be referred to (by name) as variables in the expression (see the examples).

  The \code{select} argument exists only for the methods for data frames
  and matrices.  It works by first replacing column names in the
  selection expression with the corresponding column numbers in the data
  frame and then using the resulting integer vector to index the
  columns.  This allows the use of the standard indexing conventions so
  that for example ranges of columns can be specified easily, or single
  columns can be dropped (see the examples).

  The \code{drop} argument is passed on to the indexing method for
  matrices and data frames: note that the default for matrices is
  different from that for indexing.
}
\value{
  An object similar to \code{x} contain just the selected elements (for
  a vector), rows (for a matrix) rows and columns (for a data frame),
  and so on.
}
\author{Peter Dalgaard and Brian Ripley}
\seealso{
  \code{\link{[}}, % = ./Extract.Rd
  \code{\link{transform}}
}
\examples{
subset(airquality, Temp > 80, select = c(Ozone, Temp))
subset(airquality, Day == 1, select = -Temp)
subset(airquality, select = Ozone:Wind)

with(airquality, subset(Ozone, Temp > 80))

subset(state.x77, 1:5, Illiteracy:Murder)
}
\keyword{manip}
