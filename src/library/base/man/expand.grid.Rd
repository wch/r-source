\name{expand.grid}
\title{Create a Data Frame from All Combinations of Factors}
\usage{
expand.grid(\dots)
}
\alias{expand.grid}
\arguments{
  \item{\dots}{Vectors, factors or a list containing these. }
}
\description{
  Create a data frame from all combinations of the supplied vectors or
  factors.  See the description of the return value for precise details of
  the way this is done.
}
\value{
  A data frame containing one row for each combination of the supplied
  factors.  The first factors vary fastest.  The columns are labelled by
  the factors if these are supplied as named arguments or named
  components of a list.

  Attribute \code{"out.attrs"} is a list which gives the dimension and
  dimnames for use by \code{\link{predict}} methods.
}

\references{
  Chambers, J. M. and Hastie, T. J. (1992)
  \emph{Statistical Models in S.}
  Wadsworth \& Brooks/Cole.
}
\examples{
expand.grid(height = seq(60, 80, 5), weight = seq(100, 300, 50),
            sex = c("Male","Female"))
}
\keyword{models}
\keyword{array}
