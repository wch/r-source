\name{xtfrm}
\alias{xtfrm}
\alias{xtfrm.default}
\alias{xtfrm.factor}
\alias{xtfrm.Surv}
\alias{xtfrm.Date}
\alias{xtfrm.POSIXct}
\alias{xtfrm.POSIXlt}
\title{Auxiliary Function for Sorting and Ranking}
\description{
  A generic auxiliary function that produces a numeric vector which
  will sort in the same order as \code{x}.
}
\usage{
xtfrm(x)
}
\arguments{
  \item{x}{an \R object.}
}
\details{
  This is a special case of ranking, but as a less general function than
  \code{\link{rank}} is more suitable to be made generic.  The default
  method is equivalent to \code{rank(x, ties.method="min",
    na.last="keep")}, so \code{NA} values are given rank \code{NA} and
  all tied values are given equal integer rank.

  The \code{\link{factor}} method extracts the codes.  The
  \code{\link[survival]{Surv}} method sorts first on times and then on
  status code(s).

  The default method will make use of \code{==}, \code{>}  and
  \code{is.na} methods for the class of \code{x}, but might be rather
  slow when doing so.
}
\value{
  A numeric (usually integer) vector of the same length as \code{x}.
}
\seealso{
  \code{\link{rank}}, \code{\link{sort}}, \code{\link{order}}.
}
\keyword{univar}
\keyword{manip}
