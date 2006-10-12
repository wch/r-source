\name{Ops.Date}
\alias{+.Date}
\alias{-.Date}
\alias{Ops.Date}

\title{Operators on the Date Class}
\description{
  Operators for the \code{"\link{Date}"} class.

  There is an \code{\link[base:groupGeneric]{Ops}} method and specific
  methods for \code{+} and \code{-} for the \code{\link{Date}} class.
}
\usage{
\special{date + x}
\special{date - x}
\special{date1 lop date2}
}
\arguments{
  \item{date}{date objects}
  \item{date1, date2}{date objects or character vectors.  (Character
    vectors are converted by \code{\link{as.Date}}.}
  \item{x}{a numeric vector (in days) \emph{or} an object of class
    \code{"\link{difftime}"}.}
  \item{lop}{One of \code{==}, \code{!=}, \code{<}, \code{<=}, \code{>}
    or \code{>=}.}
}
\examples{
(z <- Sys.Date())
z + 10
z < c("2006-06-01", "2007-01-01", "2010-01-01")
}
\keyword{utilities}
\keyword{chron}
