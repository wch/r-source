% File src/library/stats/man/IQR.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2010 R Core Team
% Distributed under GPL 2 or later

\name{IQR}
\alias{IQR}
\title{The Interquartile Range}
\usage{
IQR(x, na.rm = FALSE, type = 7)
}
\description{computes interquartile range of the \code{x} values.}
\arguments{
  \item{x}{a numeric vector.}
  \item{na.rm}{logical. Should missing values be removed?}
  \item{type}{an integer selecting one of the many quantile algorithms,
    see \code{\link{quantile}}.}
}
\details{
  Note that this function computes the quartiles using the
  \code{\link{quantile}} function rather than following
  Tukey's recommendations,
  i.e., \code{IQR(x) = quantile(x, 3/4) - quantile(x, 1/4)}.

  For normally \eqn{N(m,1)} distributed \eqn{X}, the expected value of
  \code{IQR(X)} is \code{2*qnorm(3/4) = 1.3490}, i.e., for a normal-consistent
  estimate of the standard deviation, use \code{IQR(x) / 1.349}.
}
\references{
  Tukey, J. W. (1977).
  \emph{Exploratory Data Analysis.}
  Reading: Addison-Wesley.
}
\seealso{
 \code{\link{fivenum}}, \code{\link{mad}} which is more robust,
 \code{\link{range}}, \code{\link{quantile}}.
}
\examples{
IQR(rivers)
}
\keyword{univar}
\keyword{robust}
\keyword{distribution}
