\name{var}
\title{Covariance Matrices}
\usage{
var(x, y=x, na.rm=FALSE, use)
}
\alias{var}
\arguments{
\item{x}{a matrix or vector.}
\item{y}{a matrix or vector.}
\item{na.rm}{logical.}
\item{use}{an optional character string giving a
method for computing covariances in the presence
of missing values.  This must be one of \code{"all.obs"},
\code{"complete.obs"} or \code{"pairwise.complete.obs"},
with abbreviation being permited.}
}
\description{
\code{var} computes the variance of \code{x} and the
covariance of \code{x} and \code{y} if \code{x} and \code{y}
are vectors. If \code{x} and \code{y} are matrices then
the covariance between the columns of \code{x} and the
the columns of \code{y} are computed.

If \code{na.rm} is \code{TRUE} then the complete observations (rows)
are use to compute the variance.  If \code{na.rm} is \code{FALSE}
and there are missing values, then \code{var} will fail.

The argument \code{use} can also be used for describing how
to handle missing values.
Specifying \code{use="all"} is equivalent to specifying
\code{na.rm=FALSE} and specifying \code{use="pair"} is equivalent to
\code{na.rm=TRUE}.
If \code{use="pair"}, then all the observations which are
complete for a pair of variables is used to compute the
covariance for that pair of variables.
This can result in covariance matrices which are not
positive semidefinite.
}
\examples{
var(1:10)
# 9.166667

var(1:5,1:5)
# 2.5
}
\keyword{univar}
\keyword{multivariate}
