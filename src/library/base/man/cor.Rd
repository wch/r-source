\name{cor}
\title{Correlation and Covariance Matrices}
\usage{
cor(x, y=x, use="all.obs")
cov(x, y=x, use="all.obs")
}
\alias{cor}
\alias{cov}
\arguments{
\item{x}{a matrix or data frame.}
\item{y}{a matrix or data frame.}
\item{use}{a character string giving the method for handling
missing observations. This must be one of the stringss
\code{"all.obs"}, \code{"complete.obs"} or \code{"pairwise.complete.obs"}
(abbreviations are acceptable).}
}
\value{
The correlation matrix or covariance matrix
of the columns of \code{x} and the columns of \code{y}.

If \code{use} has the value \code{"all.obs"}, then the presence
of missing observations will cause the computation to fail.
If \code{use} has the value \code{"complete.obs"} then missing values
are handled by casewise deletion.  Finally, if \code{use} has the
value \code{"pairwise.complete.obs"} then the correlation between
each pair of variables is computed using all complete pairs
of observations on those variables.
This can result in correlation matrices which are not
positive semidefinite.
}
\keyword{multivariate}
\keyword{array}
