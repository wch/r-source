% File src/library/stats/man/mauchly.test.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{mauchly.test}
\alias{mauchly.test}
\alias{mauchly.test.SSD}
\alias{mauchly.test.mlm}
\title{Mauchly's Test of Sphericity}
\description{
  Tests whether a Wishart-distributed covariance matrix (or
  transformation thereof) is proportional to a given matrix.
}
\usage{
mauchly.test(object, ...)
\method{mauchly.test}{mlm}(object, ...)
\method{mauchly.test}{SSD}(object, Sigma = diag(nrow = p),
   T = Thin.row(proj(M) - proj(X)), M = diag(nrow = p), X = ~0,
   idata = data.frame(index = seq_len(p)), ...)
}
\arguments{
  \item{object}{object of class \code{SSD} or \code{mlm}.}
  \item{Sigma}{matrix to be proportional to.}
  \item{T}{transformation matrix. By default computed from \code{M} and
    \code{X}.}
  \item{M}{formula or matrix describing the outer projection (see below).}
  \item{X}{formula or matrix describing the inner projection (see below).}
  \item{idata}{data frame describing intra-block design.}
  \item{\dots}{arguments to be passed to or from other methods.}
}
\details{
  Mauchly's test test for whether a covariance matrix can be assumed to
  be proportional to a given matrix.

  This is a generic function with methods for classes \code{"mlm"} and
  \code{"\link{SSD}"}.

  The basic method is for objects of
  class \code{SSD} the method for \code{mlm} objects just extracts the
  SSD matrix and invokes the corresponding method with the same options
  and arguments.

  The \code{T} argument is used to transform the observations prior to
  testing. This typically involves transformation to intra-block
  differences, but more complicated within-block designs can be
  encountered, making more elaborate transformations necessary. A
  matrix \code{T} can be given directly or specified as
  the difference between two projections onto the spaces spanned by
  \code{M} and \code{X}, which in turn can be given as matrices or as
  model formulas with respect to \code{idata} (the tests will be
  invariant to parametrization of the quotient space \code{M/X}).

  The common use of this test is in repeated measurements designs, with
  \code{X = ~1}. This is almost, but not quite the same as testing for
  compound symmetry in the untransformed covariance matrix.

  Notice that the defaults involve \code{p}, which is calculated
  internally as the dimension of the SSD matrix, and a couple of hidden
  functions in the \pkg{stats} namespace, namely \code{proj} which
  calculates projection matrices from design matrices or model formulas
  and \code{Thin.row} which removes linearly dependent rows from a
  matrix until it has full row rank.

}
\value{An object of class \code{"htest"}} %% perhaps elaborate?

\references{
  T. W. Anderson (1958).  \emph{An Introduction to Multivariate
  Statistical Analysis.} Wiley.
}

\seealso{\code{\link{SSD}}, \code{\link{anova.mlm}},
  \code{\link{rWishart}}
}

\note{
  The p-value differs slightly from that of SAS because a second order term
  is included in the asymptotic approximation in \R.
}

%% Probably use example from Baron/Li
\examples{
utils::example(SSD) # Brings in the mlmfit and reacttime objects

### traditional test of intrasubj. contrasts
mauchly.test(mlmfit, X = ~1)

### tests using intra-subject 3x2 design
idata <- data.frame(deg = gl(3, 1, 6, labels = c(0,4,8)),
                    noise = gl(2, 3, 6, labels = c("A","P")))
mauchly.test(mlmfit, X = ~ deg + noise, idata = idata)
mauchly.test(mlmfit, M = ~ deg + noise, X = ~ noise, idata = idata)
}
\keyword{htest}
\keyword{models}
\keyword{multivariate}

