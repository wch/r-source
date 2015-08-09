% File src/library/base/man/format.pval.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2010 R Core Team
% Distributed under GPL 2 or later

\name{format.pval}
\title{Format P Values}
\description{
  \code{format.pval} is intended for formatting p-values.
}
\alias{format.pval}
\usage{
format.pval(pv, digits = max(1, getOption("digits") - 2),
            eps = .Machine$double.eps, na.form = "NA", ...)
}
\arguments{
  \item{pv}{a numeric vector.}
  \item{digits}{how many significant digits are to be used.}
  \item{eps}{a numerical tolerance: see \sQuote{Details}.}
  \item{na.form}{character representation of \code{NA}s.}
  \item{\dots}{further arguments to be passed to \code{\link{format}}
    such as \code{nsmall}.}
}
\value{
  A character vector.
}
\details{
  \code{format.pval} is mainly an auxiliary function for
  \code{\link{print.summary.lm}} etc., and does separate formatting for
  fixed, floating point and very small values; those less than
  \code{eps} are formatted as \code{"< [eps]"} (where \sQuote{[eps]}
  stands for \code{format(eps, digits)}).
}
\examples{
format.pval(c(stats::runif(5), pi^-100, NA))
format.pval(c(0.1, 0.0001, 1e-27))
}
\keyword{print}
