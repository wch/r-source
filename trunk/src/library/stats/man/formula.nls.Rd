% File src/library/stats/man/formula.nls.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2010 R Core Team
% Distributed under GPL 2 or later

\name{formula.nls}
\encoding{UTF-8}
\alias{formula.nls}
\title{Extract Model Formula from nls Object}
\usage{
\method{formula}{nls}(x, \dots)
}
\arguments{
 \item{x}{an object inheriting from class \code{"nls"}, representing
   a nonlinear least squares fit.}
 \item{\dots}{further arguments passed to or from other methods.}
}
\description{
  Returns the model used to fit \code{object}.
}
\value{
  a formula representing the model used to obtain \code{object}.
}
\author{\enc{José}{Jose} Pinheiro and Douglas Bates}
\seealso{\code{\link{nls}}, \code{\link{formula}}}
\examples{
fm1 <- nls(circumference ~ A/(1+exp((B-age)/C)), Orange,
           start = list(A = 160, B = 700, C = 350))
formula(fm1)
}
\keyword{models}
