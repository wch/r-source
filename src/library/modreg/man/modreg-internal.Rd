\name{modreg-internal}
\title{Internal modreg functions}
\alias{predLoess}
\alias{simpleLoess}
\alias{pointwise}
\description{
  Internal modreg functions.
}
\usage{
predLoess(y, x, newx, s, weights, robust, span, degree, normalize, 
    parametric, drop.square, surface, cell, family, kd, divisor, 
    se = FALSE)
simpleLoess(y, x, weights, span = 0.75, degree = 2, parametric = FALSE, 
    drop.square = FALSE, normalize = TRUE, statistics = "approximate", 
    surface = "interpolate", cell = 0.2, iterations = 1,
    trace.hat = "exact") 
pointwise(results, coverage)
}
\details{
  These are not to be called by the user.
}
\keyword{internal}
