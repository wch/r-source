% File src/library/stats4/man/update-methods.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{update-methods}
\docType{methods}
\alias{update-methods}
\alias{update,ANY-method}
\alias{update,mle-method}
\title{Methods for Function \code{update} in Package \pkg{stats4}}
\description{
  Update \code{"mle"} objects.
}
\usage{
\S4method{update}{mle}(object, \dots, evaluate = TRUE)
}
\arguments{
 \item{object}{An existing fit.}
 \item{\dots}{Additional arguments to the call, or arguments with
   changed values. Use \code{name = NULL} to remove the argument \code{name}.}
 \item{evaluate}{If true evaluate the new call else return the call.}
}
\section{Methods}{
  \describe{
    \item{\code{signature(object = "ANY")}}{Generic function: see
      \code{\link[stats]{update}}.}
    \item{\code{signature(object = "mle")}}{Update a fit.}
  }
}
\examples{
x <- 0:10
y <- c(26, 17, 13, 12, 20, 5, 9, 8, 5, 4, 8)
ll <- function(ymax = 15, xhalf = 6)
    -sum(stats::dpois(y, lambda = ymax/(1+x/xhalf), log = TRUE))
fit <- mle(ll)
## note the recorded call contains ..1, a problem with S4 dispatch
update(fit, fixed = list(xhalf = 3))
}
\keyword{methods}
