% File src/library/stats/man/preplot.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{preplot}
\alias{preplot}
\title{Pre-computations for a Plotting Object}
\description{
    Compute an object to be used for plots relating to the given model object.
}
\usage{
preplot(object, \dots)
}
\arguments{
 \item{object}{a fitted model object.}
 \item{\dots}{additional arguments for specific methods.}
}
\details{
    Only the generic function is currently provided in base \R, but some
    add-on packages have methods. Principally here for S compatibility.
}
\value{
    An object set up to make a plot that describes \code{object}.
}
\keyword{models}
