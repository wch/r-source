\name{profile.mle-class}
\docType{class}
\alias{profile.mle-class}
\title{Class "profile.mle"; Profiling information for "mle" object}
\description{Likelihood profiles along each parameter of likelihood function}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("profile.mle",
  ...)}, but most often by invoking \code{profile} on an "mle" object.
}
\section{Slots}{
  \describe{
    \item{\code{profile}:}{Object of class \code{"list"}. List of
      profiles, one for each requested parameter. Each profile is a data
      frame with the first column called \code{z} being the signed square
      root of the -2 log likelihood ratio, and the others being the
      parameters with names prefixed by \code{par.vals.}}
    \item{\code{summary}:}{Object of class \code{"summary.mle"}. Summary
      of object being profiled.}
  }
}
\section{Methods}{
  \describe{
    \item{confint}{\code{signature(object = "profile.mle")}: Use profile
      to generate approximate confidence intervals for parameters.}
    \item{plot}{\code{signature(x = "profile.mle", y = "missing")}: Plot
      profiles for each parameter.}
  }
}
\seealso{
  \code{\link{mle}}, \code{\link{mle-class}}, \code{\link{summary.mle-class}} 
}
%% Do we want examples for classes??
%%\examples{
%%}
\keyword{classes}
