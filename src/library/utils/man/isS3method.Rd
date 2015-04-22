% File src/library/utils/man/isS3method.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2015 R Core Team
% Distributed under GPL 2 or later

\name{isS3method}
\alias{isS3method}
\title{Is 'method' the Name of an S3 Method?}
\description{
  Checks if \code{method} is the name of a valid / registered S3
  method.  Alternatively, when \code{f} and \code{class} are specified,
  it is checked if \code{f} is the name of an S3 generic function and
  \code{paste(f, class, sep=".")} is a valid S3 method.
}
\usage{
isS3method(method, f, class, envir = parent.frame())
}
\arguments{
  \item{method}{a character string, typically of the form
    \code{"<fn>.<class>"}.  If omitted, \code{f} and \code{class} have
    to be specified instead.}
  \item{f}{optional character string, typically specifying an S3 generic
    function.  Used, when \code{method} is not specified.}
  \item{class}{optional character string, typically specifying an S3
    class name.  Used, when \code{method} is not specified.}
  \item{envir}{the \code{\link{environment}} in which the method and its
    generic are searched first, as in \code{\link{getS3method}()}.}
  }
}
%% \details{

%% }
\value{
  \code{\link{logical}} \code{TRUE} or \code{FALSE}
}
\seealso{
  \code{\link{methods}}, \code{\link{getS3method}}.
}
\examples{
isS3method("t")           # FALSE - it is an S3 generic
isS3method("t.default")   # TRUE
isS3method("t.ts")        # TRUE
isS3method("t.test")      # FALSE
isS3method("t.data.frame")# TRUE
isS3method("t.lm")        # FALSE - not existing
isS3method("t.foo.bar")   # FALSE - not existing
}
\keyword{methods}
