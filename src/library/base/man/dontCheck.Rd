% File src/library/base/man/identity.Rd
% Part of the R package, http://www.R-project.org
% Copyright 2013 R Core Team
% Distributed under GPL 2 or later

\name{dontCheck}
\alias{dontCheck}
\title{Identity Function to Suppress Checking}
\description{
  The \code{dontCheck} function is the same as \code{\link{identity}}, but 
  is interpreted by \command{R CMD check} code analysis as a directive
  to suppress checking of \code{x}.  Currently this is only used by
 \code{\link{checkFF}(registration = TRUE)}
  when checking the \code{.NAME} argument of foreign function calls.
}
\usage{
dontCheck(x)
}
\arguments{
  \item{x}{an \R object.}
}
\seealso{
  \code{suppressForeignCheck} which explains why that and
  \code{dontCheck} are undesirable and should be avoided if at all
  possible.
}

\keyword{programming}
