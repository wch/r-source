% File src/library/utils/man/isS3stdGeneric.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2016 R Core Team
% Distributed under GPL 2 or later

\name{isS3stdGeneric}
\alias{isS3stdGeneric}
\title{Check if a Function Acts as an S3 Generic}
\description{
  Determines whether \code{f} acts as a standard S3-style generic
  function.
}
\usage{
isS3stdGeneric(f)
}
\arguments{
  \item{f}{A function object}
}
\details{
  A closure is considered a standard S3 generic if the first expression
  in its body calls \code{\link{UseMethod}}. Functions which perform
  operations before calling \code{UseMethod} will not be considered
  \dQuote{standard} S3 generics.
}
\value{
  If \code{f} is an S3 generic, a logical vector containing \code{TRUE}
  with the name of the S3 generic (the string passed to
  \code{UseMethod}). Otherwise, \code{FALSE} (unnamed).
}
\keyword{utilities}
