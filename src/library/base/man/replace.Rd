% File src/library/base/man/replace.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{replace}
\title{Replace Values in a Vector}
\usage{
replace(x, list, values)
}
\alias{replace}
\description{
\code{replace} replaces the values in \code{x}
with indices given in \code{list} by those given in \code{values}.
If necessary, the values in \code{values} are recycled.
}
\arguments{
  \item{x}{vector}
  \item{list}{an index vector}
  \item{values}{replacement values}
}
\value{
  A vector with the values replaced.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
\note{
  \code{x} is unchanged: remember to assign the result.
}
\keyword{manip}
