% File src/library/base/man/declare.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2023 R Core Team
% Distributed under GPL 2 or later

\name{declare}
\alias{declare}
\title{
  Declarations
}
\description{
  A framework for specifying information about R code for use by the
  interpreter, compiler, and code analysis tools.
}
\usage{
declare(...)
}
\arguments{
  \item{\dots}{declaration expressions.}
}
\details{
  A syntax for declaration expressions is still being developed.
}
\value{
  Evaluating a \code{declare()} call ignores the arguments and returns
  \code{NULL} invisibly.
}
\keyword{programming}
