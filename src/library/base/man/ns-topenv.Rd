% File src/library/base/man/ns-topenv.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2015 R Core Team
% Distributed under GPL 2 or later

\name{ns-topenv}
\title{Top Level Environment}
\alias{topenv}
\description{
  Finding the top level environment from an environment \code{envir} and
  its enclosing environments.
}
\usage{
topenv(envir = parent.frame(),
       matchThisEnv = getOption("topLevelEnvironment"))
}
\arguments{
  \item{envir}{environment.}

  \item{matchThisEnv}{return this environment, if it matches before
      any other criterion is satisfied.  The default, the option
      \samp{topLevelEnvironment}, is set by \code{\link{sys.source}},
      which treats a specific environment as the top level environment.
      Supplying the argument as \code{NULL} means it will never match.}
}
\details{
  \code{topenv} returns the first top level environment found when
  searching \code{envir} and its enclosing environments.  An environment is
  considered top level if it is the internal environment of a namespace,
  a package environment in the search path, or \code{.GlobalEnv}.
}
\examples{
topenv(.GlobalEnv)
topenv(new.env()) # also global env
topenv(environment(ls))# namespace:base
topenv(environment(lm))# namespace:stats
\dontshow{
stopifnot(identical(.GlobalEnv,        topenv(new.env())),
          identical(.GlobalEnv,        topenv(.GlobalEnv)),
          identical(baseenv(),         topenv(baseenv())),
          identical(.BaseNamespaceEnv, topenv(.BaseNamespaceEnv)),
          identical(topenv(environment(ls)), asNamespace("base")),
          identical(topenv(environment(lm)), asNamespace("stats")))
}
}
\keyword{programming}
