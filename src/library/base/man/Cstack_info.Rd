% File src/library/base/man/Cstack_info.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2013 R Core Team
% Distributed under GPL 2 or later

\name{Cstack_info}
\alias{Cstack_info}
\title{Report Information on C Stack Size and Usage}
\description{
  Report information on the C stack size and usage (if available).
}
\usage{
Cstack_info()
}
\details{
  On most platforms, C stack information is recorded when \R is
  initialized and used for stack-checking.  If this information is
  unavailable, the \code{size} will be returned as \code{NA}, and
  stack-checking is not performed.

  The information on the stack base address is thought to be accurate on
  Windows, Linux, OS X and FreeBSD but a heuristic is used on other
  platforms.  Because this might be slightly inaccurate, the current
  usage could be estimated as negative.  (The heuristic is not used on
  embedded uses of \R on platforms where the stack base is not thought
  to be accurate.)

  The \sQuote{evaluation depth} is the number of nested \R expressions
  currently under evaluation: this has a limit controlled by
  \code{\link{options}("expressions")}.
}
\value{
  An integer vector.  This has named elements
  \item{size}{The size of the stack (in bytes), or \code{NA} if unknown.}
  \item{current}{The estimated current usage (in bytes), possibly \code{NA}.}
  \item{direction}{\code{1} (stack grows down, the usual case) or
    \code{-1} (stack grows up).}
  \item{eval_depth}{The current evaluation depth (including two calls
    for the call to \code{Cstack_info}).}
}
\examples{\donttest{
Cstack_info()
}}
\keyword{ utilities }

