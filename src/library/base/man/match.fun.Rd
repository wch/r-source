% File src/library/base/man/match.fun.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2011 R Core Team
% Distributed under GPL 2 or later

\name{match.fun}
\alias{match.fun}
\title{Extract a Function Specified by Name}
\description{
  When called inside functions that take a function as argument, extract
  the desired function object while avoiding undesired matching to
  objects of other types.
}
\usage{
match.fun(FUN, descend = TRUE)
}
\arguments{
 \item{FUN}{item to match as function: a function, symbol or
    character string.  See \sQuote{Details}.}
 \item{descend}{logical; control whether to search past non-function
     objects.}
}
\value{
  A function matching \code{FUN} or an error is generated.
}
\details{
  \code{match.fun} is not intended to be used at the top level since it
  will perform matching in the \emph{parent} of the caller.

  If \code{FUN} is a function, it is returned.  If it is a symbol (for
  example, enclosed in backquotes) or a
  character vector of length one, it will be looked up using \code{get}
  in the environment of the parent of the caller.  If it is of any other
  mode, it is attempted first to get the argument to the caller as a
  symbol (using \code{substitute} twice), and if that fails, an error is
  declared.

  If \code{descend = TRUE}, \code{match.fun} will look past non-function
  objects with the given name; otherwise if \code{FUN} points to a
  non-function object then an error is generated.

  This is used in base functions such as \code{\link{apply}},
  \code{\link{lapply}}, \code{\link{outer}}, and \code{\link{sweep}}.
}
\author{
  Peter Dalgaard and Robert Gentleman, based on an earlier version
  by Jonathan Rougier.
}
\section{Bugs}{
  The \code{descend} argument is a bit of misnomer and probably not
  actually needed by anything.  It may go away in the future.

  It is impossible to fully foolproof this.  If one \code{attach}es a
  list or data frame containing a length-one character vector with the
  same name as a function, it may be used (although namespaces
  will help).
}
\seealso{
  \code{\link{match.arg}}, \code{\link{get}}
}
\examples{
# Same as get("*"):
match.fun("*")
# Overwrite outer with a vector
outer <- 1:5
try(match.fun(outer, descend = FALSE)) #-> Error:  not a function
match.fun(outer) # finds it anyway
is.function(match.fun("outer")) # as well
}
\keyword{programming}
