\name{builtins}
\alias{builtins}
\title{Returns the Names of All Built-in Objects}
\description{
  Return the names of all the built-in objects.  These are fetched
  directly from the symbol table of the \R interpreter.
}
\usage{builtins(internal = FALSE)}
\arguments{
  \item{internal}{a logical indicating whether only \dQuote{internal}
    functions (which can be called via \code{\link{.Internal}}) should
    be returned.}
}
\details{
  \code{builtins()} returns an unsorted list of the objects in the
  symbol table, that is all the objects in the base environment.
  These are the built-in objects plus any that have been added
  subsequently when the base package was loaded.  It is less confusing
  to use \code{ls(baseenv(), all=TRUE)}.

  \code{builtins(TRUE)} returns an unsorted list of the names of internal
  functions, that is those which can be accessed as
  \code{.Internal(\var{foo}(args ...))} for \var{foo} in the list.
}
\value{
  A character vector.
}
\keyword{utilities}
