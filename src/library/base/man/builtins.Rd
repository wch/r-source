\name{builtins}
\alias{builtins}
\title{Returns the names of all built-in objects}
\description{
  Return the names of all the built-in objects.  These are fetched
  directly from the symbol table of the \R interpreter.
}
\usage{builtins(internal = FALSE)}
\arguments{
  \item{internal}{a logical indicating whether only ``internal''
    functions (which can be called via \code{\link{.Internal}}) should
    be returned.}
}
\keyword{utilities}
