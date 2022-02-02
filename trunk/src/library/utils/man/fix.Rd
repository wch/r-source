% File src/library/utils/man/fix.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{fix}
\alias{fix}
\title{Fix an Object}
\description{
  \code{fix} invokes \code{\link{edit}} on \code{x} and then assigns the new
  (edited) version of \code{x} in the user's workspace.
}
\usage{
fix(x, \dots)
}
\arguments{
  \item{x}{the name of an \R object, as a name or a character string.}
  \item{\dots}{arguments to pass to editor: see \code{\link{edit}}.}
}
\details{
  The name supplied as \code{x} need not exist as an \R object, in
  which case a function with no arguments and an empty body is supplied
  for editing.

  Editing an \R object may change it in ways other than are obvious: see
  the comment under \code{\link{edit}}.  See
  \code{\link{edit.data.frame}} for changes that can occur when editing
  a data frame or matrix.
}
\seealso{
  \code{\link{edit}},
  \code{\link{edit.data.frame}}
}
\examples{
\dontrun{
 ## Assume 'my.fun' is a user defined function :
 fix(my.fun)
 ## now my.fun is changed
 ## Also,
 fix(my.data.frame) # calls up data editor
 fix(my.data.frame, factor.mode="char") # use of ...
}}
\keyword{utilities}
