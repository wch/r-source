\name{prompt}
\title{Produce Prototype of an \R Documentation File}
\usage{
prompt(object, ...)

prompt.default(object,
    filename = paste(name, ".Rd", sep = ""),
    force.function = FALSE)
}
\alias{prompt}
\alias{prompt.default}
\arguments{
  \item{object}{an \R object, typically a function}
  \item{filename}{name of the output file}
  \item{force.function}{treat \code{object} as function in any case}
}
\description{
  The main goal is to facilitate the constructing of files documenting R
  functions.

  An ASCII file \code{filename} is produced containing the proper
  function and argument names of \code{object}.  You have to edit it
  before adding the documentation to the source tree, i.e., (currently)
  to \code{$RHOME/src/library/base/man/}.
}
\seealso{
  \code{\link{help}} and the tutorial about writing \R documentation,
  currently in the file \code{$RHOME/doc/manual/writing-Rd.tex}.
}
\examples{
prompt(plot.default)
prompt(interactive, force.f=TRUE)
}
\keyword{documentation}
