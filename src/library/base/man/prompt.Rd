\name{prompt}
\title{Produce Prototype of an R Documentation File}
\usage{
prompt(object, \dots)

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
  Facilitate the constructing of files documenting \R functions.
}
\details{
  An ASCII file \code{filename} is produced containing the proper
  function and argument names of \code{object}.  You have to edit it
  before adding the documentation to the source tree, i.e., (currently)
  to \file{\$R\_HOME/src/library/base/man/}.
}
\seealso{
  \code{\link{prompt.data.frame}}.
  
  \code{\link{help}} and the chapter on ``Writing \R documentation'' in
  ``Writing \R Extensions'' (see the \file{doc/manual}
  subdirectory of the \R source tree).
}
\examples{
prompt(plot.default)
prompt(interactive, force.f = TRUE)
}
\keyword{documentation}
