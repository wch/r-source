\name{prompt}
\title{Produce Prototype of an R Documentation File}
\description{
  Facilitate the constructing of files documenting \R functions.
}
\synopsis{
prompt.default(object,
    filename = paste0(name, ".Rd"), name = NULL, force.function = FALSE, \dots)

prompt.data.frame(object,
    filename = paste0(name, ".Rd"), name = NULL, \dots)
}
\usage{
prompt(object, \dots)

prompt.default(object,
    filename = paste(name, ".Rd", sep = ""),
    name = NULL, force.function = FALSE, \dots)

prompt.data.frame(object,
    filename = paste(name, ".Rd", sep = ""), name = NULL, \dots)
}
\alias{prompt}
\alias{prompt.default}
\alias{prompt.data.frame}
\arguments{
  \item{object}{an \R object, typically a function}
  \item{name}{character string specifying the name of the object.}
  \item{filename}{name of the output file}
  \item{force.function}{treat \code{object} as function in any case}
  \item{\dots}{further arguments passed to or from other methods.}
}
\details{
  An ASCII file \code{filename} is produced containing the proper
  function and argument names of \code{object}.  You have to edit it
  before adding the documentation to the source tree, i.e., (currently)
  to \file{\$R\_HOME/src/library/base/man/}.

  When \code{prompt} is used in \code{\link{for}} loops or scripts, the
  explicit \code{name} specification will be useful.
}
\note{
  The documentation file produced by \code{prompt.data.frame} does not
  have the same format as many of the data frame documentation files in
  the \code{base} library.  We are trying to settle on a preferred
  format for the documentation.
}
\seealso{
  \code{\link{help}} and the chapter on ``Writing \R documentation'' in
  ``Writing \R Extensions'' (see the \file{doc/manual}
  subdirectory of the \R source tree).
}
\author{Douglas Bates for \code{prompt.data.frame}}
\examples{
prompt(plot.default)
prompt(interactive, force.function = TRUE)
unlink("plot.default.Rd")
unlink("interactive.Rd")

data(women) # data.frame
prompt(women)
unlink("women.Rd")

data(sunspots) # non-data.frame data
prompt(sunspots)
unlink("sunspots.Rd")
}
\keyword{documentation}
