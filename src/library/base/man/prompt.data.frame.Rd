\name{prompt.data.frame}
\title{Produce Prototype of an R Documentation File}
\usage{
prompt.data.frame(object, filename = paste(name, ".Rd", sep = ""))
}
\alias{prompt.data.frame}
\arguments{
  \item{object}{a data frame}
  \item{filename}{name of the output file}
}
\description{
  Facilitate the constructing of files documenting \R data frames.
}
\details{
\description{
  An ASCII file \code{filename} is produced containing the data frame
  name and the names of the variables in the frame.  You have to edit it
  before adding the documentation to the source tree, i.e., (currently)
  to \file{\$R\_HOME/src/library/base/man/}.
}
\author{Douglas Bates \email{bates@stat.wisc.edu}}
\seealso{
  \code{\link{prompt}}.

  \code{\link{help}} and the chapter on ``Writing \R documentation'' in
  ``Writing \R Extensions'' (see the \file{doc/manual}
  subdirectory of the \R source tree).
}
\note{
  The documentation file produced by \code{prompt.data.frame} does not
  have the same format as many of the data frame documentation files in
  the \code{base} library.  We are trying to settle on a preferred
  format for the documentation.
}
\examples{
data(women)
prompt(women)
}
\keyword{documentation}
