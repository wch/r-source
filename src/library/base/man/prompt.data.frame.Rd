\name{prompt}
\title{Produce Prototype of an \R Documentation File}
\usage{
prompt.data.frame(object, filename = paste(name, ".Rd", sep = ""))
}
\alias{prompt.data.frame}
\arguments{
  \item{object}{a data frame}
  \item{filename}{name of the output file}
}
\description{
  The main goal is to facilitate the constructing of files documenting
  data frames in \R.

  An ASCII file \code{filename} is produced containing the data frame
  name and the names of the variables in the frame.  You have to edit it
  before adding the documentation to the source tree, i.e., (currently)
  to \file{\$R\_HOME/src/library/base/man/}.
}
\author{Douglas Bates \email{bates@stat.wisc.edu}}
\seealso{
  \code{\link{help}} and the tutorial about writing \R documentation,
  currently in the file \file{\$R\_HOME/doc/manual/writing-Rd.tex}.
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
