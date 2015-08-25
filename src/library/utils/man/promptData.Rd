% File src/library/utils/man/promptData.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2011 R Core Team
% Distributed under GPL 2 or later

\name{promptData}
\alias{promptData}
\title{Generate Outline Documentation for a Data Set}
\description{
  Generates a shell of documentation for a data set.
}
\usage{
promptData(object, filename = NULL, name = NULL)
}
\arguments{
  \item{object}{an \R object to be documented as a data set.}
  \item{filename}{usually, a \link{connection} or a character string giving the
    name of the file to which the documentation shell should be written.
    The default corresponds to a file whose name is \code{name} followed
    by \code{".Rd"}.  Can also be \code{NA} (see below).}
  \item{name}{a character string specifying the name of the object.}
}
\value{
  If \code{filename} is \code{NA}, a list-style representation of the
  documentation shell.  Otherwise, the name of the file written to is
  returned invisibly.
}
\details{
  Unless \code{filename} is \code{NA}, a documentation shell for
  \code{object} is written to the file specified by \code{filename}, and
  a message about this is given.

  If \code{filename} is \code{NA}, a list-style representation of the
  documentation shell is created and returned.  Writing the shell to a
  file amounts to \code{cat(unlist(x), file = filename, sep = "\\n")},
  where \code{x} is the list-style representation.

  Currently, only data frames are handled explicitly by the code.
}
\seealso{\code{\link{prompt}}}
\examples{
promptData(sunspots)
unlink("sunspots.Rd")
}
\keyword{documentation}
