\name{promptData}
\alias{promptData}
\title{Generate a Shell for Documentation of Data Sets}
\description{
  Generates a shell of documentation for a data set.
}
\usage{
promptData(object, filename = NULL, name = NULL)
}
\arguments{
  \item{object}{an \R object to be documented as a data set.}
  \item{filename}{usually, a connection or a character string giving the
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
\section{Warning}{
  This function is still experimental.  Both interface and value might
  change in future versions.  In particular, it may be preferable to use
  a character string naming the data set and optionally a specification
  of where to look for it instead of using \code{object}/\code{name} as
  we currently do.  This would be different from \code{\link{prompt}},
  but consistent with other prompt-style functions in package
  \pkg{methods}, and also allow prompting for data set documentation
  without explicitly having to load the data set.
}
\seealso{\code{\link{prompt}}}
\examples{
promptData(sunspots)
unlink("sunspots.Rd")
}
\keyword{documentation}
