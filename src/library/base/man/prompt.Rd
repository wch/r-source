\name{prompt}
\title{Produce Prototype of an R Documentation File}
\alias{prompt}
\alias{prompt.default}
\alias{prompt.data.frame}
\description{
  Facilitate the constructing of files documenting \R objects.
}
\usage{
prompt(object, filename = NULL, name = NULL, \dots)
prompt.default(object, filename = NULL, name = NULL,
               force.function = FALSE, \dots)
prompt.data.frame(object, filename = NULL, name = NULL, \dots)
}
\arguments{
  \item{object}{an \R object, typically a function.}
  \item{name}{a character string specifying the name of the object.}
  \item{filename}{usually, a connection or a character string giving the
    name of the file to which the documentation shell should be written.
    The default corresponds to a file whose name is \code{name} followed
    by \code{".Rd"}.  Can also be \code{NA} (see below).}
  \item{force.function}{a logical.  If \code{TRUE}, treat \code{object}
    as function in any case.}
  \item{\dots}{further arguments passed to or from other methods.}
}
\value{
  Unless \code{filename} is \code{NA}, it is returned invisibly;
  otherwise, a list-style representation of the documentation shell.
}
\details{
  Unless \code{filename} is \code{NA}, a documentation shell for
  \code{object} is written to \code{filename}, and a message about this
  is given.  For function objects, this shell contains the proper
  function and argument names.  R documentation files thus created still
  need to be edited and moved into the \file{man} subdirectory of the
  package containing the object to be documented.

  If \code{filename} is \code{NA}, a list-style representation of the
  documentation shell is returned.  Writing the shell to a file really
  amounts to \code{cat(unlist(x), file = filename, sep = "\n")}, where
  \code{x} is the list-style representation.

  When \code{prompt} is used in \code{\link{for}} loops or scripts, the
  explicit \code{name} specification will be useful.
}
\note{
  The documentation file produced by \code{prompt.data.frame} does not
  have the same format as many of the data frame documentation files in
  the \code{base} library.  We are trying to settle on a preferred
  format for the documentation.
}
\section{Warning}{
  Currently, calling \code{prompt} on a non-function object assumes that
  the object is in fact a data set and hence documents it as such.  This
  may change in future versions of \R.  Use \code{\link{promptData}} to
  create documentation skeletons for data sets.
}
\seealso{
  \code{\link{promptData}}, \code{\link{help}} and the chapter on
  ``Writing \R documentation'' in ``Writing \R Extensions'' (see the
  \file{doc/manual} subdirectory of the \R source tree).
}
\author{Douglas Bates for \code{prompt.data.frame}}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
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
