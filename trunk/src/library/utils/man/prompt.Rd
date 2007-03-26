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

\method{prompt}{default}(object, filename = NULL, name = NULL,
       force.function = FALSE, \dots)

\method{prompt}{data.frame}(object, filename = NULL, name = NULL, \dots)
}
\arguments{
  \item{object}{an \R object, typically a function for the default
    method.}
  \item{filename}{usually, a connection or a character string giving the
    name of the file to which the documentation shell should be written.
    The default corresponds to a file whose name is \code{name} followed
    by \code{".Rd"}.  Can also be \code{NA} (see below).}
  \item{name}{a character string specifying the name of the object.}
  \item{force.function}{a logical.  If \code{TRUE}, treat \code{object}
    as function in any case.}
  \item{\dots}{further arguments passed to or from other methods.}
}
\value{
  If \code{filename} is \code{NA}, a list-style representation of the
  documentation shell.  Otherwise, the name of the file written to is
  returned invisibly.
}
\details{
  Unless \code{filename} is \code{NA}, a documentation shell for
  \code{object} is written to the file specified by \code{filename}, and
  a message about this is given.  For function objects, this shell
  contains the proper function and argument names.  R documentation
  files thus created still need to be edited and moved into the
  \file{man} subdirectory of the package containing the object to be
  documented.

  If \code{filename} is \code{NA}, a list-style representation of the
  documentation shell is created and returned.  Writing the shell to a
  file amounts to \code{cat(unlist(x), file = filename, sep = "\\n")},
  where \code{x} is the list-style representation.

  When \code{prompt} is used in \code{\link{for}} loops or scripts, the
  explicit \code{name} specification will be useful.
}
\note{
  The documentation file produced by \code{prompt.data.frame} does not
  have the same format as many of the data frame documentation files in
  the \pkg{base} package.  We are trying to settle on a preferred
  format for the documentation.
}
\section{Warning}{
  The default filename may not be a valid filename under limited file
  systems (e.g. those on Windows).

  Currently, calling \code{prompt} on a non-function object assumes that
  the object is in fact a data set and hence documents it as such.  This
  may change in future versions of \R.  Use \code{\link{promptData}} to
  create documentation skeletons for data sets.
}
\seealso{
  \code{\link{promptData}}, \code{\link{help}} and the chapter on
  \dQuote{Writing \R documentation} in \dQuote{Writing \R Extensions}
  (see the \file{doc/manual} subdirectory of the \R source tree).

  For creation of many help pages (for a package),
  see \code{\link{package.skeleton}}.

  To prompt the user for input, see \code{\link{readline}}.
}
\author{Douglas Bates for \code{prompt.data.frame}}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\examples{
require(graphics)
prompt(plot.default)
prompt(interactive, force.function = TRUE)
unlink("plot.default.Rd")
unlink("interactive.Rd")

prompt(women) # data.frame
unlink("women.Rd")

prompt(sunspots) # non-data.frame data
unlink("sunspots.Rd")
}
\keyword{documentation}
