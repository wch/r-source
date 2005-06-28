\name{promptPackage}
\alias{promptPackage}
\title{Generate a Shell for Documentation of a Package}
\description{
  Generates a shell of documentation for an installed or source package.
}
\usage{
promptPackage(package, lib.loc = NULL, filename = NULL, name = NULL,
              final = FALSE)
}
\arguments{
  \item{package}{the name of an \emph{installed} or \emph{source} package
    to be documented.}
  \item{lib.loc}{a character vector describing the location of \R
    library trees to search through, or \code{NULL}.  The default value
    of \code{NULL} corresponds to all libraries currently known.  For a
    source package this should specify the parent directory of the
    package's sources.}
  \item{filename}{usually, a connection or a character string giving the
    name of the file to which the documentation shell should be written.
    The default corresponds to a file whose name is \code{name} followed
    by \code{".Rd"}.  Can also be \code{NA} (see below).}
  \item{name}{a character string specifying the name of the help topic,
    typically of the form \code{<pkg>-package}.}
  \item{final}{a logical value indicating whether to attempt to
    create a usable version of the help topic, rather than just a shell.}
}
\value{
  If \code{filename} is \code{NA}, a list-style representation of the
  documentation shell.  Otherwise, the name of the file written to is
  returned invisibly.
}
\details{
  Unless \code{filename} is \code{NA}, a documentation shell for
  \code{package} is written to the file specified by \code{filename}, and
  a message about this is given.  

  If \code{filename} is \code{NA}, a list-style representation of the
  documentation shell is created and returned.  Writing the shell to a
  file amounts to \code{cat(unlist(x), file = filename, sep = "\\n")},
  where \code{x} is the list-style representation.
  
  If \code{final} is \code{TRUE}, the generated documentation will not
  include the place-holder slots for manual editing, it will be usable
  as-is.  In most cases a manually edited file is preferable (but
  \code{final = TRUE} is certainly less work).
}
\seealso{\code{\link{prompt}}}
\examples{
filename <- tempfile()
promptPackage("utils", file=filename)
file.show(filename)
unlink(filename)
}
\keyword{documentation}
