\name{libPaths}
\title{Search Paths for Packages}
\alias{.Library}
\alias{.libPaths}
\description{
  \code{.libPaths} gets/sets the library trees within which packages are
  looked for.
}
\usage{
.libPaths(new)

.Library
}
\arguments{
  \item{new}{a character vector with the locations of \R library trees.}
}
\details{
  \code{.Library} is a character string giving the location of the
  default library, the \file{library} subdirectory of \env{R\_HOME}.

  \code{.libPaths} is used for getting or setting the library trees that
  \R knows about (and hence uses when looking for packages).  If called
  with argument \code{new}, the library search path is set to
  the existing files in \code{unique(new, .Library)} and this is
  returned.  If given no argument, a character vector with the currently
  known library trees is returned.

#ifdef unix
  The library search path is initialized at startup from the environment
  variable \env{R\_LIBS} (which should be a colon-separated list of
  directories at which \R library trees are rooted) by calling
  \code{.libPaths} with the directories specified in \env{R\_LIBS}.
#endif
#ifdef windows
  The library search path is initialized at startup from the environment
  variable \env{R\_LIBS} (which should be a semicolon-separated list of
  directories at which \R library trees are rooted) by calling
  \code{.libPaths} with the directories specified in \env{R\_LIBS}.
#endif
}
\value{
  A character vector of file paths.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\seealso{
  \code{\link{library}}
}
\examples{
.libPaths()                 # all library trees R knows about
}
\keyword{data}
