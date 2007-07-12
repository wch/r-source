\name{path.expand}
\alias{path.expand}
\title{Expand File Paths}
\description{
  Expand a path name, for example by replacing a leading tilde by the
  user's home directory (if defined on that platform).
}
\usage{
path.expand(path)
}
\arguments{
  \item{path}{character vector containing one or more path names.}
}
#ifdef unix
\details{
  On \emph{some} Unix versions, a leading \code{~user} will expand to
  the home directory of \code{user}, but not on Unix versions without
  \code{readline} installed, nor if \R is invoked with
  \option{--no-readline}.
}
#endif
#ifdef windows
\details{
  The definition of \sQuote{home} is in the \file{rw-FAQ} Q2.14: it is
  taken from the \env{R\_USER} environment variable when
  \code{path.expand} is first called.
}
#endif
\seealso{
  \code{\link{basename}}
}
\examples{
path.expand("~/foo")
}
\keyword{file}
