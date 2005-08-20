\name{file.path}
\alias{file.path}
\title{Construct Path to File}
\description{
  Construct the path to a file from components in a platform-independent
  way.
}
\usage{
file.path(\dots, fsep = .Platform$file.sep)
}
\arguments{
  \item{\dots}{character vectors.}
  \item{fsep}{the path separator to use.}
}
\value{
  A character vector of the arguments concatenated term-by-term and
  separated by \code{fsep} if all arguments have positive length;
  otherwise, an empty character vector.
}
#ifdef windows
\note{
  The components are separated by \code{/} (not \code{\}) on Windows.
}
#endif
\keyword{file}
