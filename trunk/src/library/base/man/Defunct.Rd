\name{Defunct}
\title{Marking Objects as Defunct}
\alias{Defunct}
\alias{.Defunct}

\description{
  When a function is removed from \R it should be replaced by a function
  which calls \code{.Defunct}.
}
\usage{
.Defunct(new, package = NULL, msg)
}
\arguments{
  \item{new}{
    character string: A suggestion for a replacement function.
  }
  \item{package}{
    character string: The package to be used when suggesting where the
    defunct function might be listed.
  }
  \item{msg}{
    character string: A message to be printed, if missing a default
    message is used.
  }
}
\details{
  \code{.Defunct} is called from defunct functions. Functions should be
  listed in \code{help("pkg-defunct")} for an appropriate \code{pkg},
  including \code{base}.
}
\seealso{
  \code{\link{Deprecated}}.

  \code{base-defunct} and so on which list the defunct functions
  in the packages.
}
\keyword{documentation}
\keyword{utilities}
