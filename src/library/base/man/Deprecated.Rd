\name{Deprecated}
\title{Marking Objects as Deprecated}
\alias{Deprecated}
\alias{.Deprecated}

\description{
  When an object is about removed from \R it is first deprecated and
  should include a call to \code{.Deprecated}.
}
\usage{
.Deprecated(new, package=NULL, msg)
}
\arguments{
  \item{new}{
    character string: A suggestion for a replacement function.
  }
  \item{package}{
    character string: The package to be used when suggesting where the
    deprecated function might be listed.
  }
  \item{msg}{
    character string: A message to be printed, if missing a default
    message is used.
 }
}
\details{
  \code{.Deprecated("<new name>")} is called from deprecated
  functions.  The original help page for these functions is often
  available at \code{help("oldName-deprecated")} (note the quotes).
  Functions should be listed in \code{help("pkg-deprecated")} for an
  appropriate \code{pkg}, including \code{base}. 
}
\seealso{
  \code{\link{Defunct}}

  \code{\link{base-deprecated}} and so on which list the deprecated functions
  in the packages.
}
\keyword{documentation}
\keyword{utilities}
