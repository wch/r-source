\name{base-internal}
\alias{.getRequiredPackages}
\title{Internal Functions in the Base Package}
\description{
  Internal functions in the base package, which are only user-visible
  because of the special nature of the base namespace.
}
\usage{
.getRequiredPackages(file = "DESCRIPTION", quietly = FALSE)
}
\arguments{
  \item{file}{The full path to the \file{DESCRIPTION} file of a package.}
  \item{quietly}{logical: should progress messages be given.}
}
\details{
  \code{.getRequiredPackages} attaches all the packages mentioned in the
  \code{Depends} field: failure to find a package is an error.
}
\value{
  \code{.getRequiredPackages} returns \code{invisible()}.
}
\keyword{internal}
