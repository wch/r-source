\name{checkFF}
\alias{checkFF}
\title{Check Foreign Function Calls}
\usage{
checkFF(file, package, lib.loc = .lib.loc)
}
\description{
  Performs checks on calls to compiled code from R code.  Currently only
  checks whether \code{.C} and \code{.Fortran} calls specify argument
  \code{PACKAGE}, which is highly recommended to avoid name clashes in
  foreign function calls.
}
\arguments{
  \item{file}{the name of a file containing R code to be checked.}
  \item{package}{a character string naming an installed package.  If
    given and \code{file} is not given, the installed R code of the
    package is checked.}
  \item{lib.loc}{a character vector describing the location of \R
    library trees to search for \code{package}.}
}
\seealso{
  \code{\link{.C}},
  \code{\link{.Fortran}}.
}
\examples{
checkFF(package = "ts")
}
\keyword{programming}
\keyword{utilities}
