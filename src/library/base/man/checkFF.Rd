\name{checkFF}
\alias{checkFF}
\title{Check Foreign Function Calls}
\usage{
checkFF(file, package, lib.loc = .lib.loc,
        verbose = getOption("verbose"))
}
\description{
  Performs checks on calls to compiled code from R code.  Currently only
  whether the interface functions such as \code{.C} and \code{.Fortran}
  are called with argument \code{PACKAGE} specified, which is highly
  recommended to avoid name clashes in foreign function calls.
}
\arguments{
  \item{file}{the name of a file containing R code to be checked.}
  \item{package}{a character string naming an installed package.  If
    given and \code{file} is not given, the installed R code of the
    package is checked.}
  \item{lib.loc}{a character vector describing the location of \R
    library trees to search for \code{package}.}
  \item{verbose}{a logical.  If \code{TRUE}, additional diagnostics are
    printed.}
}
\seealso{
  \code{\link{.C}},
  \code{\link{.Fortran}};
  \code{\link{Foreign}}.
}
\examples{
checkFF(package = "ts", verbose = TRUE)
}
\keyword{programming}
\keyword{utilities}
