\name{checkFF}
\alias{checkFF}
\title{Check Foreign Function Calls}
\usage{
checkFF(package, dir, file, lib.loc = NULL,
        verbose = getOption("verbose"))
}
\description{
  Performs checks on calls to compiled code from R code.  Currently only
  whether the interface functions such as \code{.C} and \code{.Fortran}
  are called with argument \code{PACKAGE} specified, which is highly
  recommended to avoid name clashes in foreign function calls.
}
\arguments{
  \item{package}{a character string naming an installed package.  If
    given, the installed R code of the package is checked.}
  \item{dir}{a character string specifying the path to a package's root
    source directory.  This should contain the subdirectory \code{R}
    (for R code).  Only used if \code{package} is not given.}
  \item{file}{the name of a file containing R code to be checked.  Used
    if neither \code{package} nor \code{dir} are given.}
  \item{lib.loc}{a character vector of directory names of \R libraries,
    or \code{NULL}.  The default value of \code{NULL} corresponds to all
    libraries currently known.  The specified library trees are used to
    to search for \code{package}.}
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
