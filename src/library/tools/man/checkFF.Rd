\name{checkFF}
\alias{checkFF}
\alias{print.checkFF}
\title{Check Foreign Function Calls}
\usage{
checkFF(package, dir, file, lib.loc = NULL,
        verbose = getOption("verbose"))
}
\description{
  Performs checks on calls to compiled code from R code.  Currently only
  whether the interface functions such as \code{.C} and \code{.Fortran}
  are called with a \code{"\link{NativeSymbolInfo}"} first argument or
  with argument \code{PACKAGE} specified, which is highly
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
    printed (and the result is returned invisibly).}
}
\details{
  Note that we can only check if the \code{name} argument is a symbol or
  a character string, not what class of object the symbol resolves to
  at run-time.
  
  \emph{If} the package has a namespace and \emph{if} that contains a
  \code{useDynLib} directive, calls in top-level functions in the
  package are not reported as their symbols will be preferentially
  looked up in the DLL named in the first \code{useDynLib} directive.
}
\value{
  An object of class \code{"checkFF"}, which currently is a list of the
  (parsed) foreign function calls with a character first argument and
  no \code{PACKAGE} argument.

  There is a \code{print} method to display the information contained in
  such objects.
}
\seealso{
  \code{\link{.C}},
  \code{\link{.Fortran}};
  \code{\link{Foreign}}.
}
\section{Warning}{
  This function is still experimental.  Both name and interface might
  change in future versions.
}
\examples{
checkFF(package = "stats", verbose = TRUE)
}
\keyword{programming}
\keyword{utilities}
