\name{Foreign}
\title{Foreign Function Interface}
\usage{
      .C(name, \dots, NAOK=FALSE, DUP=TRUE)
.Fortran(name, \dots, NAOK=FALSE, DUP=TRUE)
}
\alias{.C}
\alias{.Fortran}
\arguments{
\item{name}{a character string giving the name of a C function or
  Fortran subroutine.}
\item{\dots}{arguments to be passed to the foreign function.}
\item{NAOK}{if \code{TRUE} then any \code{NA} values in the arguments
  are passed on to the foreign function.
  If \code{FALSE}, the presence of \code{NA} values is regarded as an error.}
\item{DUP}{if \code{TRUE} then arguments are ``duplicated'' before their
  address is passed to C or Fortran.}
}
\value{
The functions \code{.C} and \code{.Fortran} can be used to
make calls to C and Fortran code.
The functions return a list similar to the \code{\dots}
list of arguments passed in, but reflecting any
changes made by the C or Fortran code.

These calls are typically made in conjunction with
\code{\link{dyn.load}} which links DLLs to \R.
}
\seealso{
\code{\link{dyn.load}}.
}
\keyword{programming}
