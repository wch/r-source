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
\description{
The functions \code{.C} and \code{.Fortran} can be used to
make calls to C and Fortran code.
}
\value{
The functions return a list similar to the \code{\dots}
list of arguments passed in, but reflecting any
changes made by the C or Fortran code.

These calls are typically made in conjunction with
\code{\link{dyn.load}} which links DLLs to \R.
}
%%-- This note by Thomas Lumley, (minimally edited by MM):
\note{\emph{\code{DUP=FALSE} is dangerous.}
    
There are two important dangers with \code{DUP=FALSE}. The first is that
garbage collection may move the object, resulting in the pointers
pointing nowhere useful and causing hard-to-reproduce bugs.

The second is that if you pass a formal parameter of the calling
function to \code{.C}/\code{.Fortran} with \code{DUP=FALSE}, it may not
necessarily be copied.  You may be able to change not only the local
variable but the variable one level up.  This will also be very hard to
trace.

1.  If your C/Fortran routine calls back any \R function including
\code{S_alloc}/\code{R_alloc} then do not use \code{DUP=FALSE}.  Do not
even think about it.  Calling almost any \R function could trigger
garbage collection.

2.  If you don't trigger garbage collection it is safe and useful to set
\code{DUP=FALSE} if you don't change any of the variables that might be
affected, e.g.,

\code{.C("Cfunction", input=x, output=numeric(10))}.

In this case the output variable didn't exist before the call so it can't
cause trouble. If the input variable is not changed in \code{Cfunction} you are
safe.
}
\seealso{\code{\link{dyn.load}}.}
\keyword{programming}
