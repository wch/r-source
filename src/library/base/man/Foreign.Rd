\name{Foreign}
\title{Foreign Function Interface}
\usage{
      .C(name, \dots, NAOK=FALSE, DUP=TRUE)
.Fortran(name, \dots, NAOK=FALSE, DUP=TRUE)
.External(name, \dots)
.Call(name, \dots)
}
\alias{.C}
\alias{.Fortran}
\alias{.External}
\alias{.Call}
\arguments{
\item{name}{a character string giving the name of a C function or
  Fortran subroutine.}
\item{\dots}{arguments to be passed to the foreign function.}
\item{NAOK}{if \code{TRUE} then any \code{NA} or \code{NaN} or
  \code{Inf} values in the arguments are passed on to the foreign function.
  If \code{FALSE}, the presence of \code{NA}  or \code{NaN} or
  \code{Inf} values is regarded as an error.}
\item{DUP}{if \code{TRUE} then arguments are ``duplicated'' before their
  address is passed to C or Fortran.}
}
\description{
The functions \code{.C} and \code{.Fortran} can be used to
make calls to C and Fortran code.

\code{.External} can be used to call
compiled code that uses \R objects in the same way as internal \R
functions. There is no documentation to help you write this sort of code.

\code{.Call} can be used call compiled code which makes use of internal
\R objects.  The arguments are passed to the C code as a sequence of
\R objects.  It is included to provide compatibility with S version 4.
}
\value{
The functions \code{.C} and \code{.Fortran} return a list similar to the \code{\dots}
list of arguments passed in, but reflecting any
changes made by the C or Fortran code.

\code{.External} returns an \R object.

These calls are typically made in conjunction with
\code{\link{dyn.load}} which links DLLs to \R.
}
%%-- This note based on BDR's understanding
%% edited TSL
\section{Argument types}{
    The mapping of the types of \R arguments to C or Fortran arguments
    in \code{.C} or \code{.Fortran} is
    \tabular{lll}{
\    R \tab     C \tab     Fortran\cr
integer \tab int * \tab integer\cr
real \tab double * \tab double precision\cr
complex \tab complex * \tab double complex\cr
logical \tab int * \tab integer \cr
character \tab char ** \tab [compiler dependent]\cr
list \tab void * (SEXP *)\tab \cr
other \tab void * (SEXP)\tab \cr
}
    The C type \code{complex} is defined in \file{Complex.h} as a
    \code{typedef struct {double r; double i;}}. Fortran type
    \code{double complex} is an extension to the Fortran standard, and
    the availibility of a mapping of \code{complex} to Fortran may be
    compiler dependent.

    \emph{Note:} The C types corresponding to \code{integer} and
    \code{logical} are \code{int}, not \code{long} as in S.

    Character strings are passed as C arrays of character strings to
    Fortran: the first string may be usable if its length is passed
    separately.

    Lists, functions, expressions, environments and other language
elements are passed as generic pointers (\code{void *}). R functions can
be invoked using \code{call_S} or \code{call_R} and can be passed
lists or the simple types as arguments.
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

Also note that with \code{DUP=FALSE}, lists are passed in the internal
R format \code{(SEXP)} like any other non-atomic object. The accessor
macros in \code{Rinternals.h} are needed to get at the list elements.
}

\section{Header files for external code}{
    Writing code for use with \code{.External} will typically use
    internal \R structures. If possible use just those in
    \file{Rinternals.h}, as other header files are not installed and are
    even more likely to be changed.
}
    
\seealso{\code{\link{dyn.load}}.}
\keyword{programming}
