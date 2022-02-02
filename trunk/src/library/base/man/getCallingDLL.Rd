% File src/library/base/man/getCallingDLL.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2008 R Core Team
% Distributed under GPL 2 or later

\name{getCallingDLL}
\alias{getCallingDLL}
\alias{getCallingDLLe}
\title{Compute DLL for Native Interface Call}
\description{
  This is an internal function that is called from \R's C code to
  determine the enclosing namespace of a
  \code{.C}/\code{.Call}/\code{.Fortran}/\code{.External} call which has
  no \code{PACKAGE} argument. If the call has been made from a function
  within a namespace, then we can find the DLL associated with that
  namespace.  The purpose of this is to avoid having to use the
  \code{PACKAGE} argument in these native calls and so better support
  versions of packages.

  This is an internal function that may be migrated to internal C
  code in the future and so should not be used by \R programmers.
}
\usage{
getCallingDLL(f = sys.function(-1), doStop = FALSE)
getCallingDLLe(e)
}
\arguments{
  \item{f}{the function whose namespace and DLL are to be found.
    By default, this is the current function being called which
    is the one in which the native routine is being invoked.}
  \item{doStop}{a logical value indicating whether failure to find
    a namespace and/or DLL is an error (\code{TRUE}) or not
    (\code{FALSE}).  The default is \code{FALSE} so that when this is
    called because there is no \code{PACKAGE} argument in a
    \code{\link{.C}}, \code{\link{.Call}}, \code{\link{.Fortran}},
    \code{\link{.External}} call,  no error occurs and the regular lookup
    is performed by searching all DLLs in order.}
  \item{e}{an environment.}
}

\seealso{
\code{\link{.C}},
\code{\link{.Call}},
\code{\link{.Fortran}},
\code{\link{.External}}
}
\examples{
if(exists("ansari.test"))
   getCallingDLL(ansari.test)
}
\keyword{internal}
