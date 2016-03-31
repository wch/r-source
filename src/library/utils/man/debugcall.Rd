% File src/library/base/man/debugcall.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2016 R Core Team
% Distributed under GPL 2 or later

\name{debugcall}
\title{Debug a Call}
\alias{debugcall}
\alias{undebugcall}
\usage{
debugcall(call, once = FALSE)
undebugcall(call)
}
\arguments{
  \item{call}{An R expression calling a function. The called function
    will be debugged. See Details.}
  \item{once}{logical; if \code{TRUE}, debugging only occurs once, as via
    \code{debugonce}. Defaults to \code{FALSE}}
}
\description{
  Set or unset debugging flags based on a call to a function. Takes into
  account S3/S4 method dispatch based on the classes of the arguments in
  the call.
}
\details{
  \code{debugcall} debugs the non-generic function, S3 method or S4
  method that would be called by evaluating \code{call}. Thus, the user
  does not need to specify the signature when debugging
  methods. Although the call is actually to the generic, it is the
  method that is debugged, not the generic, except for non-standard S3
  generics (see \code{\link{isS3stdGeneric}}).
}
\value{
  \code{debugcall} invisibly returns the debugged call expression.
}
\note{
  Non-standard evaluation is used to retrieve the \code{call} (via
  \code{\link{substitute}}). For this reason, passing a variable
  containing a call expression, rather than the call expression itself,
  will not work.
}
\seealso{
  \code{\link{debug}} for the primary debugging interface
}

\examples{
\dontrun{
## Evaluate call after setting debugging
## 
f <- factor(1:10)
res <- eval(debugcall(summary(f))) 
}
}
\keyword{programming}
\keyword{environment}
\keyword{utilities}
