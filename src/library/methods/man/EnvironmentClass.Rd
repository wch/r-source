\name{environment-class}
\docType{class}
\alias{environment-class}
\title{Class "environment" }
\description{ A formal class for R environments.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("environment", ...)}.
The arguments in \dots, if any, should be named and will be assigned to
the newly created environment.
}


\section{Methods}{
  \describe{
    \item{coerce}{\code{signature(from = "ANY", to = "environment")}:
      calls \code{\link{as.environment}}. }
    \item{initialize}{\code{signature(object = "environment")}:
      Implements the assignments in the new environment.  Note that the
      \code{object} argument is ignored; a new environment is
      \emph{always} created, since environments are not protected by copying. }
  }
}
\seealso{ \code{\link{new.env}} }

\keyword{classes}
