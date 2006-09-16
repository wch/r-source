\name{methods-package}
\alias{methods-package}
\docType{package}
\title{
Formal Methods and Classes
}
\description{
Formally defined methods and classes for R objects, plus
other programming tools, as described in the references.
}
\details{
This package provides the \dQuote{S4} or  \dQuote{S version 4} 
approach to methods and classes in a functional language.

For a complete
list of functions and classes, use \code{library(help="methods")}.
}

\references{
 The R package \pkg{methods} implements, with a few exceptions, the
  programming interface for classes and methods in the book
  \emph{Programming with Data} (John M. Chambers, Springer, 1998), in
  particular sections 1.6, 2.7, 2.8, and chapters 7 and 8.

  While the programming interface for the methods package follows the
  reference, the R software is an original implementation, so details in
  the reference that reflect the S4 implementation may appear
  differently in R.  Also, there are extensions to the programming
  interface developed more recently than the reference.  For a
  discussion of details see \code{?\link{Methods}}
  and the links from that documentation.
}
\author{
R Development Core Team

Maintainer: R Core Team \email{R-core@r-project.org}
}
\keyword{ package }
\keyword{ methods }
