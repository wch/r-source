\name{ObjectsWithPackage-class}
\docType{class}
\alias{ObjectsWithPackage-class}
\title{A Vector of Object Names, with associated Package Names }
\description{
  This class of objects is used to represent ordinary character string
  object names, extended with a \code{package} slot naming the package
  associated with each object.
}
\section{Objects from the Class}{
  The function \code{\link{getGenerics}} returns an object of this class.
}
\section{Slots}{
  \describe{
    \item{\code{.Data}:}{Object of class \code{"character"}: the
      object names.}
    \item{\code{package}:}{Object of class \code{"character"} the
      package names.}
  }
}
\section{Extends}{
  Class \code{"character"}, from data part.\cr
  Class \code{"vector"}, by class \code{"character"}.
}
\seealso{ \code{Methods} for general background. }
\keyword{classes}
