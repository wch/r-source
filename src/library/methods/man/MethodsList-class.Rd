\name{MethodsList-class}
\alias{MethodsList-class}
\alias{body<-,MethodDefinition-method}
\docType{class}
\title{Class MethodsList, Representation of Methods for a Generic Function }
\description{ Objects from this class are generated and revised by the
  definition of methods for a generic function.}
\section{Slots}{\describe{

  \item{\code{argument}:}{Object of class \code{"name"}.  The name of the
    argument being used for dispatch at this level. }
  \item{\code{methods}:}{A named list of the methods (and method lists)
    defined \emph{explicitly} for this argument, with the names being
    the classes for which the methods have been defined. }
  \item{\code{allMethods}:}{A named list, which may be empty if this
    object has not been used in dispatch yet.  Otherwise, it contains
    all the directly defined methods from the \code{methods} slot, plus
    any inherited methods. }
}}


\section{Extends}{
Class \code{"OptionalMethods"}, directly.
}
\keyword{classes}
\keyword{methods}
