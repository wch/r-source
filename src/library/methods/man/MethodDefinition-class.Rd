\name{MethodDefinition-class}
\docType{class}
\alias{MethodDefinition-class}
\title{Classes to Represent Method Definitions }
\description{These classes extend the basic class \code{"function"} when
functions are to be stored and used as method definitions.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("MethodDefinition", ...)}.
~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{.Data}:}{Object of class \code{"function"}; the data
        part of the definition. }
    \item{\code{target}:}{Object of class \code{"signature"}; the
        signature for which the method was wanted. }
    \item{\code{defined}:}{Object of class \code{"signature"}; the
        signature for which a method was found.  If the method was
        inherited, this will not be identical to \code{target}. }
  }
}
\section{Extends}{
Class \code{"function"}, from data part.
Class \code{"PossibleMethod"}, directly.
Class \code{"OptionalMethods"}, by class "function".
}
\section{Methods}{
  \describe{
    \item{findNextMethod}{\code{signature(method = "MethodDefinition")}: ... }
    \item{loadMethod}{\code{signature(method = "MethodDefinition")}: ... }
    \item{show}{\code{signature(object = "MethodDefinition")}: ... }
  }
}
\seealso{ See 
 \code{\link{MethodWithNext-class}} for an extension used by \code{\link{callNextMethod}}. }

\keyword{classes}
