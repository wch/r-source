\name{MethodWithNext-class}
\docType{class}
\alias{MethodWithNext-class}
\title{Class MethodWithNext, ~~class for ... ~~ }
\description{  ~~ A concise (1-5 lines) description of what the class is  ~~}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("MethodWithNext", ...)}.
~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{.Data}:}{Object of class \code{"function"}; the actual
      function definition.}
    \item{\code{nextMethod}:}{Object of class \code{"PossibleMethod"}
      the method to use in response to a \code{\link{callNextMethod}()} call. }
    \item{\code{excluded}:}{Object of class \code{"list"}; one or more
      signatures excluded in finding the next method. }
    \item{\code{target}:}{Object of class \code{"signature", from class "MethodDefinition"} }
    \item{\code{defined}:}{Object of class \code{"signature", from class "MethodDefinition"} }
  }
}
\section{Extends}{
Class \code{"MethodDefinition"}, directly.
Class \code{"function"}, from data part.
Class \code{"PossibleMethod"}, by class "MethodDefinition".
Class \code{"OptionalMethods"}, by class "MethodDefinition".
}
\section{Methods}{
  \describe{
    \item{findNextMethod}{\code{signature(method = "MethodWithNext")}:
      used internally by method dispatch. }
    \item{loadMethod}{\code{signature(method = "MethodWithNext")}: used
      internally by method dispatch. }
    \item{show}{\code{signature(object = "MethodWithNext")} }
  }
}

\seealso{  \code{\link{callNextMethod}}, and 
 \code{\link{MethodDefinition-class}}. }

\keyword{classes}
