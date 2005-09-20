\name{TraceClasses}
\docType{class}
\alias{traceable-class}
\alias{functionWithTrace-class}
\alias{MethodDefinitionWithTrace-class}
\alias{MethodWithNextWithTrace-class}
\alias{genericFunctionWithTrace-class}
\alias{groupGenericFunctionWithTrace-class}
%% private
\alias{.untracedFunction}
\alias{.makeTracedFunction}
\alias{.doTracePrint}
\alias{.doTrace}
\alias{.InitTraceFunctions}
\title{Classes Used Internally to Control Tracing }
\description{ The classes described  here are used by the R function
  \code{\link{trace}} to create versions of functions and methods
  including browser calls, etc., and also to \code{\link{untrace}} the
  same objects.}
\section{Objects from the Class}{
  Objects will be created from these classes by calls to \code{trace}.
  (There is an \code{\link{initialize}} method for class
  \code{"traceable"}, but you are unlikely to need it directly.)
}
\usage{
### Objects from the following classes are generated
### by calling trace() on an object from the corresponding
### class without the "WithTrace" in the name.

"functionWithTrace"
"MethodDefinitionWithTrace"
"MethodWithNextWithTrace"
"genericFunctionWithTrace"
"groupGenericFunctionWithTrace"

### the following is a virtual class extended by each of the
### classes above

"traceable"
}
\section{Slots}{
  \describe{
    \item{\code{.Data}:}{The data part, which will be \code{"function"}
      for class \code{"functionWithTrace"}, and similarly for the other
      classes.}
    \item{\code{original}:}{Object of the original class; e.g.,
      \code{"function"} for class \code{"functionWithTrace"}. }
  }
}
\section{Extends}{
  Each of the classes extends the corresponding untraced class, from the
  data part; e.g., \code{"functionWithTrace"} extends \code{"function"}.
  Each of the specific classes extends \code{"traceable"}, directly,
  and class \code{"VIRTUAL"}, by class \code{"traceable"}.
}
\section{Methods}{
  The point of the specific classes is that objects generated from them,
  by function \code{trace()}, remain callable or dispatchable, in
  addition to their new trace information.
}
\seealso{ function \code{\link{trace}} }
\keyword{classes}
