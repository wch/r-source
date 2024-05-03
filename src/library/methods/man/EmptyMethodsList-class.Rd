% File src/library/methods/man/EmptyMethodsList-class.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2015 R Core Team
% Distributed under GPL 2 or later
\name{EmptyMethodsList-class}
\docType{class}
\title{S4 Classes Used Internally During Method Selection}
\alias{EmptyMethodsList-class}
\alias{optionalMethod-class}
\alias{PossibleMethod-class}
\alias{standardGeneric-class}
\alias{standardGenericWithTrace-class}
\alias{nonstandardGeneric-class}
\alias{nonstandardGenericFunction-class}
\alias{nonstandardGroupGenericFunction-class}
\alias{OptionalFunction-class}
\alias{derivedDefaultMethod-class}
\alias{internalDispatchMethod-class}
\description{
  Objects from class \code{"EmptyMethodsList"} are generated during
  method selection to indicate failed search (forcing backtracking).
  Other classes described here are used internally in method dispatch.
  All these are for internal use.}
\usage{
## class described below
"EmptyMethodsList" # deprecated, since \R 3.2.0; rather "defunct" from 4.5.0

### Other, virtual classes used in method dispatch
"standardGeneric"
"standardGenericWithTrace"
"nonstandardGeneric"
"nonstandardGenericFunction"
"nonstandardGroupGenericFunction"
"OptionalFunction"
"optionalMethod"
"PossibleMethod"
"derivedDefaultMethod"
"internalDispatchMethod"
}
\section{Extends / Slots}{
  \code{"EmptyMethodsList"}:
  \describe{
    \item{\code{argument}:}{Object of class \code{"name"} the argument
      names being selected on.}
    \item{\code{sublist}:}{Object of class \code{"list"} (unused, and
      perhaps to be dropped in a later version.) }
  }

  \code{"optionalMethod"} and
  \code{"PossibleMethod"} have \emph{no} slots and extend S4 class \code{"\linkS4class{function}"}.
}

\section{Methods}{
  No methods defined with class \code{"EmptyMethodsList"} in the signature.
}
\seealso{
  Function \code{\link{MethodsListSelect}} (deprecated since \R version
  3.2.0) uses the objects;
  class \code{\linkS4class{MethodsList}} is formally defunct since \R 3.2.0.
}
\keyword{classes}
\keyword{internal}
