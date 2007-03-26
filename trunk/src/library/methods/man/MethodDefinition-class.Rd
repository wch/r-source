\name{MethodDefinition-class}
\docType{class}
\alias{MethodDefinition-class}
\alias{SealedMethodDefinition-class}
\title{Classes to Represent Method Definitions}
\description{
  These classes extend the basic class \code{"function"} when
  functions are to be stored and used as method definitions.
}
\section{Objects from the Class}{
  The action of setting a method by a call to \code{\link{setMethod}} creates an object of this class.  It's
  unwise to create them directly.

  The class \code{"SealedMethodDefinition"} is created by a call to
  \code{\link{setMethod}} with argument \code{sealed = TRUE}.  It has
  the same representation as \code{"MethodDefinition"}.
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
      \item{\code{generic}:}{Object of class \code{"character"}; the function
        for which the method was created. }  
  }
}
\section{Extends}{
  Class \code{"function"}, from data part.\cr
  Class \code{"PossibleMethod"}, directly.\cr
  Class \code{"OptionalMethods"}, by class \code{"function"}.
}
\details{
  Method definition objects are functions with additional information
  defining how the function is being used as a method.  The
  \code{target} slot is the class signature for which the method will
  be dispatched, and the \code{defined} slot the signature for which
  the method was originally specified (that is, the one that appeared
  in some call to \code{\link{setMethod}}).
}
\seealso{class \code{\link{MethodsList-class}} for the objects
  defining sets of methods associated with a particular generic
  function.  The individual method definitions stored in these objects
  are from class \code{MethodDefinition}, or an extension.
  \code{\link{MethodWithNext-class}} for an extension used by
  \code{\link{callNextMethod}}.
}
\keyword{classes}
