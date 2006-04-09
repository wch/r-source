\name{signature-class}
\docType{class}
\alias{signature-class}
\alias{show,signature-method}
\title{Class "signature" For Method Definitions}
\description{This class represents the mapping of some of the formal
  arguments of a function onto the names of some classes.  It is used as
  one of two slots in the \code{\link{MethodDefinition-class}}.
}
\section{Objects from the Class}{
  Objects can be created by calls of the form \code{new("signature",
    functionDef, ...)}.  The \code{functionDef} argument, if it is
  supplied as a function object, defines the formal names.  The other
  arguments define the classes.
}
\section{Slots}{
  \describe{
    \item{\code{.Data}:}{Object of class \code{"character"} the classes. }
    \item{\code{names}:}{Object of class \code{"character"} the
      corresponding argument names. }
  }
}
\section{Extends}{
Class \code{"character"}, from data part.
Class \code{"vector"}, by class "character".
}
\section{Methods}{
  \describe{
    \item{initialize}{\code{signature(object = "signature")}: see the
      discussion of objects from the class, above. }
  }
}
\seealso{
 \code{\link{MethodDefinition-class}} for the use of this class }

\keyword{classes}
